import System.IO(hIsEOF, stdin)
import Data.List(transpose)

data Table   = Table String [Hotkey]
type Key     = String
type Command = String
type Help    = String
data Hotkey  = Hotkey (Key, Command, Help)

instance (Show Table) where
    show (Table h hotkeys) = show h ++ show' hotkeys
        where show' []                        = ""
              show' (hk@(Hotkey (k,c,h)):hks) = "\n" ++ show (k,c,h) ++ show' hks


main :: IO ()
main = do
    ls <- getInput []
    --print $ reverse ls
    let str = map (\x -> prettify x 149 0.2 0.4) (parse ls)
    putStrLn (unlines str)
    
getInput :: [String] -> IO [String]
getInput ls = do
    iseof <- hIsEOF stdin
    if iseof
       then return ls
       else do
           l <- getLine
           getInput (l:ls)

parse :: [String] -> [Table]
parse ls = parse' . map (unwords . words) . filter (/= "") . reverse $ ls
    where parse' [] = []
          parse' lines' =
              let (tableHead, remains) = getTableHead lines'
                  (table, remains')    = breakTable remains
               in case parseTable tableHead table of
                    Just t@(Table _ _) -> t : parse' remains'
                    Nothing               -> parse' remains'

getTableHead ls@(l1:l2:l3:ls')
  | isTableHead l1 l2 l3 = (unwords . words . tail $ l2, ls')
  | otherwise            = ([], ls')
getTableHead ls          = ([], ls)

parseTable :: String -> [String] -> Maybe Table
parseTable [] []    = Nothing
parseTable h  table = Just $ Table h (parseHotkey table)

parseHotkey :: [String] -> [Hotkey]
parseHotkey []     = []
parseHotkey (t:ts) =
    let (content, r) = break isComment ts
     in case content of
          []          -> Hotkey ("","", t)                : parseHotkey r
          key:[]      -> Hotkey (key,"", t)               : parseHotkey r
          key:command -> Hotkey (key, unlines command, t) : parseHotkey r

tail3 = tail . tail . tail

breakTable :: [String] -> ([String], [String])
breakTable ls = breakTable' ls []
    where breakTable' remain@(l1:l2:l3:ls) checked
            | isTableHead l1 l2 l3   = (checked, remain)
            | otherwise              = breakTable' (l2:l3:ls) (checked ++ [l1])
          breakTable' remain checked = (checked ++ remain, [])

isTableHead l1 l2 l3 = l1 == "#" && isComment l2 && l3 == "#"

isComment = (=='#') . head

prettify :: Table -> Int -> Float -> Float -> String
prettify t@(Table h hotkeys) width r1 r2 =
    "┌" ++ replicate width '─' ++ "┐\n"
    ++ unlines (map (\line -> "│" ++ alignCenter line width ++ "│") (wrapLines [h] width))
    ++ unlines hk
    ++ "└" ++ replicate w1 '─' ++ "┴" ++ replicate w2 '─' ++ "┴"
    ++ replicate w3 '─' ++ "┘"
        where w1 = round (fromIntegral width * r1) - 1
              w2 = round (fromIntegral width * r2) - 1
              w3 = width - w1 - w2 - 2
              (col1:cols) = (map (\x -> prettifyHotkey x width r1 r2) hotkeys)
              hk = map (\x -> if x == '┼'
                                 then '┬'
                                 else x) col1 : cols

--prettifyHotkey :: Hotkey -> Int -> String
prettifyHotkey hotkey@(Hotkey (k, c, h)) width r1 r2=
    "├" ++ replicate w1 '─' ++ "┼" ++ replicate w2 '─'
    ++ "┼" ++ replicate w3 '─'  ++ "┤\n"
    ++ unlines' (map (\x@[k,c,h] -> "│" ++ k ++ "│" ++ c ++ "│" ++ h ++ "│") $ transpose hk)
        where w1      = round (fromIntegral width * r1) - 1
              w2      = round (fromIntegral width * r2) - 1
              w3      = width - w1 - w2 - 2
              ws      = [w1, w2, w3]
              hk''    = zipWith wrapLines [lines k, lines c, lines h] ws
              hk'     = zipWith (\ls w' -> map (`alignLeft` w') ls) hk'' ws
              lens    = map length hk'
              max_len = maximum lens
              sp      = map (`replicate` ' ') ws
              hk      = zipWith3 (\x y z -> x ++ replicate (max_len-y) z) hk' lens sp

-- unlines but remove last \n
unlines' = init . unlines

wrapLines :: [String] -> Int -> [String]
wrapLines ls width = reverse (wrapLines' ls [])
    where wrapLines' [] ret = ret
          wrapLines' (l:ls) ret =
              if length l > width
                 then let (lhs, rhs) = splitAt width l
                       in case rhs of
                            [] -> wrapLines' ls (lhs:ret)
                            ot -> wrapLines' (ot:ls) (lhs:ret)
                 else wrapLines' ls (l:ret)

alignCenter :: String -> Int -> String
alignCenter line width =
    let len = length line
        n_space_l = (width - len) `div` 2
        n_space_r = width - len - n_space_l
     in replicate n_space_l ' ' ++ line ++ replicate n_space_r ' '
alignLeft :: String -> Int -> String
alignLeft line width =
    let len = length line
        n_space_r = width - len
     in line ++ replicate n_space_r ' '

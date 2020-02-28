import Data.List
import Control.Monad
main = do
    print "helloworld"
    print "graham scan algorithm"
    print $ mean [3, 3, 4]
    let ps = [(0,0),(1,0),(1.2,0),(1,-1),(-1,-1),(-2,0),(-1,1)]
    print $ ps 
    print $ calcDirections ps
    let p0 = findP0 ps
    putStr "p0: "
    print $ p0
    putStr "sorted points: "
    print $ do
        p0' <- p0
        sortPoints p0' ps
    putStr "angles: "
    print $ do
        (x0,y0) <- p0
        Just $ map (\(x,y) -> angle (x-x0,y-y0)) ps
    putStr "result: "
    print $ getConvexHull ps

mean :: (Fractional a, Foldable t) => t a -> a
mean xs = (sum xs) / fromIntegral (length xs)

-- graham scan algorithm
data Direction = CounterClockwise | Clockwise | Colinear deriving (Show, Eq)

ccw :: (Num a, Ord a) => [(a,a)] -> Direction
ccw [(x1,y1),(x2,y2),(x3,y3)]
  | cross_product > 0 = CounterClockwise
  | cross_product < 0 = Clockwise
  | cross_product == 0 = Colinear
  where cross_product = (x2-x1)*(y3-y1)-(y2-y1)*(x3-x1)

calcDirections (p1:p2:p3:ps) = Just $ calcDirections' (p1:p2:p3:ps)
    where calcDirections' (p1:p2:p3:[]) = ccw [p1,p2,p3]:[]
          calcDirections' (p1:p2:p3:ps) = ccw [p1,p2,p3]:calcDirections' (p2:p3:ps)
calcDirections _ = Nothing

findP0 :: (Num a, Ord a) => [(a,a)] -> Maybe (a,a)
findP0 [] = Nothing
findP0 (p:ps) = Just $ findP0' p ps
    where findP0' p [] = p
          findP0' (x,y) ((x',y'):p's)
            | y > y'    = findP0' (x',y') p's
            | y < y'    = findP0' (x,y)   p's
            | otherwise = if   x < x'
                          then findP0' (x,y)   p's
                          else findP0' (x',y') p's

sortPoints p0 [] = Nothing
sortPoints p0 (p:[]) = Just (p:[])
sortPoints (x0,y0) ps = Just $ sortBy 
    (\(x,y) (x',y') -> compare (angle (x-x0,y-y0)) (angle (x'-x0,y'-y0))) ps

angle (x,y)
  | a < 0     = a + pi
  | a > 0     = a
  | a == 0     = a
  | otherwise = -1/0
  where a = atan (y/x)

getConvexHull ps = do
    p0 <- findP0 ps
    sortedps <- sortPoints p0 ps
    return $ getCH sortedps []
        where getCH []     stack = stack
              getCH (p:ps) stack = getCH ps (p:pop_if_clockwise stack p)
              pop_if_clockwise []        _ = []
              pop_if_clockwise (s:[])    _ = s:[]
              pop_if_clockwise (h:h':hs) p
                | ccw [h',h,p] == Clockwise = pop_if_clockwise (h':hs) p
                | otherwise                = h:h':hs


-- ch04 exercises {{{
safeListFunc f [] = Nothing
safeListFunc f l  = Just $ f l

safeHead = safeListFunc head
safeTail = safeListFunc tail
safeLast = safeListFunc last
safeInit = safeListFunc init

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith pred [] = []
splitWith pred lst = let (pre,suf) = break (not . pred) lst
                      in pre : splitWith pred suf
-- }}}

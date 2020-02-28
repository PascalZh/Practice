import Control.Monad.State
import Data.Ratio
import Data.List
--main = do
    --let f = (\x -> square . square $ x)  5
    --putStrLn $ show f
    --print (CJust (*3) <*> CJust 4)
main = do
    print $ Prob [(32, 3%6)]
    let x = Prob [(32, 3%6)]
    print $ getProb x
    print $ fmap negate (Prob [(3,1%2),(5,1%4),(9,1%4)])
    print $ flipThree
    let newTree = changeToP [R,L] freeTree
    print freeTree
    print newTree
    print ((freeTree, []) -: goRight -: goLeft)

infixl 0 -:
(-:) :: a -> (a -> b) -> b
x -: f = f x

square :: Int -> Int
square x = x * x

data CMaybe a = CNothing | CJust a deriving (Show)

instance Functor CMaybe where
    fmap f CNothing = CNothing
    fmap f (CJust a) = CJust (f a)

instance Applicative CMaybe where
    pure = CJust
    CNothing <*> _ = CNothing
    (CJust f) <*> something = fmap f something

instance Monad CMaybe where
    return x = CJust x
    CNothing >>= f = CNothing
    CJust x >>= f = f x
    fail _ = CNothing

data Employee = Employee {code :: Int, name :: String} deriving (Show)

type Stack = [Int]
pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a:xs)

stackManip :: State Stack Int
stackManip = do
    push 3
    pop
    pop

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
    where multAll (Prob innerxs,p) = map (\(x,r) -> (x,p*r)) innerxs

instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs

instance Applicative Prob where
    pure x = Prob [(x,1%1)]

instance Monad Prob where
    return x = Prob [(x,1%1)]
    m >>= f = flatten (fmap f m)
    fail _ = Prob []

data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads,1%2),(Tails,1%2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads,1%10),(Tails,9%10)]

flipThree :: Prob Bool
flipThree = do
    a <- coin
    b <- coin
    c <- loadedCoin
    return (all (==Tails) [a,b,c])

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char
freeTree =
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty)
            )
            (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
            )
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
            )
        )
data Direction = L | R deriving (Show)
type Directions = [Direction]

changeToP :: Directions -> Tree Char -> Tree Char
changeToP (L:ds) (Node x l r) = Node x (changeToP ds l) r
changeToP (R:ds) (Node x l r) = Node x l (changeToP ds r)
changeToP []     (Node _ l r) = Node 'P' l r

elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt []     (Node x _ _) = x

type Breadcrumbs = [Direction]
goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)  
goLeft (Node _ l _, bs) = (l, L:bs)
goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)  
goRight (Node _ _ r, bs) = (r, R:bs)  

-- separating interface from implementation
-- the number generator System.Random is quite
-- slow, so we need to separate the interface from implementation
-- The typeclass is exactly what we want in Haskell.
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
   FlexibleInstances #-}

class (Monad m) => MonadSupply s m | m -> s where
    next :: m (Maybe s)

---- FunctionalDependencies example:
--import qualified Supply as S
--instance MonadSupply s (S.Supply s) where
    --next = S.next

---- example using typeclass MonadSupply
--showTwo :: (Show s) => S.Supply s String
--showTwo = do
  --a <- next
  --b <- next
  --return (show "a: " ++ show a ++ ", b: " ++ show b)

--showTwo_class :: (Show s, Monad m, MonadSupply s m) => m String
--showTwo_class = do
  --a <- next
  --b <- next
  --return (show "a: " ++ show a ++ ", b: " ++ show b)

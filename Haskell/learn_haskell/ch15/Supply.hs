-- Supply Monad is a highly encapsulated Monad
-- similar to State Monad. Instead of State, Supply
-- has a advantage that it is not leaky, it means
-- Supply doesn't allow modifying the state directly unlike set in State.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Supply
    (
      Supply
    , next
    , runSupply
    ) where
import Control.Monad.State

newtype Supply s a = S (State [s] a) deriving (Monad, Applicative, Functor)

next = S $ do
    st <- get
    case st of
      [] -> return Nothing
      (x:xs) -> do put xs
                   return (Just x)

runSupply (S m) xs = runState m xs

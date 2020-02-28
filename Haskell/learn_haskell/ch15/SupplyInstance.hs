import SupplyClass
newtype Reader e a = R { runReader :: e -> a }
instance Functor (Reader e)
instance Applicative (Reader e)
instance Monad (Reader e) where
    return a = R $ \_ -> a
    m >>= k = R $ \r -> runReader (k (runReader m r)) r

ask :: Reader e e
ask = R id
-- > runReader (ask >>= \x -> return (x * 3)) 2
-- 6
newtype MySupply e a = MySupply { runMySupply :: Reader e a }
    deriving (Monad)

instance MonadSupply e (MySupply e) where
    next = MySupply $ do
        v <- ask
        return $ Just v

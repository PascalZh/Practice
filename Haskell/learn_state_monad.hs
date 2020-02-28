#!/usr/bin/env runhaskell
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State
import System.Random

main :: IO ()
main = do
    print $ runState rollDie (mkStdGen 3232322)
    print $ runState rollDice (mkStdGen 566)

rollDie :: State StdGen Int
rollDie = state $ randomR (1, 6)

rollDice :: State StdGen (Int, Int)
rollDice = liftM2 (,) rollDie rollDie

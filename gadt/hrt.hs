{-# LANGUAGE RankNTypes #-}

import System.Random
import Control.Monad.State


type IdFunc = forall a. a -> a

type SomeInt = IdFunc -> Integer

someInt :: IdFunc -> Integer
someInt id' = id' 3

someOtherInt :: SomeInt -> Integer
someOtherInt someInt' = someInt' id + someInt' id

data Player = 
  Player {
    playerName :: String,
    playerPos :: (Double, Double)
  }
  deriving (Eq, Ord, Show)

type GenAction m = forall a. (Random a) => m a
type GenActionR m = forall a. (Random a) => (a, a) -> m a

genRandom :: (Random a, RandomGen g) => State g a
genRandom = state random

genRandomR :: (RandomGen g) => GenActionR (State g)
genRandomR range = state (randomR range)

randomPlayer :: (MonadIO m) => GenActionR m -> m Player
randomPlayer genR = do
  liftIO (putStrLn "Generating random player....")

  len <- genR (8, 12)
  name <- replicateM len (genR ('a', 'z'))
  x <- genR (-100, 100)
  y <- genR (-100, 100)

  liftIO (putStrLn "Done.")
  return (Player name (x, y))

main :: IO ()
-- -- main = do
-- --   print $ someOtherInt someInt
main = randomPlayer randomRIO >>= print
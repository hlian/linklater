module Utils where

import Data.Array.IO (IOArray, readArray, writeArray, newListArray)
import System.Random (randomRIO)
import System.Random.Shuffle (shuffleM)

-- Naked imports
import BasePrelude

type Permutation a = IOArray Int a

-- | Randomly shuffle a list
--   /O(N)/
shuffle :: [a] -> IO [a]
shuffle = shuffleM

-- | Randomly sample from a list, slowly
--   /O(N)/
sample :: [a] -> IO (Maybe a)
sample xs = do
  shuffled <- shuffle xs
  return (listToMaybe shuffled)

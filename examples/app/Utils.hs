module Utils where

import System.Random.Shuffle (shuffleM)

-- Naked imports
import BasePrelude

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

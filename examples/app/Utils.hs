module Utils where

import Data.Array.IO (IOArray, readArray, writeArray, newListArray)
import System.Random (randomRIO)

-- Naked imports
import BasePrelude

type Permutation a = IOArray Int a

-- | Randomly shuffle a list
--   /O(N)/
shuffle :: [a] -> IO (Permutation a)
shuffle xs = do
  ar <- copy xs
  shuffled <- forM [0..tops] $ \i -> do
      j <- randomRIO (i, tops)
      vi <- readArray ar i
      vj <- readArray ar j
      writeArray ar j vi
      return vj
  copy shuffled
  where
    tops = length xs
    copy :: [a] -> IO (Permutation a)
    copy src = newListArray (0, tops) src

-- | Randomly sample from a list, slowly
--   /O(N)/
sample :: [a] -> IO a
sample xs = do
  shuffled <- shuffle xs
  readArray shuffled 0

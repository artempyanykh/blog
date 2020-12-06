module Swaps where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Array.MArray
import Data.Array.ST (STArray, STUArray)
import Data.IntMap.Strict ((!))
import qualified Data.IntMap.Strict as IntMap
import Data.List
import Data.STRef (modifySTRef, newSTRef, readSTRef)

-- ST based solution
minimumSwapsST :: [Int] -> Int
minimumSwapsST input = runST $ do
  let inputLen = length input
      bounds = (1, inputLen)
  elements <- newSTArray bounds input
  elementPositions <- newSTArray_ bounds
  -- Fill in element positions
  forM_ [1 .. inputLen] $ \idx -> do
    el <- readArray elements idx
    writeArray elementPositions el idx
  numSwaps <- newSTRef 0 -- need a mutable! counter
  forM_ [1 .. inputLen] $ \idx -> do
    el <- readArray elements idx
    when (el /= idx) $ do
      desiredElPosition <- readArray elementPositions idx
      writeArray elementPositions el desiredElPosition
      writeArray elements desiredElPosition el
      modifySTRef numSwaps (+ 1)
  readSTRef numSwaps
  where
    newSTArray :: (Int, Int) -> [Int] -> ST s (STUArray s Int Int)
    newSTArray = newListArray

    newSTArray_ :: (Int, Int) -> ST s (STUArray s Int Int)
    newSTArray_ = newArray_

-- Data.Map based solution
minimumSwaps :: [Int] -> Int
minimumSwaps input =
  let indexedInput = zip [1 ..] input
      elements = IntMap.fromList indexedInput
      elementPositions = buildElementPositions indexedInput
      maxIdx = IntMap.size elements
      go numSwaps idx elements elementPositions
        | idx == maxIdx = numSwaps
        | elements ! idx == idx = go numSwaps (idx + 1) elements elementPositions
        | otherwise =
          let curEl = elements ! idx
              desiredElPosition = elementPositions ! idx
              updatedElPositions =
                IntMap.insert curEl desiredElPosition elementPositions
              updatedElementes = IntMap.insert desiredElPosition curEl elements
           in go (numSwaps + 1) (idx + 1) updatedElementes updatedElPositions
   in go 0 1 elements elementPositions
  where
    buildElementPositions indexedInput =
      foldl' (\acc (idx, el) -> IntMap.insert el idx acc) IntMap.empty indexedInput
module Main where

import Data.List
import Swaps
import System.Environment
import System.IO

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
  line <- getLine
  rest <- readMultipleLinesAsStringArray (n - 1)
  return (line : rest)

main :: IO ()
main = do
  stdout <- getEnv "OUTPUT_PATH"
  fptr <- openFile stdout WriteMode

  n <- readLn :: IO Int

  arrTemp <- getLine

  let arr = Data.List.map (read :: String -> Int) . words $ arrTemp

  let res = minimumSwapsST arr

  hPutStrLn fptr $ show res

  hFlush fptr
  hClose fptr

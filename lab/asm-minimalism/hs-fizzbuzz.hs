import System.Environment
import Data.List (uncons)
import Control.Monad (forM_, when, unless)

main = do
  args <- getArgs

  case headMay args of
    Nothing -> printUsage
    Just nStr  -> case parseNum nStr of
      Nothing -> printNanError
      Just n -> solveFizzBuzz n

----------------------------------------------------------
-- FizzBuzz solution
----------------------------------------------------------
solveFizzBuzz nMax = forM_ [1..nMax] $ \n -> do
  let rem3 = n `rem` 3
      rem5 = n `rem` 5

  when (rem3 == 0) $ putStr "Fizz"

  if rem5 == 0
     then putStr "Buzz"
     else unless (rem3 == 0) $ putStr (show n)

  putStrLn ""

----------------------------------------------------------
-- Helper functions
----------------------------------------------------------

printUsage = putStrLn "Usage: hs-fizzbuzz <number>"
printNanError = putStrLn "Error: Not a number"

headMay :: [a] -> Maybe a
headMay xs = fst <$> uncons xs

parseNum :: String -> Maybe Int
parseNum s = case reads s of
  (n, "") : xs -> Just n
  _ -> Nothing

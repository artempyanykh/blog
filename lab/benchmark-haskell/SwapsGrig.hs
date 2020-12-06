module SwapsGrig where

import Data.Array.IArray (Array, array, elems, (!))

imap :: (Int -> a -> b) -> [a] -> [b]
imap = m 1
  where
    m _ _ [] = []
    m i f (x : xs) = (f i x) : m (i + 1) f xs

swapsGrig :: [Int] -> Int
swapsGrig xs = n - cyclesCount
  where
    n = length xs
    bindings = imap (\i x -> (i, lookup i x)) xs
    lookup i x = if i <= x then x else lookup i (table ! x)
    table = array (1, n) bindings :: Array Int Int
    cyclesCount = length $ filter id $ imap (==) $ elems table
import Criterion
import Criterion.Main
import Swaps
import SwapsGrig
import System.Random.Shuffle (shuffleM)

-- perRunEnv
-- bench
-- minimumSwaps, minimumSwapsST
-- 10, 100, 1000

mkInput n = shuffleM [1 .. n]

mkBenchmarkable :: Int -> ([Int] -> Int) -> Benchmarkable
mkBenchmarkable n f = perRunEnv (mkInput n) $ \input -> return (f input)

mkBenchmark :: Int -> ([Int] -> Int) -> Benchmark
mkBenchmark n f = bench (show n) (mkBenchmarkable n f)

mkBenchGroup f groupName =
  bgroup groupName $ map (flip mkBenchmark f) [10, 100, 1000]

main :: IO ()
main =
  defaultMain [mkBenchGroup minimumSwapsST "ST", mkBenchGroup swapsGrig "Grigore"]

-- [mkBenchGroup minimumSwaps "Map", mkBenchGroup minimumSwapsST "ST"]
{-# LANGUAGE
  BangPatterns,
  BlockArguments,
  RankNTypes,
  ScopedTypeVariables,
  TypeOperators #-}

-- Algebraic operations require traversing the stack,
-- which causes this quadratic behavior in left-recursive functions.

import Test.Tasty.Bench (Benchmark, Benchmarkable, bcompareWithin, bench, bgroup, defaultMain, nf)
import Bluefin.Eff (Eff, type (:>), runPureEff)
import Bluefin.Algae
import Bluefin.Algae.State

-- Left recursive counter
leftRecCounter :: z :> zz => Handler (State Int) z -> Int -> Eff zz ()
leftRecCounter _state 0 = pure ()
leftRecCounter state n = do
  leftRecCounter state (n - 1)
  modify state (+ 1)

-- Benchmarking harness

-- @bcompareQuadratic name tolerance factor f@:
-- Assert that @f factor@ runs @factor * factor@ times slower than @f 1@,
-- within a relative tolerance interval @[1 - tolerance, 1 + tolerance]@.
bcompareQuadratic :: String -> Double -> Int -> (Int -> Benchmarkable) -> Benchmark
bcompareQuadratic = bcompareAsymptotic (\x -> x * x)

-- Compare the benchmarks (f 1) and (f factor) with respect to a given growth function
bcompareAsymptotic :: (Double -> Double) -> String -> Double -> Int -> (Int -> Benchmarkable) -> Benchmark
bcompareAsymptotic growth name tolerance factor f = bgroup name
  [ bench "baseline" (f 1)
  , bcompareWithin lower upper (name ++ ".baseline") $
      bench ("x" ++ show factor) (f factor)
  ] where
    factor2 = growth (fromIntegral factor)
    lower = (1 - tolerance) * factor2
    upper = (1 + tolerance) * factor2

runQuadraticCounter :: Int -> Int
runQuadraticCounter n = runPureEff $ execState 0 \state -> leftRecCounter state n

testQuadraticCounter :: Benchmark
testQuadraticCounter = bcompareQuadratic "quadratic-counter" 0.2 10 (\factor ->
  nf runQuadraticCounter (100 * factor))

main :: IO ()
main = defaultMain [ testQuadraticCounter ]

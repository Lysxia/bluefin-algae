{-# LANGUAGE
  BangPatterns,
  BlockArguments,
  ScopedTypeVariables,
  TypeOperators #-}
module Main (main) where

import Control.Monad (join)
import Data.Functor (void)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Bluefin.Eff (Eff, runPureEff, runEff, type (:>))
import qualified Bluefin.State as B
import Bluefin.Algae
import Bluefin.Algae.State
import Bluefin.Algae.Error
import qualified Bluefin.Algae.Error.Cancellable as EC
import qualified Bluefin.Algae.NonDeterminism as NonDet
import Bluefin.Algae.Coroutine
import qualified Bluefin.Exception as E
import qualified Bluefin.Exception.Dynamic as ED

-- * State

-- Simple sanity test

incr :: z :> zz => Handler (State Int) z -> Eff zz Int
incr state = do
  n <- get state
  put state (n + 1)
  pure n

-- Distinguishing Bluefin.Algae.State (pure state) from Bluefin.State (IORef)

algaeStateLitmus :: [Int]
algaeStateLitmus = runPureEff $ NonDet.toList \choice ->
  fst <$> runState 0 \state -> do
    _ <- NonDet.choose choice True False
    incr state

bluefinStateLitmus :: [Int]
bluefinStateLitmus = runPureEff $ NonDet.toList \choice ->
  B.evalState 0 \state -> do
    _ <- NonDet.choose choice True False
    incr' state
  where
    incr' state = do
      n <- B.get state
      B.put state (n + 1)
      pure n

testState :: TestTree
testState = testGroup "State"
  [ testCase "simple" $ runPureEff (runState 0 incr) @?= (0, 1)
  , testCase "litmus-0" $ algaeStateLitmus @?= [0,0]
  , testCase "litmus-1" $ bluefinStateLitmus @?= [0,1]
  ]

-- * Error

errorLitmus :: IO Int
errorLitmus = runEff \io -> snd <$> runState 0 \state ->
  void (try \err ->
    ED.onException (ED.ioeToDynExn io) (throw err ()) (void (incr state)))

errorCancelLitmus :: IO Int
errorCancelLitmus = runEff \io -> snd <$> runState 0 \state ->
  void (EC.try (ED.ioeToDynExn io) \err ->
    ED.onException (ED.ioeToDynExn io) (EC.throw err ()) (void (incr state)))

exnLitmus :: IO Int
exnLitmus = runEff \io -> snd <$> runState 0 \state ->
  void (E.try \exn ->
    ED.onException (ED.ioeToDynExn io) (E.throw exn ()) (void (incr state)))

testError :: TestTree
testError = testGroup "Error"
  [ testCase "litmus-error" $ errorLitmus >>= \n -> n @?= 0
  , testCase "litmus-errorC" $ errorCancelLitmus >>= \n -> n @?= 1
  , testCase "litmus-exn" $ exnLitmus >>= \n -> n @?= 1
  ]

-- * Nondeterminism

coinFlip :: z :> zz => Handler NonDet.Choice z -> Eff zz Bool
coinFlip choice =
  join $ NonDet.choose choice -- flip coin
    (NonDet.empty choice)     -- coin falls in gutter
    (join $ NonDet.choose choice
      (pure True)    -- heads
      (pure False))  -- tails

coinFlipList :: [Bool]
coinFlipList = runPureEff (NonDet.toList coinFlip)

testNonDet :: TestTree
testNonDet = testGroup "NonDet"
  [ testCase "coin-flip" $ coinFlipList @?= [True, False]
  ]

-- * Streaming

cumulSum :: z :> zz => Handler (Coroutine Int Int) z -> Eff zz a
cumulSum h = loop 0 where
  loop !n = do
    m <- yield h n
    loop (m + n)

feed :: Monad m => [i] -> Pipe o i m a -> m [o]
feed xs0 (Pipe m) = m >>= loop xs0 where
  loop _ (Done _) = pure []
  loop xs (Yielding o k) = case xs of
    [] -> pure [o]
    i : ys -> (o :) <$> (k i >>= loop ys)

testCoroutine :: TestTree
testCoroutine = testGroup "Coroutine"
  [ testCase "cumul-sum" $ runPureEff (feed [1,2,3] (evalCoroutine cumulSum)) @?= [0,1,3,6]
  ]

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testState
  , testError
  , testNonDet
  , testCoroutine
  ]

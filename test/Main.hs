{-# LANGUAGE
  BangPatterns,
  BlockArguments,
  RankNTypes,
  ScopedTypeVariables,
  TypeOperators #-}
module Main (main) where

import Control.Monad (join)
import Data.Functor (void)
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit
import Bluefin.Eff (Eff, runPureEff, runEff, bracket, type (:&), type (:>))
import qualified Bluefin.State as B
import Bluefin.Algae
import Bluefin.Algae.State
import Bluefin.Algae.Error
import qualified Bluefin.Algae.Error.DynExn as EC
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
  evalState 0 \state -> do
    _ <- NonDet.choose choice True False
    incr state >> incr state

bluefinStateLitmus :: [Int]
bluefinStateLitmus = runPureEff $ NonDet.toList \choice ->
  B.evalState 0 \state -> do
    _ <- NonDet.choose choice True False
    incr' state >> incr' state
  where
    incr' state = do
      n <- B.get state
      B.put state (n + 1)
      pure n

testState :: TestTree
testState = testGroup "State"
  [ testCase "simple" $ runPureEff (runState 0 incr) @?= (0, 1)
  , testCase "litmus-0" $ algaeStateLitmus @?= [1,1]
  , testCase "litmus-1" $ bluefinStateLitmus @?= [1,3]
  ]

-- * Error

onException :: Eff es a -> Eff es () -> Eff es a
onException run post = bracket (pure ()) (\_ -> post) (\_ -> run)

errorLitmus :: Int
errorLitmus = runPureEff $ snd <$> runState 0 \state ->
  void (try \err ->
    onException (throw err ()) (void (incr state)))

errorDynLitmus :: IO Int
errorDynLitmus = runEff \io -> snd <$> runState 0 \state ->
  void (EC.try (ED.ioeToDynExn io) \err ->
    onException (EC.throw err ()) (void (incr state)))

errorNoCancelLitmus :: Int
errorNoCancelLitmus = runPureEff $ snd <$> runState 0 \state ->
  void (try' \err ->
    onException (throw err ()) (void (incr state)))

exnLitmus :: Int
exnLitmus = runPureEff $ snd <$> runState 0 \state ->
  void (E.try \exn ->
    onException (E.throw exn ()) (void (incr state)))

testError :: TestTree
testError = testGroup "Error"
  [ testCase "litmus-error" $ errorLitmus @?= 1
  , testCase "litmus-error-dyn" $ errorDynLitmus >>= \n -> n @?= 1
  , testCase "litmus-error-no-cancel" $ errorNoCancelLitmus @?= 0
  , testCase "litmus-exn" $ exnLitmus @?= 1
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

toStream :: z :> zz =>
  (forall z0. Handler NonDet.Choice z0 -> Eff (z0 :& zz) a) ->
  Handler (Coroutine a ()) z -> Eff zz ()
toStream f h = NonDet.forAllChoices f (yield h)

testNonDet :: TestTree
testNonDet = testGroup "NonDet"
  [ testCase "coin-flip" $ coinFlipList @?= [True, False]
  , testCase "via-stream" $ runPureEff (consume [(), ()] (toStream coinFlip)) @?= [True, False]
  ]

-- * Streaming

cumulSum :: z :> zz => Handler (Coroutine Int Int) z -> Eff zz a
cumulSum h = loop 0 where
  loop !n = do
    m <- yield h n
    loop (m + n)

feed :: Monad m => [i] -> Pipe o i m a -> m [o]
feed xs0 (MkPipe m) = m >>= loop xs0 where
  loop _ (Done _) = pure []
  loop xs (Yielding o k) = case xs of
    [] -> pure [o]
    i : ys -> (o :) <$> (k i >>= loop ys)

coyield :: (z :> zz, z' :> zz) =>
  Handler (State ([i], [o])) z -> Handler (Error [o]) z' -> o -> Eff zz i
coyield state err o = do
  (is, os) <- get state
  case is of
    [] -> throw err (o : os)
    i : ys -> put state (ys, o : os) >> pure i

consume :: [i] -> (forall z0 zz0. Handler (Coroutine o i) z0 -> Eff (z0 :& zz0) a) -> Eff zz [o]
consume is f = do
  r <- try \err -> runState (is, []) \state ->
    forCoroutine f (coyield state err)
  pure $ reverse $ case r of
    Left os -> os
    Right (_, (_, os)) -> os

testCoroutine :: TestTree
testCoroutine = testGroup "Coroutine"
  [ testCase "cumul-sum" $ runPureEff (feed [1,2,3] (toPipe cumulSum)) @?= [0,1,3,6]
  , testCase "consume-sum" $ runPureEff (consume [1,2,3] cumulSum) @?= [0,1,3,6]
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

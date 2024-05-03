{-# LANGUAGE
  BangPatterns,
  BlockArguments,
  DataKinds,
  RankNTypes,
  ScopedTypeVariables,
  TypeApplications,
  TypeOperators #-}
module Main (main) where

import Control.Monad (join)
import Data.Bifunctor (bimap)
import Data.Coerce (coerce)
import Data.Functor (void)
import Data.Finite (Finite, finite, getFinite, moduloProxy, separateSum, separateZero)
import Data.Void (Void, absurd)
import GHC.TypeNats (KnownNat, type (+))
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
  , testCase "via-stream" $ runPureEff (feedCoroutine [(), ()] (toStream coinFlip)) @?= [True, False]
  ]

-- * Streaming

cumulSum :: z :> zz => Handler (Coroutine Int Int) z -> Eff zz a
cumulSum h = loop 0 where
  loop !n = do
    m <- yield h n
    loop (m + n)

feedCoroutine :: [i] -> (forall zz0. ScopedEff (Coroutine o i) zz0 a) -> Eff zz [o]
feedCoroutine is f = do
  r <- try \err -> runState (is, []) \state ->
    forCoroutine f (coyield state err)
  pure $ reverse $ case r of
    Left os -> os
    Right (_, (_, os)) -> os

coyield :: (z :> zz, z' :> zz) =>
  Handler (State ([i], [o])) z -> Handler (Error [o]) z' -> o -> Eff zz i
coyield state err o = do
  (is, os) <- get state
  case is of
    [] -> throw err (o : os)
    i : ys -> put state (ys, o : os) >> pure i

-- * Concurrency

-- | Coroutine identifier
newtype Cid n = Cid (Finite n) deriving (Eq, Show)

cid :: KnownNat n => Integer -> Cid n
cid = Cid . finite

-- | Next coroutine identifier (wraps around).
nextCid :: KnownNat n => Cid n -> Cid n
nextCid (Cid n) = Cid $ moduloProxy n (getFinite n + 1)  -- (cid+1) `mod` n

type Yell a = Error a

-- | Hot potato coroutine:
--
-- - receive hot @potato@ (whose value represents the temperature of the potato);
-- - if it is too hot (@potato > 0@), pass the potato to the next coroutine,
--   the potato cools down slightly at every pass;
-- - once the potato is cool enough, the coroutine which owns the potato yells its @Cid@.
hotpotato :: (KnownNat n, z :> zz, z' :> zz) =>
  Handler (Yell (Cid n)) z ->  -- ^ the coroutine may yell using this handle
  Cid n ->                     -- ^ coroutine ID
  Int ->                       -- ^ hot @potato@
  Handler (Coroutine (Cid n, Int) Int) z' ->  -- ^ handle to yield to another coroutine
  Eff zz void                  -- ^ does not return (the yell is an exception)
hotpotato yell c potato0 cr = loop potato0 where
  loop potato | potato > 0 = do                  -- if potato is too hot,
    potato' <- yield cr (nextCid c, potato - 1)  -- pass to the next coroutine, the potato cools down,
    loop potato'                                 -- wait for the potato to come back and repeat.
  loop _ | otherwise = throw yell c  -- if potato has cooled down, we won the potato.

-- | Four coroutines play the hot potato game.
hotpotatoes :: Cid 4
hotpotatoes = runPureEff do
  e <- try \yell ->
    loopCoPipe
      (  nobody
      +| hotpotato yell (cid 0 :: Cid 4)
      +| hotpotato yell (cid 1 :: Cid 4)
      +| hotpotato yell (cid 2 :: Cid 4)
      +| hotpotato yell (cid 3 :: Cid 4)) (Cid 3, 42)
  case e of
    Left c -> pure c
    Right o -> absurd o

-- | Empty copipe
nobody :: CoPipe (Cid 0, Int) o (Eff zz) void
nobody = mapCoPipe (\(Cid n, _) -> separateZero n) id id voidCoPipe

infixl 4 +|

-- | Add another hot potato coroutine to the mix.
--
-- The coroutines are numbered sequentially.
(+|) :: KnownNat n =>
  CoPipe (Cid n, i) o (Eff zz) Void ->
  CoPipeSEff i o zz a ->
  CoPipe (Cid (n + 1), i) o (Eff zz) a
(+|) l r = eitherCoPipe split l r'
  where
    r' = mapCoPipe (\(_ :: Cid 1, potato) -> potato) id id (toCoPipe r)

split :: forall n i. KnownNat n => (Cid (n + 1), i) -> Either (Cid n, i) (Cid 1, i)
split (c, potato) = bimap (flip (,) potato) (flip (,) potato) (coerce (separateSum @n @1) c)

testCoroutine :: TestTree
testCoroutine = testGroup "Coroutine"
  [ testCase "feedPipe-sum" $ runPureEff (feedPipe [1,2,3] (toPipe cumulSum)) @?= [0,1,3,6]
  , testCase "feedCoroutine-sum" $ runPureEff (feedCoroutine [1,2,3] cumulSum) @?= [0,1,3,6]
  , testCase "hotpotato" $ hotpotatoes @?= Cid 1
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

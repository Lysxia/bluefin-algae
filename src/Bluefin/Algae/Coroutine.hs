{-# LANGUAGE
  BangPatterns,
  GADTs,
  KindSignatures,
  RankNTypes,
  ScopedTypeVariables,
  StandaloneKindSignatures,
  TypeOperators #-}

-- | = Coroutines: yield as an algebraic effect
module Bluefin.Algae.Coroutine
  ( -- * Operations
    Coroutine(..)
  , yield

    -- * Handlers
  , execCoroutine
  , evalCoroutine
  , Pipe(..)
  , PipeEvent(..)
  ) where

import Bluefin.Eff
import Bluefin.Algae

-- | Coroutine effect with outputs @o@ and inputs @i@.
data Coroutine o i a where
  -- | Yield an output and wait for an input.
  Yield :: o -> Coroutine o i i

-- | Call the 'Yield' operation.
yield :: z :> zz => Handler (Coroutine o i) z -> o -> Eff zz i
yield h o = call h (Yield o)

-- | Apply a function to every 'Yield'.
execCoroutine :: forall o i a zz.
  (o -> Eff zz i) ->
  (forall z. Handler (Coroutine o i) z -> Eff (z :& zz) a) ->
  Eff zz a
execCoroutine h f = handle coroutineHandler f
  where
    coroutineHandler :: HandlerBody (Coroutine o i) zz a
    coroutineHandler (Yield o) k = h o >>= k

-- | Evaluate a coroutine into a tree.
evalCoroutine :: forall o i a zz.
  (forall z. Handler (Coroutine o i) z -> Eff (z :& zz) a) ->
  Pipe o i (Eff zz) a
evalCoroutine f = Pipe (handle coroutineHandler (wrap . f))
  where
    coroutineHandler :: HandlerBody (Coroutine o i) zz (PipeEvent o i (Eff zz) a)
    coroutineHandler (Yield o) k = pure (Yielding o k)

    wrap :: Eff (z :& zz) a -> Eff (z :& zz) (PipeEvent o i (Eff zz) a)
    wrap = fmap Done

-- | A tree of 'Yielding' events interleaved with @m@ computations.
newtype Pipe o i m a = Pipe (m (PipeEvent o i m a))

-- | Events of 'Pipe'.
data PipeEvent o i m a
  = Done a
  | Yielding o (i -> m (PipeEvent o i m a))

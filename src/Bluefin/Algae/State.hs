{-# LANGUAGE
  BangPatterns,
  GADTs,
  KindSignatures,
  RankNTypes,
  ScopedTypeVariables,
  StandaloneKindSignatures,
  TypeOperators #-}

-- | = State as an algebraic effect
--
-- The 'runState' handler calls each continuation exactly once.
-- It is compatible with single-shot continuations.
module Bluefin.Algae.State
  ( -- * Operations
    State(..)
  , get
  , put
  , put'
  , modify
  , modify'

    -- * Handlers
  , runState
  , evalState
  , execState
  ) where

import Data.Kind (Type)
import Bluefin.Eff (Eff, type (:&), type (:>))
import Bluefin.Algae

-- | The state effect.
data State (s :: Type) (r :: Type) where
  -- | Get the current state.
  Get :: State s s
  -- | Put a new state.
  Put :: s -> State s ()

-- | Get the current state. Call the 'Get' operation.
get :: z :> zz => Handler (State s) z -> Eff zz s
get h = call h Get

-- | Put a new state. Call the 'Put' operation.
put :: z :> zz => Handler (State s) z -> s -> Eff zz ()
put h s = call h (Put s)

-- | Strict variant of 'put'.
put' :: z :> zz => Handler (State s) z -> s -> Eff zz ()
put' h !s = call h (Put s)

-- | Modify the state.
modify :: z :> zz => Handler (State s) z -> (s -> s) -> Eff zz ()
modify h f = get h >>= put h . f

-- | Strict variant of 'modify'.
modify' :: z :> zz => Handler (State s) z -> (s -> s) -> Eff zz ()
modify' h f = get h >>= put' h . f

-- | Pure state handler.
runState ::
  s ->  -- ^ Initial state
  (forall z. Handler (State s) z -> Eff (z :& zz) a) ->  -- ^ Stateful computation
  Eff zz (a, s)
runState = runStateWith (,)

-- | Return only the result value.
evalState ::
  s ->  -- ^ Initial state
  (forall z. Handler (State s) z -> Eff (z :& zz) a) ->  -- ^ Stateful computation
  Eff zz a
evalState = runStateWith const

-- | Return only the final state.
execState ::
  s ->  -- ^ Initial state
  (forall z. Handler (State s) z -> Eff (z :& zz) a) ->  -- ^ Stateful computation
  Eff zz s
execState = runStateWith (const id)

runStateWith :: forall s a r zz.
  (a -> s -> r) ->  -- ^ Combine the result and final state.
  s ->  -- ^ Initial state
  (forall z. Handler (State s) z -> Eff (z :& zz) a) ->  -- ^ Stateful computation
  Eff zz r
runStateWith finish s0 f = unwrap s0 (handle stateHandler (wrap . f))
  where
    stateHandler :: HandlerBody (State s) zz (s -> Eff zz r)
    stateHandler Get k = pure (\s -> k s >>= \k1 -> k1 s)
    stateHandler (Put s) k = pure (\_ -> k () >>= \k1 -> k1 s)

    wrap :: Eff (z :& zz) a -> Eff (z :& zz) (s -> Eff zz r)
    wrap = fmap (\a s -> pure (finish a s))

    unwrap :: s -> Eff zz (s -> Eff zz r) -> Eff zz r
    unwrap s m = m >>= \k -> k s

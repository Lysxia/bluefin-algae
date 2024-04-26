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
  ( State(..)
  , get
  , put
  , runState
  ) where

import Data.Kind (Type)
import Bluefin.Eff (Eff, type (:&), type (:>))
import Bluefin.Algae

data State (s :: Type) (x :: Type) where
  Get :: State s s
  Put :: s -> State s ()

get :: z :> zz => Handler (State s) z -> Eff zz s
get h = call h Get

put :: z :> zz => Handler (State s) z -> s -> Eff zz ()
put h s = call h (Put s)

runState :: forall s a zz.
  s ->
  (forall z. Handler (State s) z -> Eff (z :& zz) a) ->
  Eff zz (a, s)
runState s0 f = unwrap s0 (handle stateHandler (wrap . f))
  where
    stateHandler :: HandlerBody (State s) zz (s -> Eff zz (a, s))
    stateHandler Get k = pure (\s -> k s >>= \k1 -> k1 s)
    stateHandler (Put s) k = pure (\_ -> k () >>= \k1 -> k1 s)

    wrap :: Eff (z :& zz) a -> Eff (z :& zz) (s -> Eff zz (a, s))
    wrap = fmap (\a s -> pure (a, s))

    unwrap :: s -> Eff zz (s -> Eff zz b) -> Eff zz b
    unwrap s m = m >>= \k -> k s

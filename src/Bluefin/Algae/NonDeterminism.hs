{-# LANGUAGE
  BangPatterns,
  GADTs,
  KindSignatures,
  RankNTypes,
  ScopedTypeVariables,
  StandaloneKindSignatures,
  TypeOperators #-}

-- | Nondeterministic choice as an algebraic effect.
--
-- === Warning: Non-linear continuations
--
-- The handlers 'forAllChoices' and 'enumerate' call continuations zero or twice.
-- Don't use them to handle a computation that must ensure linear usage of resources.
module Bluefin.Algae.NonDeterminism
  ( -- * Operations
    Choice(..)
  , choose
  , empty
    -- * Handlers
  , forAllChoices
  , toList
  ) where

import Data.Kind (Type)
import Bluefin.Internal (insertFirst)
import Bluefin.Eff (Eff, type (:&), type (:>))
import Bluefin.Algae

-- | Choice effect.
data Choice (a :: Type) where
  -- | Choose one of two alternatives.
  Choose :: a -> a -> Choice a
  -- | No choice.
  Empty :: Choice a

-- | Choose one of two alternatives. Call the 'Choose' operation.
choose :: z :> zz => Handler Choice z -> a -> a -> Eff zz a
choose h x y = call h (Choose x y)

-- | No choice. Call the 'Empty' operation.
empty :: z :> zz => Handler Choice z -> Eff zz a
empty h = call h Empty

-- | Apply a function to every result of the nondeterministic computation.
forAllChoices :: forall a zz.
  (forall z. Handler Choice z -> Eff (z :& zz) a) ->
  (a -> Eff zz ()) ->
  Eff zz ()
forAllChoices f h = handleChoice (>>= insertFirst . h) (pure ()) (>>) f

-- | Collect the results of a nondeterministic computation in a list.
toList :: forall a zz.
  (forall z. Handler Choice z -> Eff (z :& zz) a) ->
  Eff zz [a]
toList f = unwrap (handleChoice (fmap (:)) (pure id) (liftA2 (.)) f)
  where
    unwrap :: Eff zz ([a] -> [a]) -> Eff zz [a]
    unwrap = fmap ($ [])

-- | Generic 'Choice' handler parameterized by a monoid.
handleChoice :: forall a r zz.
  (forall z. Eff (z :& zz) a -> Eff (z :& zz) r) ->
  Eff zz r ->
  (Eff zz r -> Eff zz r -> Eff zz r) ->
  (forall z. Handler Choice z -> Eff (z :& zz) a) ->
  Eff zz r
handleChoice oneE emptyE appendE f = handle choiceHandler (oneE . f)
  where
    choiceHandler :: HandlerBody Choice zz r
    choiceHandler (Choose x y) k = appendE (k x) (k y)
    choiceHandler Empty _k = emptyE

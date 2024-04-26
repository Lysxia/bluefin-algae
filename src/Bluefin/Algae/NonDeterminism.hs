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
module Bluefin.Algae.NonDeterminism where

import Data.Kind (Type)
import Bluefin.Internal (insertFirst)
import Bluefin.Eff (Eff, type (:&), type (:>))
import Bluefin.Algae

-- | Choice effect.
data Choice (a :: Type) where
  Choose :: a -> a -> Choice a
  Empty :: Choice a

choose :: z :> zz => Handler Choice z -> a -> a -> Eff zz a
choose h x y = call h (Choose x y)

empty :: z :> zz => Handler Choice z -> Eff zz a
empty h = call h Empty

-- | Apply a function to every result of the nondeterministic computation.
forAllChoices :: forall a zz.
  (forall z. Handler Choice z -> Eff (z :& zz) a) ->
  (a -> Eff zz ()) ->
  Eff zz ()
forAllChoices f h = handle choiceHandler (\c -> f c >>= (insertFirst . h))
  where
    choiceHandler :: HandlerBody Choice zz ()
    choiceHandler (Choose x y) k = k x >> k y
    choiceHandler Empty _k = pure ()

-- | Collect the results of a nondeterministic computation in a list.
toList :: forall a zz.
  (forall z. Handler Choice z -> Eff (z :& zz) a) ->
  Eff zz [a]
toList f = unwrap (handle choiceHandler (\c -> (:) <$> f c))
  where
    choiceHandler :: HandlerBody Choice zz ([a] -> [a])
    choiceHandler (Choose x y) k = liftA2 (.) (k x) (k y)
    choiceHandler Empty _k = pure id

    unwrap :: Eff zz ([a] -> [a]) -> Eff zz [a]
    unwrap = fmap ($ [])

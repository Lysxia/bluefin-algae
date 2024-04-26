{-# LANGUAGE
  BangPatterns,
  GADTs,
  KindSignatures,
  RankNTypes,
  ScopedTypeVariables,
  StandaloneKindSignatures,
  TypeOperators #-}

-- | = Errors as an algebraic effect
--
-- === Warning: Affine continuations
--
-- The error handlers in this module discard the continuation.
-- For single-shot continuations (must be used exactly once),
-- use "Bluefin.Algae.Error.Cancellable".
module Bluefin.Algae.Error
  ( Error(..)
  , try
  , throw
  ) where

import Data.Kind (Type)
import Bluefin.Eff (Eff, type (:&), type (:>))
import Bluefin.Algae

data Error (e :: Type) (x :: Type) where
  Throw :: e -> Error e x

catch :: forall e a zz.
  (forall z. Handler (Error e) z -> Eff (z :& zz) a) ->
  (e -> Eff zz a) ->
  Eff zz a
catch f h = handle errorHandler f
  where
    errorHandler :: HandlerBody (Error e) zz a
    errorHandler (Throw e) _ = h e

try :: forall e a zz.
  (forall z. Handler (Error e) z -> Eff (z :& zz) a) ->
  Eff zz (Either e a)
try f = catch (fmap Right . f) (pure . Left)

throw :: z :> zz => Handler (Error e) z -> e -> Eff zz a
throw h e = call h (Throw e)

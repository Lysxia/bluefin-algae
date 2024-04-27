{-# LANGUAGE
  BangPatterns,
  GADTs,
  RankNTypes,
  ScopedTypeVariables,
  StandaloneKindSignatures,
  TypeOperators #-}

-- | = Errors as an algebraic effect
--
-- Variant of "Bluefin.Algae.Error" that uses dynamic exceptions to cancel
-- continuations.
module Bluefin.Algae.Error.DynExn
  ( Error(..)
  , throw
  , catch
  , try
  ) where

import Bluefin.Eff (Eff, type (:&), type (:>))
import Bluefin.Algae.DynExn
import Bluefin.Algae.Error (Error(..))
import Bluefin.Exception.Dynamic (DynExn)

-- | Catch an error.
catch :: forall e a ex zz. ex :> zz =>
  DynExn ex ->
  (forall z. Handler ex (Error e) z -> Eff (z :& zz) a) ->
  (e -> Eff zz a) ->
  Eff zz a
catch ex f h = handle ex errorHandler f
  where
    errorHandler :: HandlerBody ex (Error e) zz a
    errorHandler (Throw e) k = cancel ex k *> h e

-- | Return 'Either' the error or the result of the handled computation. 
try :: forall e a ex zz. ex :> zz =>
  DynExn ex ->
  (forall z. Handler ex (Error e) z -> Eff (z :& zz) a) ->
  Eff zz (Either e a)
try ex f = catch ex (fmap Right . f) (pure . Left)

-- | Throw an error. Call the 'Throw' operation.
throw :: (ex :> zz, z :> zz) => Handler ex (Error e) z -> e -> Eff zz a
throw h e = call h (Throw e)

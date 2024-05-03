{-# LANGUAGE
  BangPatterns,
  GADTs,
  RankNTypes,
  ScopedTypeVariables,
  StandaloneKindSignatures,
  TypeOperators #-}

-- | = Exceptions as an algebraic effect
--
-- Variant of "Bluefin.Algae.Exception" that uses dynamic exceptions to cancel
-- continuations.
module Bluefin.Algae.Exception.DynExn
  ( Exception(..)
  , throw
  , catch
  , try
  ) where

import Bluefin.Eff (Eff, type (:&), type (:>))
import Bluefin.Algae.DynExn
import Bluefin.Algae.Exception (Exception(..))
import Bluefin.Exception.Dynamic (DynExn)

-- | Catch an exception.
catch :: forall e a ex zz. ex :> zz =>
  DynExn ex ->
  (forall z. Handler ex (Exception e) z -> Eff (z :& zz) a) ->
  (e -> Eff zz a) ->
  Eff zz a
catch ex f h = handle ex exceptionHandler f
  where
    exceptionHandler :: HandlerBody ex (Exception e) zz a
    exceptionHandler (Throw e) k = cancel ex k *> h e

-- | Return 'Either' the exception or the result of the handled computation.
try :: forall e a ex zz. ex :> zz =>
  DynExn ex ->
  (forall z. Handler ex (Exception e) z -> Eff (z :& zz) a) ->
  Eff zz (Either e a)
try ex f = catch ex (fmap Right . f) (pure . Left)

-- | Throw an exception. Call the 'Throw' operation.
throw :: (ex :> zz, z :> zz) => Handler ex (Exception e) z -> e -> Eff zz a
throw h e = call h (Throw e)

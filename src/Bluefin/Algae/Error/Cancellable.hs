{-# LANGUAGE
  BangPatterns,
  GADTs,
  RankNTypes,
  ScopedTypeVariables,
  StandaloneKindSignatures,
  TypeOperators #-}

-- | = Errors with cancellable continuations
--
-- Cancellable variant of "Bluefin.Algae.Error".
--
-- The error handlers in this module use the continuation exactly once,
-- cancelling the continuation ('cancel') to trigger its exception handlers.
module Bluefin.Algae.Error.Cancellable
  ( Error(..)
  , try
  , throw
  ) where

import Bluefin.Eff (Eff, type (:&), type (:>))
import Bluefin.Algae.Cancellable
import Bluefin.Algae.Error (Error(..))
import Bluefin.Exception.Dynamic (DynExn)

catch :: forall e a ex zz. ex :> zz =>
  DynExn ex ->
  (forall z. Handler ex (Error e) z -> Eff (z :& zz) a) ->
  (e -> Eff zz a) ->
  Eff zz a
catch ex f h = handle ex errorHandler f
  where
    errorHandler :: HandlerBody ex (Error e) zz a
    errorHandler (Throw e) k = cancel ex k *> h e

try :: forall e a ex zz. ex :> zz =>
  DynExn ex ->
  (forall z. Handler ex (Error e) z -> Eff (z :& zz) a) ->
  Eff zz (Either e a)
try ex f = catch ex (fmap Right . f) (pure . Left)

throw :: (ex :> zz, z :> zz) => Handler ex (Error e) z -> e -> Eff zz a
throw h e = call h (Throw e)

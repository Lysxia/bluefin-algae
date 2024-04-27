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
-- These scoped exceptions are similar to "Bluefin.Exception".
-- The main difference is that they cannot be intercepted by 'Bluefin.Exception.bracket',
-- so the basic handler 'catch' does not free resources allocated via 'Bluefin.Exception.bracket'.
-- Use 'catch'' instad.
module Bluefin.Algae.Error
  ( Error(..)

    -- * Basic interface
  , catch
  , try
  , throw

    -- * Cancelled continuation
  , catch'
  , try'
  ) where

import Data.Kind (Type)
import Bluefin.Eff (Eff, type (:&), type (:>))
import Bluefin.Algae

data Error (e :: Type) (x :: Type) where
  Throw :: e -> Error e x

-- | Exception handler.
--
-- === Warning: Discarded continuations
--
-- 'catch' discard the continuation, which may be problematic
-- if there are resources to be freed by the continuation (typically
-- if 'throw' was called in the middle of a 'Bluefin.Exception.bracket').
-- Use 'catch'' to free those resources instead.
catch :: forall e a zz.
  (forall z. Handler (Error e) z -> Eff (z :& zz) a) ->
  (e -> Eff zz a) ->
  Eff zz a
catch f h = handle errorHandler f
  where
    errorHandler :: HandlerBody (Error e) zz a
    errorHandler (Throw e) _ = h e

-- | Return 'Either' the exception or the result.
--
-- 'try' discards the continuation like 'catch'.
-- Use 'try'' to 'cancel' it instead.
try :: forall e a zz.
  (forall z. Handler (Error e) z -> Eff (z :& zz) a) ->
  Eff zz (Either e a)
try f = catch (fmap Right . f) (pure . Left)

-- | Variant of 'catch' with linear continuations.
--
-- The continuation is canceled ('cancel') when
-- an exception is thrown to this handler.
catch' :: forall e a zz.
  (forall z. Handler (Error e) z -> Eff (z :& zz) a) ->
  (e -> Eff zz a) ->
  Eff zz a
catch' f h = handle' errorHandler f
  where
    errorHandler :: HandlerBody' (Error e) zz a
    errorHandler (Throw e) k = cancel k >> h e

-- | Variant of 'try' with linear continuations.
try' :: forall e a zz.
  (forall z. Handler (Error e) z -> Eff (z :& zz) a) ->
  Eff zz (Either e a)
try' f = catch' (fmap Right . f) (pure . Left)

throw :: z :> zz => Handler (Error e) z -> e -> Eff zz a
throw h e = call h (Throw e)

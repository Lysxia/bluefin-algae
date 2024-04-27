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
-- These scoped errors are similar to "Bluefin.Exception".
--
-- Algebraic operations in Bluefin are truly scoped:
-- they cannot be intercepted by exception handlers, notably 'Bluefin.Exception.bracket'.
--
-- 'catch' and 'try' make an explicit call to 'cancel' to trigger exception handlers.
-- This makes them equivalent to "Bluefin.Exception".
--
-- The simpler variants 'catch'' and 'try'' don't use 'cancel', so they are
-- faster when there is no 'Bluefin.Exception.bracket' to worry about.
module Bluefin.Algae.Error
  ( -- * Operations
    Error(..)
  , throw

    -- * Default handlers
  , catch
  , try

    -- * Variant without cancelling continuations
  , catch'
  , try'
  ) where

import Data.Kind (Type)
import Bluefin.Eff (Eff, type (:&), type (:>))
import Bluefin.Algae

-- | Error interface.
data Error (e :: Type) (r :: Type) where
  -- | Throw an error.
  Throw :: e -> Error e r

-- | Throw an error. Call the 'Throw' operation.
throw :: z :> zz => Handler (Error e) z -> e -> Eff zz a
throw h e = call h (Throw e)

-- | Catch an error.
--
-- Simple version of 'catch' which just discards the continuation
-- instead of explicitly cancelling it.
--
-- === Warning: Discarded continuations
--
-- 'catch'' discards the continuation, which may be problematic
-- if there are resources to be freed by the continuation (typically
-- if 'throw' was called in the middle of a 'Bluefin.Exception.bracket').
-- Use 'catch' to free those resources instead.
--
-- Without anything like 'Bluefin.Exception.bracket', 'catch'' does less work.
-- 'catch' makes 'throw' traverse the stack twice (first to find the prompt,
-- then to 'cancel' the continuation).
-- 'catch'' makes 'throw' traverse the stack only once.
catch' :: forall e a zz.
  (forall z. Handler (Error e) z -> Eff (z :& zz) a) ->  -- ^ Handled computation
  (e -> Eff zz a) ->  -- ^ Error clause
  Eff zz a
catch' f h = handle errorHandler f
  where
    errorHandler :: HandlerBody (Error e) zz a
    errorHandler (Throw e) _ = h e

-- | Return 'Either' the error or the result of the handled computation.
--
-- Simple version of 'try' which discards the continuation (like 'catch'').
try' :: forall e a zz.
  (forall z. Handler (Error e) z -> Eff (z :& zz) a) ->
  Eff zz (Either e a)
try' f = catch' (fmap Right . f) (pure . Left)

-- | Catch an error.
--
-- The continuation is canceled ('cancel') when
-- an error is thrown to this handler.
catch :: forall e a zz.
  (forall z. Handler (Error e) z -> Eff (z :& zz) a) ->
  (e -> Eff zz a) ->
  Eff zz a
catch f h = handle' errorHandler f
  where
    errorHandler :: HandlerBody' (Error e) zz a
    errorHandler (Throw e) k = cancel k >> h e

-- | Return 'Either' the error or the result of the handled computation.
--
-- The continuation is canceled ('cancel') when
-- an error is thrown to this handler.
try :: forall e a zz.
  (forall z. Handler (Error e) z -> Eff (z :& zz) a) ->
  Eff zz (Either e a)
try f = catch (fmap Right . f) (pure . Left)

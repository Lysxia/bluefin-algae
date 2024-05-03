{-# LANGUAGE
  BangPatterns,
  GADTs,
  KindSignatures,
  RankNTypes,
  ScopedTypeVariables,
  StandaloneKindSignatures,
  TypeOperators #-}

-- | = Exceptions as an algebraic effect
--
-- These scoped exceptions are similar to "Bluefin.Exception".
--
-- Algebraic operations in Bluefin are truly scoped:
-- they cannot be intercepted by exception handlers, notably 'Bluefin.Eff.bracket'.
--
-- 'catch' and 'try' make an explicit call to 'cancel' to trigger exception handlers.
-- This makes them equivalent to "Bluefin.Exception".
--
-- The simpler variants 'catch'' and 'try'' don't use 'cancel', so they are
-- faster when there is no 'Bluefin.Eff.bracket' to worry about.
module Bluefin.Algae.Exception
  ( -- * Operations
    Exception(..)
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

-- | Exception interface.
data Exception (e :: Type) (r :: Type) where
  -- | Throw an exception.
  Throw :: e -> Exception e r

-- | Throw an exception. Call the 'Throw' operation.
throw :: z :> zz => Handler (Exception e) z -> e -> Eff zz a
throw h e = call h (Throw e)

-- | Catch an exception.
--
-- Simple version of 'catch' which just discards the continuation
-- instead of explicitly cancelling it.
--
-- === Warning: Discarded continuations
--
-- 'catch'' discards the continuation, which may be problematic
-- if there are resources to be freed by the continuation (typically
-- if 'throw' was called in the middle of a 'Bluefin.Eff.bracket').
-- Use 'catch' to free those resources instead.
--
-- Without anything like 'Bluefin.Eff.bracket', 'catch'' does less work.
-- 'catch' makes 'throw' traverse the stack twice (first to find the prompt,
-- then to 'cancel' the continuation).
-- 'catch'' makes 'throw' traverse the stack only once.
catch' :: forall e a zz.
  (forall z. Handler (Exception e) z -> Eff (z :& zz) a) ->  -- ^ Handled computation
  (e -> Eff zz a) ->  -- ^ Exception clause
  Eff zz a
catch' f h = handle exceptionHandler f
  where
    exceptionHandler :: HandlerBody (Exception e) zz a
    exceptionHandler (Throw e) _ = h e

-- | Return 'Either' the exception or the result of the handled computation.
--
-- Simple version of 'try' which discards the continuation (like 'catch'').
try' :: forall e a zz.
  (forall z. Handler (Exception e) z -> Eff (z :& zz) a) ->
  Eff zz (Either e a)
try' f = catch' (fmap Right . f) (pure . Left)

-- | Catch an exception.
--
-- The continuation is canceled ('cancel') when
-- an exception is thrown to this handler.
catch :: forall e a zz.
  (forall z. Handler (Exception e) z -> Eff (z :& zz) a) ->
  (e -> Eff zz a) ->
  Eff zz a
catch f h = handle' exceptionHandler f
  where
    exceptionHandler :: HandlerBody' (Exception e) zz a
    exceptionHandler (Throw e) k = cancel k >> h e

-- | Return 'Either' the exception or the result of the handled computation.
--
-- The continuation is canceled ('cancel') when
-- an exception is thrown to this handler.
try :: forall e a zz.
  (forall z. Handler (Exception e) z -> Eff (z :& zz) a) ->
  Eff zz (Either e a)
try f = catch (fmap Right . f) (pure . Left)

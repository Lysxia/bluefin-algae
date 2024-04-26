{-# LANGUAGE
  BangPatterns,
  GADTs,
  RankNTypes,
  ScopedTypeVariables,
  StandaloneKindSignatures,
  TypeOperators #-}

-- | = Algebraic effects and named handlers with cancellable continuations
--
-- See "Bluefin.Algae" for a general exposition on effect handlers.
-- "Bluefin.Algae" and "Bluefin.Algae.Cancellable" use the same names
-- so they should not be imported unqualified at the same time.
--
-- Cancellable continuations are useful to work with native exception handlers
-- such as 'Control.Exception.bracket' and other resource-management schemes à
-- la @resourcet@.
--
-- Cancellable continuations should be called exactly once (via 'continue' or 'discontinue'):
-- - at least once to ensure resources are eventually freed (no leaks);
-- - at most once to avoid use-after-free errors.
--
-- Enforcing this requirement with linear types would be a welcome contribution.
--
-- === Example
--
-- ==== Problem
--
-- Given 'Bluefin.Exception.Dynamic.bracket' and a @Fail@ effect,
-- the simple 'Bluefin.Algae.handle' from "Bluefin.Algae" may cause resource leaks:
--
-- @
-- 'Bluefin.Algae.handle' (\\_e _k -> pure Nothing)
--   ('Bluefin.Exception.Dynamic.bracket' ex acquire release (\\_ -> 'call' h Fail))
-- @
--
-- @bracket@ is intended to ensure that the acquired resource is released even if the bracketed
-- function throws an exception. However, when the @Fail@ operation is called, the handler
-- @(\\_e _k -> pure Nothing)@ discards the continuation @_k@ which contains the
-- exception handler installed by @bracket@.
-- The resource leaks because @release@ will never be called.
--
-- ==== Solution
--
-- Using 'handle' from this module instead lets us cancel the continuation with 'discontinue'.
-- Cancellable continuations require a 'DynExn' or 'IOE' handle.
--
-- @
-- 'handle'' io (\\_e k ->
--      try @CancelContinuation ('discontinueIO' k CancelContinuation) >> pure Nothing)
--   ('Bluefin.Exception.Dynamic.bracket' acquire release (\\_ -> 'call' h Fail))
--
-- data CancelContinuation = CancelContinuation deriving (Show, Exception)
-- @
module Bluefin.Algae.Cancellable
  ( AEffect
  , HandlerBody
  , Handler
  , handle
  , call
  , continue
  , discontinue
  , discontinueIO
  ) where

import Control.Exception (Exception)
import Data.Kind (Type)
import Bluefin.Internal (Eff, Effects, type (:&), type (:>), IOE)
import Bluefin.DelCont
import Bluefin.Exception.Dynamic
import Bluefin.Algae (AEffect)

-- | Interpretation of an algebraic effect @f@: a function to handle the operations of @f@
-- with cancellable continuations.
type HandlerBody :: Effects -> AEffect -> Effects -> Type -> Type
type HandlerBody ex f ss a = (forall x ss0. ex :> ss0 => f x -> (Eff ss0 x -> Eff ss a) -> Eff ss a)

-- | Handler to call operations of the effect @f@ with cancellable continuations.
type Handler :: Effects -> AEffect -> Effects -> Type
data Handler ex f s where
  Handler :: !(PromptTag ss a s) -> HandlerBody ex f ss a -> Handler ex f s

-- | Handle operations of @f@ with cancellable continuations.
--
-- The handle for exceptions (first argument) is only there to guide type inference.
-- it can be either 'IOE' or 'DynExn'.
handle ::
  h ex ->
  HandlerBody ex f ss a ->
  (forall s. Handler ex f s -> Eff (s :& ss) a) ->
  Eff ss a
handle _ h act = reset (\p -> act (Handler p h))

-- | Call an operation of @f@ with cancellable continuations.
call :: (ex :> es, s :> es) => Handler ex f s -> f a -> Eff es a
call (Handler p h) op = shift0 p (\k -> h op k)

-- | Resume a cancellable continuation with a result.
--
-- In other words, this converts a cancellable continuation to a simple continuation.
continue :: (Eff ss0 b -> Eff ss a) -> b -> Eff ss a
continue k = k . pure

-- | Cancel a continuation: resume by throwing a (dynamic) exception.
discontinue :: (Exception e, ex :> es0) => DynExn ex -> (Eff es0 b -> Eff es a) -> e -> Eff es a
discontinue ex k e = k (throw ex e)

-- | 'discontinue' with an 'IOE' handle instead of the more restrictive 'DynExn'.
--
-- Note that different outcomes are possible depending on your handled computation.
-- Be sure to handle them appropriately.
--
-- - A common situation is that the continuation will rethrow the initial exception,
--   then you can just catch it.
-- - The continuation may throw a different exception, so you should be
--   careful to catch the right exception.
-- - The continuation may also catch your exception and terminate normally
--   with a result of type @a@.
discontinueIO :: (Exception e, io :> es0) => IOE io -> (Eff es0 b -> Eff es a) -> e -> Eff es a
discontinueIO io = discontinue (ioeToDynExn io)

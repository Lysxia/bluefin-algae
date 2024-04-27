{-# LANGUAGE
  BangPatterns,
  DeriveAnyClass,
  GADTs,
  RankNTypes,
  ScopedTypeVariables,
  StandaloneKindSignatures,
  TypeOperators #-}

-- | = Algebraic effects and named handlers
--
-- Variant of "Bluefin.Algae" using dynamic exceptions to cancel continuations.
module Bluefin.Algae.DynExn
  ( AEffect
  , HandlerBody
  , Handler
  , handle
  , call
  , continue
  , discontinue
  , discontinueIO
  , cancel
  ) where

import Control.Exception (Exception)
import Data.Kind (Type)
import Data.Functor (void)
import Bluefin.Internal (Eff, Effects, type (:&), type (:>), IOE)
import Bluefin.DelCont (PromptTag, Continuation, reset, shift0, resume, continue)
import Bluefin.Exception.Dynamic
import Bluefin.Algae (AEffect)

-- | Interpretation of an algebraic effect @f@: a function to handle the operations of @f@
-- with cancellable continuations.
type HandlerBody :: Effects -> AEffect -> Effects -> Type -> Type
type HandlerBody ex f ss a = (forall x ss0. ex :> ss0 => f x -> Continuation ss0 ss x a -> Eff ss a)

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

-- | Resume by throwing a (dynamic) exception.
--
-- Note that different outcomes are possible depending on your handled computation.
-- Be sure to handle them appropriately.
--
-- - A common situation is that the continuation will rethrow the initial exception,
--   then you can just catch it (or use 'cancel').
-- - The continuation may throw a different exception, so you should be
--   careful to catch the right exception.
-- - The continuation may also catch your exception and terminate normally
--   with a result of type @a@.
discontinue :: (Exception e, ex :> es0) => DynExn ex -> Continuation es0 es b a -> e -> Eff es a
discontinue ex k e = resume k (throw ex e)

-- | Specialization of 'discontinue' to 'IOE'.
discontinueIO :: (Exception e, io :> es0) => IOE io -> Continuation es0 es b a -> e -> Eff es a
discontinueIO io = discontinue (ioeToDynExn io)

-- | 'discontinue' a continuation with the 'CancelContinuation' exception and catch it when it
-- is re-thrown by the continuation.
--
-- The continuation SHOULD re-throw 'CancelContinuation' if it catches it.
cancel :: (ex :> es0, ex :> es) => DynExn ex -> Continuation es0 es b a -> Eff es ()
cancel ex k = catch ex (void (discontinue ex k CancelContinuation)) (\CancelContinuation -> pure ())

data CancelContinuation = CancelContinuation
  deriving (Show, Exception)

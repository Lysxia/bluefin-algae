{-# LANGUAGE
  BangPatterns,
  GADTs,
  RankNTypes,
  ScopedTypeVariables,
  StandaloneKindSignatures,
  TypeOperators #-}

-- | = Algebraic effects and named handlers
--
-- Algebraic effect handlers are a powerful framework for
-- user-defined effects with a simple equational intuition.
--
-- Algebraic effect handlers are expressive enough to define various effects
-- from scratch. In comparison, the 'Bluefin.State.runState' handler from
-- "Bluefin.State" requires mutable references (@IORef@), relying on @IO@'s
-- built-in statefulness. In terms of pure expressiveness, delimited
-- continuations are all you need.
--
-- An "algebraic effect" is a signature for a set of operations which we
-- represent with a GADT. For example, the "state effect" @State s@ contains
-- two operations: @Get@ takes no parameter and returns a value of type @s@,
-- and @Put@ takes a value of type @s@ and returns @()@. The constructors
-- @Get@ and @Put@ are "uninterpreted operations": they only describe the types
-- of arguments and results, with no intrinsic meaning.
-- 
-- @
-- data 'Bluefin.Algae.State.State' s r where
--   Get :: 'Bluefin.Algae.State.State' s s
--   Put :: s -> 'Bluefin.Algae.State.State' s ()
-- @
--
-- Below is an example of a stateful computation: a term of some type @'Eff' zz a@ with
-- a state handler @h :: 'Handler' ('Bluefin.Algae.State.State' s) z@ in scope (@z :> zz@).
-- The @State@ operations can be called using 'call' and the state handler @h@.
--
-- @
-- -- Increment a counter and return its previous value.
-- incr :: z :> zz => 'Handler' ('Bluefin.Algae.State.State' Int) z -> 'Eff' zz Int
-- incr h = do
--     n <- get
--     put (n + 1)
--     pure n
--   where
--     get = 'call' h Get
--     put s = 'call' h (Put s)
-- @
--
-- We handle the state effect by giving an interpretation of the @Get@ and @Put@
-- operations, as a function @f :: 'HandlerBody' (State s) zz a@.
--
-- To 'call' @Get@ or @Put@ is to call the function @f@ supplied by 'handle'.
-- The following equations show how 'handle' propagates an interpretation
-- @f@ throughout a computation that calls @Get@ and @Put@:
--
-- @
-- 'handle' f (\\h -> 'call' h Get     >>= k h) = f Get     ('handle' f (\\h -> k h))
-- 'handle' f (\\h -> 'call' h (Put s) >>= k h) = f (Put s) ('handle' f (\\h -> k h))
-- 'handle' f (\\h -> pure r) = pure r
-- @
--
-- With those equations, @'handle' f@ applied to the above @incr@ simplifies to:
--
-- @
-- 'handle' f incr =
--   f Get \\n ->
--   f (Put (n+1)) \\() ->
--   pure n
-- @
--
-- === References
--
-- - <https://homepages.inf.ed.ac.uk/gdp/publications/handling-algebraic-effects.pdf Handling Algebraic Effects> (2013) by Gordon D. Plotkin and Matija Pretnar.
-- - <https://www.microsoft.com/en-us/research/uploads/prod/2021/05/namedh-tr.pdf First-class names for effect handlers> (2021) by Ningning Xie, Youyou Cong, and Daan Leijen.
module Bluefin.Algae
  ( AEffect

    -- * Simple interface
  , HandlerBody
  , Handler
  , handle
  , call

    -- * Cancellable continuations
    -- $cancel
  , HandlerBody'
  , handle'
  , continue
  , cancel
  ) where

import Data.Kind (Type)
import Bluefin.Eff (Eff, Effects, type (:&), type (:>))
import Bluefin.Algae.DelCont

-- | Algebraic effect.
type AEffect = Type -> Type

-- | Interpretation of an algebraic effect @f@: a function to handle the operations of @f@.
type HandlerBody :: AEffect -> Effects -> Type -> Type
type HandlerBody f ss a = (forall x. f x -> (x -> Eff ss a) -> Eff ss a)

-- | Generalization of 'HandlerBody' with cancellable continuations.
type HandlerBody' :: AEffect -> Effects -> Type -> Type
type HandlerBody' f ss a = (forall ss0 x. f x -> Continuation ss0 ss x a -> Eff ss a)

-- | Handler to call operations of the effect @f@.
type Handler :: AEffect -> Effects -> Type
data Handler f s where
  Handler :: !(PromptTag ss a s) -> HandlerBody' f ss a -> Handler f s

-- | Handle operations of @f@.
--
-- === Warning for exception-like effects
--
-- If the handler might not call the continuation (like for an exception effect), and
-- if the handled computation manages resources (e.g., opening files, transactions)
-- prefer 'handle'' to trigger resource clean up with cancellable continuations.
handle ::
  HandlerBody f ss a ->
  (forall s. Handler f s -> Eff (s :& ss) a) ->
  Eff ss a
handle h = handle' (\f k -> h f (continue k))

-- | Generalization of 'handle' with cancellable continuations.
handle' ::
  HandlerBody' f ss a ->
  (forall s. Handler f s -> Eff (s :& ss) a) ->
  Eff ss a
handle' h act = reset (\p -> act (Handler p h))

-- | Call an operation of @f@.
call :: s :> ss => Handler f s -> f a -> Eff ss a
call (Handler p h) op = shift0 p (\k -> h op k)

-- $cancel
-- Cancellable continuations are useful to work with native exception handlers
-- such as 'Control.Exception.bracket' and other resource-management schemes à
-- la @resourcet@.
--
-- Cancellable continuations should be called exactly once (via 'continue' or 'cancel'):
--
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
-- the simple 'Bluefin.Algae.handle' may cause resource leaks:
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
-- Using 'handle'' instead lets us 'cancel' the continuation.
--
-- @
-- 'handle'' (\\_e k -> 'cancel' k >> pure Nothing)
--   ('Bluefin.Exception.Dynamic.bracket' acquire release (\\_ -> 'call' h Fail))
-- @

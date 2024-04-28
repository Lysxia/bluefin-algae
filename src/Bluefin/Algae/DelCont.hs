{-# LANGUAGE
  BangPatterns,
  MagicHash,
  RankNTypes,
  ScopedTypeVariables,
  StandaloneKindSignatures,
  TypeOperators,
  UnboxedTuples #-}

-- | = Delimited continuations
--
-- Native multi-prompt delimited continuations.
-- These primitives let us manipulate slices of the call stack/evaluation
-- context delimited by 'reset'.
--
-- This module serves as a foundation for algebraic effect handlers,
-- a more structured interface for manipulating continuations and implementing
-- user-defined effects.
--
-- The behavior of 'reset' and 'shift0' is summarized by the following equations:
--
-- @
-- 'reset' (\\_ -> 'pure' x) = 'pure' x
-- 'reset' (\\t -> C ('shift0' t f)) = f (\\x -> 'reset' (\\t -> C x))
-- @
--
-- where @C@ is an evaluation context (in which @t@ may occur), i.e.,
-- a term of the following form:
--
-- > C x ::= C x >>= k      -- for any function k
-- >       | H (\h -> C x)  -- for any handler H ∈ { reset, (`runState` s), ... }
-- >       | x
--
--
-- This module ensures type safety. The rank-2 type of 'reset'
-- guarantees that 'shift0' will always have a maching 'reset' on the stack.
--
-- === References
--
-- - <https://ghc-proposals.readthedocs.io/en/latest/proposals/0313-delimited-continuation-primops.html Delimited continuation primops> (GHC proposal, implemented in GHC 9.6.1).
-- - <https://homes.luddy.indiana.edu/ccshan/recur/recur.pdf Shift to Control> (2004) by Chung-chieh Shan. The name 'shift0' follows the nomenclature in that paper.
module Bluefin.Algae.DelCont
  ( PromptTag
  , reset
  , shift0
  , Continuation
  , weakenC1
  , resume
  , continue
  , cancel
  ) where

import Data.Coerce (coerce)
import Data.Functor (void)
import Data.Kind (Type)
import GHC.Exts (State#, RealWorld, PromptTag#, prompt#, control0#, newPromptTag#)
import GHC.IO (IO(IO))
import Bluefin.Internal (Eff(UnsafeMkEff))
import Bluefin.Eff
import qualified Bluefin.Exception as E

-- | Tag for a prompt of type @Eff ss a@ and scope @s@.
type PromptTag :: Effects -> Type -> Effects -> Type
data PromptTag ss a s = MkPromptTag (PromptTag# a)

-- | Run the enclosed computation under a prompt of type @Eff ss a@.
--
-- @
-- f : forall s. 'PromptTag' ss a s -> 'Eff' (s ':&' ss) a
-- -------------------------------------------------
-- 'reset' (\\t -> f t) : 'Eff' ss a
-- @
--
-- The enclosed computation @f@ is given a tag which identifies that prompt
-- and remembers its type.
-- The scope parameter @s@ prevents the tag from being used outside of the
-- computation.
--
-- A prompt ('reset') delimits a slice of the call stack (or evaluation context),
-- which can be captured with 'shift0'. This slice, a continuation,
-- becomes a function of type @Eff ss0 b -> Eff ss a@ (where @Eff ss0 b@ is the
-- result type of 'shift0' at its calling site).
-- Calling the continuation restores the slice on the stack.
reset :: forall a ss.
  (forall s. PromptTag ss a s -> Eff (s :& ss) a) ->
  Eff ss a
reset f = unsafeMkEff (\z0 -> case newPromptTag# z0 of
    (# z1, tag #) -> prompt# tag (unsafeRunEff (f (MkPromptTag tag))) z1)

-- | Continuations are slices of the call stack, or evaluation context.
--
-- The 'Continuation' type is abstract since not all functions @'Eff' t b -> 'Eff' s a@
-- represent evaluation contexts. In particular, 'weakenC1' is not definable for arbitrary
-- such functions.
--
-- For example, in
--
-- @
-- reset \\tag0 ->
--   reset \\tag1 ->
--     reset \\tag2 ->
--       shift0 tag1 f >>= etc
-- @
--
-- 'shift0' captures a continuation, the slice represented by the following
-- function:
--
-- @
-- MkContinuation \\hole ->
--   reset \\tag1 ->
--     reset \\tag2 ->
--       hole >>= etc
-- @
--
-- That continuation has type @'Continuation' t s b a@ where @Eff t b@ is the type of the hole,
-- and @Eff s a@ is the type of the result once the hole is plugged.
--
-- The second argument of 'shift0', @f@, is applied to the continuation:
--
-- @
-- reset \\tag0 ->
--  f (MkContinuation \\hole ->
--   reset \\tag1 ->
--     reset \\tag2 ->
--       hole >>= etc)
-- @
newtype Continuation t s b a = MkContinuation (Eff t b -> Eff s a)

-- | Extend the context of a continuation.
weakenC1 :: Continuation t s b a -> Continuation (e :& t) (e :& s) b a
weakenC1 = coerce

-- | Resume a continuation with a computation under it.
resume :: Continuation t s b a -> Eff t b -> Eff s a
resume (MkContinuation k) = k

-- | Resume a cancellable continuation with a result.
--
-- In other words, this converts a cancellable continuation to a simple continuation.
continue :: Continuation t s b a -> b -> Eff s a
continue k = resume k . pure

-- | Cancel a continuation: resume by throwing a scoped exception and catch it.
--
-- The continuation SHOULD re-throw unknown exceptions.
-- (That is guaranteed if you don't use "Exception.Dynamic".)
cancel :: Continuation t s b a -> Eff s ()
cancel k = E.catch (\ex -> void (resume (weakenC1 k) (E.throw ex ()))) (\_ -> pure ())

-- | Capture the continuation up to the tagged prompt.
--
-- @
-- _ : s :> ss0
-- t : 'PromptTag' ss a s
-- f : ('Eff' ss0 b -> 'Eff' ss a) -> 'Eff' ss a
-- ---------------------------------------
-- 'shift0' t (\\k -> f k) : 'Eff' ss0 b
-- @
--
-- The prompt ('reset') is reinserted on the stack when the continuation is called:
--
-- @
-- 'reset' (\\t -> C ('shift0' t f)) = f (\\x -> 'reset' (\\t -> C x))
-- @
shift0 :: forall s a b ss ss0.
  s :> ss0 =>
  PromptTag ss a s ->
  (Continuation ss0 ss b a -> Eff ss a) ->
  Eff ss0 b
shift0 (MkPromptTag tag) f = unsafeMkEff (\z0 ->
  control0# tag (\k# ->
    unsafeRunEff (f (unsafeContinuation# (prompt# tag . k#)))) z0)

-- * Internal

type IO# a = State# RealWorld -> (# State# RealWorld , a #)
type Continuation# a b = IO# a -> IO# b

unsafeMkEff :: IO# a -> Eff ss a
unsafeMkEff f = UnsafeMkEff (IO f)

unsafeRunEff :: Eff ss a -> IO# a
unsafeRunEff (UnsafeMkEff (IO f)) = f

unsafeContinuation# :: Continuation# b a -> Continuation t s b a
unsafeContinuation# k = MkContinuation (unsafeMkEff . k . unsafeRunEff)

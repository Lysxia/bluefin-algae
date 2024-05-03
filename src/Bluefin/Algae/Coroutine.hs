{-# LANGUAGE
  BangPatterns,
  DataKinds,
  GADTs,
  KindSignatures,
  RankNTypes,
  ScopedTypeVariables,
  StandaloneKindSignatures,
  TypeOperators #-}

-- | = Coroutines: yield as an algebraic effect
--
-- Coroutines are "cooperative threads", passing control to other coroutines
-- with explicit 'yield' calls.
--
-- Coroutines are an expressive way of defining iterators.
--
-- = References
--
-- Coroutines are also known as generators in Javascript and Python.
--
-- - <https://en.wikipedia.org/wiki/Coroutine Coroutine> and
--   <https://en.wikipedia.org/wiki/Generator_(computer_programming) Generator>
--   on Wikipedia
-- - <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/function*#description Generators in Javascript>
-- - <https://docs.python.org/3/reference/expressions.html#yieldexpr Generators in Python>
module Bluefin.Algae.Coroutine
  ( -- * Operations
    Coroutine(..)
  , yield

    -- * Handlers
  , withCoroutine
  , forCoroutine

    -- * Transducers and pipes
    -- ** Transducers
  , Transducer(..)
  , CoTransducer
  , next
  , simpleTransducer
  , mapTransducer
  , voidTransducer
  , eitherTransducer
  , loopTransducer

    -- ** Pipes
  , Pipe(..)
  , PipeEvent(..)
  , CoPipe
  , mapPipe
  , stepPipe
  , runPipe
  , forPipe
  , eitherPipe
  , loopPipe

    -- ** Handlers involving transducers and pipes

    -- | Using the handlers 'toTransducer' and 'toPipe' as primitives,
    -- we can define the other handlers.
    --
    -- @
    -- 'withCoroutine' g f = 'runPipe' ('toTransducer' g) ('toPipe' f)
    -- 'forCoroutine' g f = 'runPipe' ('simpleTransducer' g) ('toPipe' f)
    -- 'withTransducer' g f = 'runPipe' g ('toPipe' f)
    -- @
  , TransducerSEff
  , toTransducer
  , PipeSEff
  , toPipe
  , withTransducer

    -- ** Interpreting transducers and pipes as coroutines
  , TransducerEff
  , fromTransducer
  , PipeEff
  , fromPipe
  ) where

import Data.Function (fix)
import Data.Bifunctor (bimap)
import Data.Void (Void, absurd)
import Bluefin.Eff
import Bluefin.Algae

-- * Coroutines

-- | Coroutine effect with outputs @o@ and inputs @i@.
data Coroutine o i a where
  -- | Yield an output and wait for an input.
  Yield :: o -> Coroutine o i i

-- | Call the 'Yield' operation.
yield :: z :> zz => Handler (Coroutine o i) z -> o -> Eff zz i
yield h o = call h (Yield o)

-- * Transducers

-- | A 'Transducer' yields an @o@ for every @i@ you feed it.
--
-- Its interactions are described by the following diagram:
--
-- @
-- +------------------+              +--------------------+
-- | 'Transducer' o i m | (input i)--> | 'CoTransducer' o i m |
-- |                  | <-(output o) |                    |
-- +------------------+              +--------------------+
-- @
--
-- @'Transducer' i o m@ is equivalent to @'CoPipe' i o m Void@.
newtype Transducer i o m = MkTransducer (i -> CoTransducer i o m)

-- | Intermediate state of a 'Transducer' after receiving an input @i@.
type CoTransducer i o m = m (o, Transducer i o m)

-- | Apply a transducer to yield the next output and transducer state.
next :: Transducer i o m -> i -> m (o, Transducer i o m)
next (MkTransducer f) = f

-- | A transducer which runs the same function on every input.
simpleTransducer :: Functor m => (i -> m o) -> Transducer i o m
simpleTransducer f = fix $ \self -> MkTransducer (\i -> (\o -> (o, self)) <$> f i)

-- | Transform the input and output of a transducer.
mapTransducer :: Functor m =>
  (i' -> i) ->
  (o -> o') ->
  Transducer i o m ->
  Transducer i' o' m
mapTransducer fi fo t = loop t
  where
    loop (MkTransducer u) = MkTransducer (fmap (bimap fo loop) . u . fi)

-- | Sum of transducers with the same output type, branching on the input type.
eitherTransducer :: Functor m =>
  (i -> Either i1 i2) ->   -- ^ Dispatch input
  Transducer i1 o m ->     -- ^ Left transducer
  Transducer i2 o m ->     -- ^ Right transducer
  Transducer i o m
eitherTransducer split = loop
  where
    loop t1 t2 = MkTransducer (transduce_ t1 t2 . split)
    (<<&>>) = flip (fmap . fmap)
    transduce_ t1 t2 (Left i1) = next t1 i1 <<&>> \t1' -> loop t1' t2
    transduce_ t1 t2 (Right i2) = next t2 i2 <<&>> \t2' -> loop t1 t2'

-- | Transducer with no input.
voidTransducer :: Transducer Void o m
voidTransducer = MkTransducer absurd

loopTransducer :: Monad m => Transducer o o m -> o -> m void
loopTransducer t0 o0 = loop (o0, t0)
  where
    loop (o, t) = next t o >>= loop

-- | Representation of 'Transducer' as scoped 'Eff' computations.
type TransducerSEff i o zz = forall void. i -> ScopedEff (Coroutine o i) zz void

-- | Representation of 'Transducer' as 'Eff' computations.
type TransducerEff i o zz = forall void z. z :> zz => i -> Handler (Coroutine o i) z -> Eff zz void

-- * Pipes

-- | A 'Pipe' represents a coroutine as a tree: a 'Pipe' yields an output @o@ and
-- waits for an input @i@, or terminates with a result @a@.
--
-- @
-- +--------------+                  +----------------+
-- | 'Pipe' i o m a | ('Yielding' o)---> | 'CoPipe' i o m a |
-- |              | <------(input i) |                |
-- +--------------+                  +----------------+
--        v ('Done')
--      +---+
--      | a |
--      +---+
-- @
newtype Pipe i o m a = MkPipe (m (PipeEvent i o m a))

-- | Events of 'Pipe'.
data PipeEvent i o m a
  = Done a
  | Yielding o (CoPipe i o m a)

-- | Intermediate state of a 'Pipe' after yielding an output @o@.
type CoPipe i o m a = i -> m (PipeEvent i o m a)

-- | Unwrap 'Pipe'.
stepPipe :: Pipe i o m a -> m (PipeEvent i o m a)
stepPipe (MkPipe m) = m

-- | Transform inputs and outputs of a 'Pipe'.
mapPipe :: Functor m => (i' -> i) -> (o -> o') -> (a -> a') -> Pipe i o m a -> Pipe i' o' m a'
mapPipe fi fo fa (MkPipe m) = MkPipe (loop <$> m)
  where
    loop (Done a) = Done (fa a)
    loop (Yielding o k) = Yielding (fo o) (fmap loop . k . fi)

-- | Run a 'Pipe' with a transducer to respond to every event.
runPipe :: Monad m => Transducer i o m -> Pipe o i m a -> m a
runPipe t0 (MkPipe p) = p >>= loop t0
  where
    loop _ (Done a) = pure a
    loop t (Yielding i k) = do
      (o, t') <- next t i
      k o >>= loop t'

-- | Iterate through a 'Pipe'. Respond to every 'Yielding' event by running the loop body.
-- Return the final result of the 'Pipe'.
--
-- @
-- 'forPipe' p g = 'runPipe' ('simpleTransducer' g) p
-- @
forPipe :: Monad m =>
  Pipe i o m a ->  -- ^ Iterator
  (o -> m i) ->    -- ^ Loop body
  m a
forPipe (MkPipe m) h = m >>= loop
  where
    loop (Done a) = pure a
    loop (Yielding o k) = h o >>= \i -> k i >>= loop

-- | Sum a transducer and a pipe with the same output type,
-- branching on the input type.
eitherPipe :: Monad m =>
  (i -> Either i1 i2) ->   -- ^ Dispatch input
  Transducer i1 o m ->     -- ^ Left input transducer
  Pipe i2 o m a ->         -- ^ Right input pipe
  Pipe i o m a
eitherPipe split t0 (MkPipe p) = MkPipe (p >>= loop t0)
  where
    (<&>) = flip fmap
    loop _ (Done a) = pure (Done a)
    loop t (Yielding o k) = pure (Yielding o (switch t k . split))
    switch t k (Left i1) = next t i1 <&> \(o, t') -> Yielding o (switch t' k . split)
    switch t k (Right i2) = k i2 >>= loop t

-- | Loop the output of a pipe back to its input.
loopPipe :: Monad m => Pipe o o m a -> m a
loopPipe (MkPipe p) = p >>= loop
  where
    loop (Done a) = pure a
    loop (Yielding o k) = k o >>= loop

-- | Representation of 'Pipe' as scoped 'Eff' computations.
type PipeSEff i o zz a = ScopedEff (Coroutine o i) zz a

-- | Representation of 'Pipe' as 'Eff' computations.
type PipeEff i o zz a = forall z. z :> zz => Handler (Coroutine o i) z -> Eff zz a

-- * Handlers

-- | Convert a coroutine that doesn't return into a 'Transducer'.
toTransducer :: forall o i zz.
  TransducerSEff i o zz -> Transducer i o (Eff zz)
toTransducer f = MkTransducer (\o -> handle coroutineHandler (f o))
  where
    coroutineHandler :: HandlerBody (Coroutine o i) zz (o, Transducer i o (Eff zz))
    coroutineHandler (Yield i) k = pure (i, MkTransducer k)

-- | Convert a 'Transducer' into a coroutine.
fromTransducer :: Transducer i o (Eff zz) -> TransducerEff i o zz
fromTransducer t0 i0 h = loop t0 i0 where
  loop t i = next t i >>= \(o, t') -> yield h o >>= loop t'

-- | Evaluate a coroutine into a 'Pipe'.
toPipe :: forall o i a zz.
  PipeSEff i o zz a ->
  Pipe i o (Eff zz) a
toPipe f = MkPipe (handle coroutineHandler (wrap . f))
  where
    coroutineHandler :: HandlerBody (Coroutine o i) zz (PipeEvent i o (Eff zz) a)
    coroutineHandler (Yield o) k = pure (Yielding o k)

    wrap :: Eff (z :& zz) a -> Eff (z :& zz) (PipeEvent i o (Eff zz) a)
    wrap = fmap Done

-- | Convet a 'Pipe' into a coroutine.
fromPipe :: Pipe i o (Eff zz) a -> PipeEff i o zz a
fromPipe (MkPipe p) h = p >>= loop where
  loop (Done a) = pure a
  loop (Yielding o k) = yield h o >>= \i -> k i >>= loop

-- | Interleave the execution of a transducer and a coroutine.
withTransducer :: forall o i a zz.
  Transducer i o (Eff zz) ->
  ScopedEff (Coroutine i o) zz a ->
  Eff zz a
withTransducer g f = with g (handle coroutineHandler (fmap wrap . f))
  where
    coroutineHandler :: HandlerBody (Coroutine i o) zz (Transducer i o (Eff zz) -> Eff zz a)
    coroutineHandler (Yield o) k = pure $ \g1 -> do
      (i, g2) <- next g1 o
      with g2 (k i)

    wrap :: a -> z -> Eff zz a
    wrap a _ = pure a

    with :: forall g. g -> Eff zz (g -> Eff zz a) -> Eff zz a
    with g' m = m >>= \f' -> f' g'

-- | Interleave the execution of two coroutines, feeding each one's output to the other's input.
-- Return the result of the main thread.
--
-- The secondary thread cannot return (it can terminate by throwing an exception).
withCoroutine :: forall o i a zz.
  (forall void. i -> ScopedEff (Coroutine o i) zz void) ->  -- ^ Secondary thread
  ScopedEff (Coroutine i o) zz a ->                         -- ^ Main thread
  Eff zz a
withCoroutine g f = withTransducer (toTransducer g) f

-- | Iterate through a coroutine:
-- execute the loop body @o -> Eff zz i@ for every call to 'Yield' in the coroutine.
forCoroutine :: forall o i a zz.
  ScopedEff (Coroutine o i) zz a ->  -- ^ Iterator
  (o -> Eff zz i) ->  -- ^ Loop body
  Eff zz a
forCoroutine f h = handle coroutineHandler f
  where
    coroutineHandler :: HandlerBody (Coroutine o i) zz a
    coroutineHandler (Yield o) k = h o >>= k

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

    -- * Pipes
    -- ** Definition
  , Pipe(..)
  , PipeEvent(..)
  , CoPipe(..)

    -- ** Unwrap
  , stepPipe
  , applyCoPipe
  , next

    -- ** Constructors
  , simpleCoPipe
  , voidCoPipe
  , nothingPipe
  , nothingCoPipe

    -- ** Pipe combinators
  , mapPipe
  , mapCoPipe
  , eitherPipe
  , eitherCoPipe
  , openPipe
  , openCoPipe

    -- ** Destructors
  , runPipe
  , runCoPipe
  , forPipe
  , forCoPipe
  , loopPipe
  , loopCoPipe

    -- ** Handlers involving pipes

    -- | Using the handlers 'toCoPipe' and 'toPipe' as primitives,
    -- we can define the other handlers.
    --
    -- @
    -- 'withCoroutine' g f = 'runPipe' ('toCoPipe' g) ('toPipe' f)
    -- 'forCoroutine' g f = 'runPipe' ('simpleCoPipe' g) ('toPipe' f)
    -- 'withCoPipe' g f = 'runPipe' g ('toPipe' f)
    -- @
  , CoPipeSEff
  , toCoPipe
  , PipeSEff
  , toPipe
  , withCoPipe

    -- ** Interpreting pipes as coroutines
  , CoPipeEff
  , fromCoPipe
  , PipeEff
  , fromPipe
  ) where

import Data.Coerce (coerce)
import Data.Function (fix)
import Data.Functor ((<&>))
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

-- * Pipes

-- | Output-first coroutine.
--
-- A 'Pipe' represents a coroutine as a tree: a 'Pipe' yields an output @o@ and
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

-- | Input-first coroutine. 'Pipe' continuation.
newtype CoPipe i o m a = MkCoPipe (i -> Pipe i o m a)

-- | Unwrap 'Pipe'.
stepPipe :: Pipe i o m a -> m (PipeEvent i o m a)
stepPipe (MkPipe p) = p

-- | Unwrap 'CoPipe'.
applyCoPipe :: CoPipe i o m a -> i -> Pipe i o m a
applyCoPipe (MkCoPipe k) = k

-- | Apply a non-returning 'CoPipe' to yield the next output and 'CoPipe' state.
next :: Functor m => CoPipe i o m Void -> i -> m (o, CoPipe i o m Void)
next (MkCoPipe f) i = go <$> stepPipe (f i) where
  go (Done v) = absurd v
  go (Yielding o k) = (o, k)

-- | A 'CoPipe' which runs the same function on every input.
simpleCoPipe :: Functor m => (i -> m o) -> CoPipe i o m void
simpleCoPipe f = fix $ \self -> MkCoPipe (\i -> MkPipe ((\o -> Yielding o self) <$> f i))

-- | Transform inputs and outputs of a 'Pipe'.
mapPipe :: Functor m => (i' -> i) -> (o -> o') -> (a -> a') -> Pipe i o m a -> Pipe i' o' m a'
mapPipe fi fo fa = mapPipe_
  where
    mapPipe_ (MkPipe p) = MkPipe (loop <$> p)
    loop (Done a) = Done (fa a)
    loop (Yielding o k) = Yielding (fo o) (mapCoPipe_ k)
    mapCoPipe_ (MkCoPipe k) = MkCoPipe (mapPipe_ . k . fi)

-- | Transform the input and output of a 'CoPipe'.
mapCoPipe :: Functor m => (i' -> i) -> (o -> o') -> (a -> a') -> CoPipe i o m a -> CoPipe i' o' m a'
mapCoPipe fi fo fa (MkCoPipe k) = MkCoPipe (mapPipe fi fo fa . k . fi)

-- | Run a 'Pipe' with a 'CoPipe' to respond to every output.
runPipe :: Monad m => CoPipe i o m Void -> Pipe o i m a -> m a
runPipe t (MkPipe p) = p >>= \e -> case e of
  Done a -> pure a
  Yielding i k -> do
    (o, t') <- next t i
    runCoPipe t' k o

-- | Run a 'CoPipe' with another 'CoPipe' to respond to every input.
runCoPipe :: Monad m => CoPipe i o m Void -> CoPipe o i m a -> o -> m a
runCoPipe t (MkCoPipe k) i = runPipe t (k i)

-- | Iterate through a 'Pipe'. Respond to every 'Yielding' event by running the loop body.
-- Return the final result of the 'Pipe'.
--
-- @
-- 'forPipe' p g = 'runPipe' ('simpleCoPipe' g) p
-- @
forPipe :: Monad m =>
  Pipe i o m a ->  -- ^ Iterator
  (o -> m i) ->    -- ^ Loop body
  m a
forPipe p h = stepPipe p >>= loop
  where
    loop (Done a) = pure a
    loop (Yielding o k) = h o >>= \i -> stepPipe (applyCoPipe k i) >>= loop

-- | Iterate through a 'CoPipe'.
forCoPipe :: Monad m =>
  CoPipe i o m a ->
  (o -> m i) ->
  i -> m a
forCoPipe (MkCoPipe k) h i = forPipe (k i) h

-- | 'CoPipe' with no input.
voidCoPipe :: CoPipe Void o m a
voidCoPipe = MkCoPipe absurd

-- | Sum a copipe and a pipe with the same output type,
-- branching on the input type.
eitherPipe :: Monad m =>
  (i -> Either i1 i2) ->   -- ^ Dispatch input
  CoPipe i1 o m Void ->    -- ^ Left copipe
  Pipe i2 o m a ->         -- ^ Right pipe
  Pipe i o m a
eitherPipe split t0 (MkPipe p) = MkPipe $ p <&> \e -> case e of
  Done a -> Done a
  Yielding o k -> Yielding o (eitherCoPipe split t0 k)

-- | Sum two copipes with the same output type, branching on the input type.
eitherCoPipe :: Functor m =>
  (i -> Either i1 i2) ->   -- ^ Dispatch input
  CoPipe i1 o m Void ->    -- ^ Left copipe
  CoPipe i2 o m a ->       -- ^ Right copipe
  CoPipe i o m a
eitherCoPipe split = loop
  where
    loop t1 t2 = MkCoPipe (MkPipe . transduce_ t1 t2 . split)
    transduce_ t1 t2 (Left i1) = next t1 i1 <&> \(o, t1') -> Yielding o (loop t1' t2)
    transduce_ t1 (MkCoPipe t2) (Right i2) = stepPipe (t2 i2) <&> \e -> case e of
      Done a -> Done a
      Yielding o t2' -> Yielding  o (loop t1 t2')

-- | Loop the output of a pipe back to its input.
loopPipe :: Monad m => Pipe o o m a -> m a
loopPipe (MkPipe p) = p >>= \e -> case e of
  Done a -> pure a
  Yielding o k -> loopCoPipe k o

-- | Forward the output of a 'CoPipe' to its input.
loopCoPipe :: Monad m => CoPipe o o m a -> o -> m a
loopCoPipe (MkCoPipe k) o = loopPipe (k o)

-- | Convert a returning 'Pipe' into a non-returning 'CoPipe',
-- yielding 'Nothing' forever once the end has been reached.
openPipe :: Applicative m => Pipe i o m () -> Pipe i (Maybe o) m void
openPipe (MkPipe p) = MkPipe (p <&> \e -> case e of
  Done _ -> Yielding Nothing nothingCoPipe
  Yielding o k -> Yielding (Just o) (openCoPipe k))

-- | Convert a returning 'CoPipe' into a non-returning 'CoPipe',
-- yielding 'Nothing' forever once the end has been reached.
openCoPipe :: Applicative m => CoPipe i o m () -> CoPipe i (Maybe o) m void
openCoPipe (MkCoPipe k) = MkCoPipe (openPipe . k)

-- | Yield 'Nothing' forever.
nothingPipe :: Applicative m => Pipe i (Maybe o) m void
nothingPipe = MkPipe (pure (Yielding Nothing nothingCoPipe))

-- | Yield 'Nothing' forever.
nothingCoPipe :: Applicative m => CoPipe i (Maybe o) m void
nothingCoPipe = MkCoPipe (\_ -> nothingPipe)

-- | Representation of 'Pipe' as scoped 'Eff' computations.
type PipeSEff i o zz a = ScopedEff (Coroutine o i) zz a

-- | Representation of 'Pipe' as 'Eff' computations.
type PipeEff i o zz a = forall z. z :> zz => Handler (Coroutine o i) z -> Eff zz a

-- | Representation of 'CoPipe' as scoped 'Eff' computations.
type CoPipeSEff i o zz a = i -> ScopedEff (Coroutine o i) zz a

-- | Representation of 'CoPipe' as 'Eff' computations.
type CoPipeEff i o zz a = forall z. z :> zz => i -> Handler (Coroutine o i) z -> Eff zz a


-- * Handlers

-- | Convert a coroutine that doesn't return into a 'CoPipe'.
toCoPipe :: forall o i a zz.
  CoPipeSEff i o zz a -> CoPipe i o (Eff zz) a
toCoPipe f = MkCoPipe (\i -> toPipe (\h -> f i h))

-- | Convert a 'CoPipe' into a coroutine.
fromCoPipe :: CoPipe i o (Eff zz) a -> CoPipeEff i o zz a
fromCoPipe (MkCoPipe k) i h = fromPipe (k i) h

-- | Evaluate a coroutine into a 'Pipe'.
toPipe :: forall o i a zz.
  PipeSEff i o zz a ->
  Pipe i o (Eff zz) a
toPipe f = MkPipe (handle coroutineHandler (wrap . f))
  where
    coroutineHandler :: HandlerBody (Coroutine o i) zz (PipeEvent i o (Eff zz) a)
    coroutineHandler (Yield o) k = pure (Yielding o (coerce k))

    wrap :: Eff (z :& zz) a -> Eff (z :& zz) (PipeEvent i o (Eff zz) a)
    wrap = fmap Done

-- | Convet a 'Pipe' into a coroutine.
fromPipe :: Pipe i o (Eff zz) a -> PipeEff i o zz a
fromPipe (MkPipe p) h = p >>= \e -> case e of
  Done a -> pure a
  Yielding o k -> yield h o >>= \i -> fromCoPipe k i h

-- | Interleave the execution of a copipe and a coroutine.
withCoPipe :: forall o i a zz.
  CoPipe i o (Eff zz) Void ->
  ScopedEff (Coroutine i o) zz a ->  -- ^ Main coroutine
  Eff zz a
withCoPipe g f = with g (handle coroutineHandler (fmap wrap . f))
  where
    coroutineHandler :: HandlerBody (Coroutine i o) zz (CoPipe i o (Eff zz) Void -> Eff zz a)
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
  (i -> ScopedEff (Coroutine o i) zz Void) ->  -- ^ Secondary thread
  ScopedEff (Coroutine i o) zz a ->            -- ^ Main thread
  Eff zz a
withCoroutine g f = withCoPipe (toCoPipe g) f

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

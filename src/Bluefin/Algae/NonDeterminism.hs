{-# LANGUAGE
  BangPatterns,
  GADTs,
  KindSignatures,
  RankNTypes,
  ScopedTypeVariables,
  StandaloneKindSignatures,
  TypeOperators #-}

-- | Nondeterministic choice as an algebraic effect.
--
-- === Warning: Non-linear continuations
--
-- The handlers 'forAllChoices' and 'toList' call continuations zero or twice.
-- Don't use them to handle a computation that must ensure linear usage of resources.
module Bluefin.Algae.NonDeterminism
  ( -- * Operations
    Choice(..)
  , choose
  , nil
  , assume
  , pick
  , removeFrom
    -- * Handlers
  , forAllChoices
  , toList
  , foldChoice
  ) where

import Control.Monad ((>=>), join)
import Bluefin.Internal (insertFirst)
import Bluefin.Eff (Eff, type (:&), type (:>))
import Bluefin.Algae

-- | Choice effect.
data Choice :: AEffect where
  -- | Choose one of two alternatives.
  Choose :: a -> a -> Choice a
  -- | No choice.
  Nil :: Choice a

-- | Choose one of two alternatives. Call the 'Choose' operation.
choose :: z :> zz => Handler Choice z -> a -> a -> Eff zz a
choose h x y = call h (Choose x y)

-- | No choice. Call the 'Nil' operation.
nil :: z :> zz => Handler Choice z -> Eff zz a
nil h = call h Nil

-- | Do nothing if @True@. Discard (call 'nil') if @False@.
assume :: z :> zz => Handler Choice z -> Bool -> Eff zz ()
assume _ True = pure ()
assume h False = nil h

-- | Pick an element in a list.
pick :: z :> zz => Handler Choice z -> [a] -> Eff zz a
pick h [] = nil h
pick h (x : xs) = join $ choose h (pure x) (pick h xs)

-- | Remove an element from a list, returning the resulting list as well.
-- The order of elements is preserved.
removeFrom :: z :> zz => Handler Choice z -> [a] -> Eff zz (a, [a])
removeFrom h = loop []
  where
    loop _ [] = nil h
    loop ys (x : xs) = join $ choose h (pure (x, reverse ys ++ xs)) (loop (x : ys) xs)

-- | Apply a function to every result of the nondeterministic computation.
forAllChoices :: forall a zz.
  (forall z. Handler Choice z -> Eff (z :& zz) a) ->
  (a -> Eff zz ()) ->
  Eff zz ()
forAllChoices f h = foldChoice h (pure ()) (>>) f

-- | Collect the results of a nondeterministic computation in a list.
toList :: forall a zz.
  (forall z. Handler Choice z -> Eff (z :& zz) a) ->
  Eff zz [a]
toList f = unwrap (foldChoice (pure . (:)) (pure id) (liftA2 (.)) f)
  where
    unwrap :: Eff zz ([a] -> [a]) -> Eff zz [a]
    unwrap = fmap ($ [])

-- | Generic 'Choice' handler parameterized by a monoid.
foldChoice :: forall a r zz.
  (a -> Eff zz r) ->           -- ^ Injection
  Eff zz r ->                            -- ^ Identity element
  (Eff zz r -> Eff zz r -> Eff zz r) ->  -- ^ Binary operation
  ScopedEff Choice zz a ->
  Eff zz r
foldChoice oneE nilE appendE f = handle choiceHandler (f >=> insertFirst . oneE)
  where
    choiceHandler :: HandlerBody Choice zz r
    choiceHandler (Choose x y) k = appendE (k x) (k y)
    choiceHandler Nil _k = nilE

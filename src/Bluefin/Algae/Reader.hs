{-# LANGUAGE
  DataKinds,
  GADTs,
  KindSignatures,
  RankNTypes,
  ScopedTypeVariables,
  TypeOperators #-}
-- | = Reader as an algebraic effect
--
-- Access to a read-only environment.
module Bluefin.Algae.Reader
  ( -- * Operation
    Reader(..)
  , ask

    -- * Handler
  , runReader
  ) where

import Data.Kind (Type)
import Bluefin.Algae
import Bluefin.Eff (Eff, type (:>), type (:&))

-- | The reader effect.
data Reader (a :: Type) :: AEffect where
  -- | Ask for a value.
  Ask :: Reader a a

-- | Ask for a value. Call the 'Ask' operation.
ask :: s :> ss => Handler (Reader a) s -> Eff ss a
ask h = call h Ask

-- | Answer 'Ask' operations of the handled computation with a fixed value.
runReader :: forall a b ss.
  a -> (forall s. Handler (Reader a) s -> Eff (s :& ss) b) -> Eff ss b
runReader a = handle readerHandler
  where
    readerHandler :: Reader a r -> (r -> Eff ss b) -> Eff ss b
    readerHandler Ask k = k a

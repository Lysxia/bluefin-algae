{-# LANGUAGE
  KindSignatures,
  ScopedTypeVariables,
  TypeOperators #-}
-- | = Dynamic exceptions
--
-- This is the vanilla exception mechanism from @IO@.
-- Use this module to handle exceptions from external (non-bluefin) APIs.
--
-- Another motivation is to serve as a principled (experimental) framework
-- for resource management with 'bracket'.
--
-- The core Bluefin API exposes a 'Bluefin.Eff.bracket' in "Bluefin.Eff"
-- which (intentionally) weakens the scoping of scoped exceptions in
-- "Bluefin.Exception".
--
-- This module is an experiment for a world where
--
-- - scoped exceptions are truly scoped (unlike "Bluefin.Exception");
-- - the capability to catch and throw dynamic exceptions is explicit
--   (unlike 'Bluefin.Eff.bracket' in "Bluefin.Eff").
module Bluefin.Exception.Dynamic
  ( DynExn
  , ioeToDynExn
  , throw
  , catch
  , bracket
  , finally
  , onException
  , throwIO
  , catchIO
  ) where

import qualified Control.Exception as E
import qualified Bluefin.Internal as B
import Bluefin.Eff (Eff, Effects, type (:>))
import Bluefin.IO (IOE)

-- | Capability to catch and throw dynamic exceptions.
data DynExn (ex :: Effects) = UnsafeDynExn

-- | Refine an 'IOE' capability to a 'DynExn'.
ioeToDynExn :: IOE io -> DynExn io
ioeToDynExn _ = UnsafeDynExn

-- | Throw an exception.
throw :: (E.Exception e, ex :> es) => DynExn ex -> e -> Eff es a
throw _ e = B.UnsafeMkEff (E.throwIO e)

-- | Catch an exception.
catch :: (E.Exception e, ex :> es) => DynExn ex -> Eff es a -> (e -> Eff es a) -> Eff es a
catch _ m h = B.UnsafeMkEff (E.catch (B.unsafeUnEff m) (B.unsafeUnEff . h))

-- | @'bracket' ex acquire release run@: @acquire@ a resource, @run@ a computation depending on it,
-- and finally @relase@ the resource even if @run@ threw an exception.
bracket :: ex :> es => DynExn ex -> Eff es a -> (a -> Eff es ()) -> (a -> Eff es b) -> Eff es b
bracket ex acquire release run = do
  a <- acquire
  finally ex (run a) (release a)

-- | @'finally' ex run cleanup@: @run@ a computation, then @cleanup@ even if
-- @run@ threw an exception.
finally :: ex :> es => DynExn ex -> Eff es a -> Eff es () -> Eff es a
finally ex run cleanup =
  onException ex run cleanup   -- if run throws an exception, then only this cleanup will run
    <* cleanup                 -- if run does not throw, then only this cleanup will run

-- | @'onException' ex run cleanup@: @run@ a computation, and if an exception is thrown,
-- @cleanup@, then rethrow the exception.
onException :: ex :> es => DynExn ex -> Eff es a -> Eff es () -> Eff es a
onException ex run cleanup = catch ex run (\(e :: E.SomeException) -> cleanup >> throw ex e)

-- | 'throw' with an 'IOE' capability.
throwIO :: (E.Exception e, io :> es) => IOE io -> e -> Eff es a
throwIO io = throw (ioeToDynExn io)

-- | 'catch' with an 'IOE' capability.
catchIO :: (E.Exception e, io :> es) => IOE io -> Eff es a -> (e -> Eff es a) -> Eff es a
catchIO io = catch (ioeToDynExn io)

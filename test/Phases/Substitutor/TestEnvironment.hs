{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Phases.Substitutor.TestEnvironment (
    module Data.Error,
    module Monads.Error,
    module Monads.VarReader,
    TestEnvironment (runTestEnvironment),
  ) where

import           Data.Error
import           Data.Variable
import           Monads.Error
import           Monads.VarReader

import qualified Control.Monad.Except as E

modError :: String -> Error
modError = Error "Tests"

throwModError :: MonadError m => String -> m a
throwModError = throwError . modError

newtype TestEnvironment a = TestEnvironment {runTestEnvironment :: Either Error a}
  deriving (Functor, Applicative, Monad, E.MonadError Error, MonadError)

instance MonadPathReader TestEnvironment where
  getVarPath = throwModError "Inaccessible mock method"

instance MonadPwdReader TestEnvironment where
  getVarPwd = throwModError "Inaccessible mock method"

instance MonadVarReader TestEnvironment where
  getVar var
    | Right var == variable "mockX" = pure "value X"
    | Right var == variable "mockY" = pure "va'lu'eY"
    | Right var == variable "mockZ" = pure "\"v a l u e Z\""
    | otherwise = pure ""
  getVars = throwModError "Inaccessible mock method"

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Phases.Linker.TestEnvironment (
    module Environment.MonadPwdReader,
    TestEnvironment,
    runTestEnvironment,
  ) where

import Environment.FSPrimitive
import Environment.MonadPwdReader

newtype TestEnvironment a = TestEnvironment (AbsFilePath -> a)
  deriving (Functor, Applicative, Monad)

instance MonadPwdReader TestEnvironment where
  getVarPwd = TestEnvironment id

runTestEnvironment :: AbsFilePath -> TestEnvironment a -> a
runTestEnvironment pwd (TestEnvironment func) = func pwd

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Phases.Linker.TestEnvironment (
    module Monads.FS,
    module Monads.PwdReader,
    module Monads.SelfReferenced,
    TestEnvironment,
    runTestEnvironment,
  ) where

import Data.FSObjects
import Monads.FS
import Monads.PwdReader
import Monads.SelfReferenced

newtype TestEnvironment a = TestEnvironment (AbsFilePath -> a)
  deriving (Functor, Applicative, Monad)

instance MonadPwdReader TestEnvironment where
  getVarPwd = TestEnvironment id

instance MonadFS TestEnvironment where
  findFileByAbsPath = undefined

instance MonadSelfReferenced TestEnvironment where
  getSelfPath = TestEnvironment . const . either undefined id $ absFilePath "/home/user/cli-exe"

runTestEnvironment :: AbsFilePath -> TestEnvironment a -> a
runTestEnvironment pwd (TestEnvironment func) = func pwd

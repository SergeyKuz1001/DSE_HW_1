{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Phases.Analyzer.TestEnvironment (
    module Environment.FSPrimitive,
    module Environment.MonadError,
    module Environment.MonadFS,
    module Environment.MonadPathReader,
    module Environment.MonadPwdReader,
    TestEnvironment,
    runTestEnvironment,
  ) where

import Environment.FSPrimitive
import Environment.MonadError
import Environment.MonadFS
import Environment.MonadPathReader
import Environment.MonadPwdReader

import qualified Control.Monad.Except as ME
import Control.Monad.State (StateT, gets, runStateT)
import Data.List (find)
import Prelude hiding (putStr, putStrLn, getLine)

data TestEnvironment a

runTestEnvironment :: AbsFilePath -> [File] -> TestEnvironment a -> Either Error a
runTestEnvironment = undefined

{-data GlobalState = GlobalState
  { pwd   :: AbsFilePath
  , files :: [File]
  }

newtype TestEnvironment a = TestEnvironment (StateT GlobalState (Either Error) a)
  deriving (Functor, Applicative, Monad, ME.MonadError Error, MonadError)

instance MonadVarPathReader TestEnvironment where
  getVarPath = return []

instance MonadVarPwdReader TestEnvironment where
  getVarPwd = TestEnvironment $ gets pwd

instance MonadFS TestEnvironment where
  findFileByAbsPath absPath = TestEnvironment . gets $
    \gs ->
      let allFiles = files gs
      in  find ((absPath ==) . filePath) allFiles

runTestEnvironment :: AbsFilePath -> [File] -> TestEnvironment a -> Either Error a
runTestEnvironment pwd_ files_ (TestEnvironment m) = fst <$> runStateT m (GlobalState pwd_ files_)-}

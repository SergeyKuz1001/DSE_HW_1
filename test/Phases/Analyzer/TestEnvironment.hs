{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Phases.Analyzer.TestEnvironment (
    module Data.Error,
    module Data.FSObjects,
    module Monads.Error,
    module Monads.FS,
    module Monads.PathReader,
    module Monads.PwdReader,
    TestEnvironment,
    runTestEnvironment,
  ) where

import Data.Error
import Data.FSObjects
import Monads.Error
import Monads.FS
import Monads.PathReader
import Monads.PwdReader

import qualified Control.Monad.Except as ME
import Control.Monad.State (StateT, gets, runStateT)
import Prelude hiding (putStr, putStrLn, getLine)

data GlobalState = GlobalState
  { pwd   :: AbsFilePath
  , path  :: [AbsFilePath]
  , files :: [File]
  }

newtype TestEnvironment a = TestEnvironment (StateT GlobalState (Either Error) a)
  deriving (Functor, Applicative, Monad, ME.MonadError Error, MonadError)

instance MonadPathReader TestEnvironment where
  getVarPath = TestEnvironment $ gets path

instance MonadPwdReader TestEnvironment where
  getVarPwd = TestEnvironment $ gets pwd

instance MonadFS TestEnvironment where
  findFileByAbsPath absPath = TestEnvironment . gets $
    \gs ->
      let allFiles = files gs
          allFilesWithPaths = (\file -> (asFilePath $ filePath file, file)) <$> allFiles
      in  lookup (asFilePath absPath) allFilesWithPaths

runTestEnvironment :: AbsFilePath -> [AbsFilePath] -> [File] -> TestEnvironment a -> Either Error a
runTestEnvironment pwd_ path_ files_ (TestEnvironment m) = fst <$> runStateT m (GlobalState pwd_ path_ files_)

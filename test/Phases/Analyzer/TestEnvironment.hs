{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Phases.Analyzer.TestEnvironment (
    module Environment.MonadError,
    module Environment.MonadFS,
    module Environment.MonadVarPathReader,
    TestEnvironment,
    runTestEnvironment,
  ) where

import Environment.MonadError
import Environment.MonadFS
import Environment.MonadVarPathReader

import qualified Control.Monad.Except as ME
import Control.Monad.State (StateT, gets, runStateT)
import Data.List (find)
import Prelude hiding (putStr, putStrLn, getLine)

data GlobalState = GlobalState
  { pwd   :: AbsFilePath
  , files :: [File]
  }

newtype TestEnvironment a = TestEnvironment (StateT GlobalState (Either Error) a)
  deriving (Functor, Applicative, Monad, ME.MonadError Error, MonadError)

instance MonadVarPathReader TestEnvironment where
  getVarPath = return []

instance MonadFS TestEnvironment where
  findFile filePath = TestEnvironment . gets $
    \gs ->
      let allFiles = files gs
          filePath' =
            if filePath == ""
              then ""
              else if head filePath == '/'
                then filePath
                else asFilePath (pwd gs) ++ filePath
      in  find ((filePath' ==) . asFilePath . absFilePath) allFiles

runTestEnvironment :: AbsFilePath -> [File] -> TestEnvironment a -> Either Error a
runTestEnvironment pwd_ files_ (TestEnvironment m) = fst <$> runStateT m (GlobalState pwd_ files_)

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TestEnvironment (
    module Environment.MonadError,
    module Environment.MonadIO,
    File(..),
    TestEnvironment,
    runTestEnvironment,
  ) where

import Environment.MonadError
import Environment.MonadIO

import qualified Control.Monad.Except as ME
import Control.Monad.State (StateT, state, gets, runStateT)
import Data.Maybe (listToMaybe, isJust)
import Prelude hiding (putStr, putStrLn, getLine)

data File = File
  { path         :: FilePath
  , isExecutable :: Bool
  }

data GlobalState = GlobalState
  { pwd        :: FilePath
  , files      :: [File]
  , inputLines :: [String]
  , outputText :: String
  }

newtype TestEnvironment a = TestEnvironment (StateT GlobalState (Either Error) a)
  deriving (Functor, Applicative, Monad, ME.MonadError Error, MonadError)

findFile :: FilePath -> TestEnvironment (Maybe File)
findFile filePath = TestEnvironment . gets $
  \gs ->
    let allFiles = files gs
        absFilePath =
          if filePath == ""
            then ""
            else if head filePath == '/'
              then filePath
              else pwd gs ++ filePath
    in  listToMaybe $ filter ((absFilePath ==) . path) allFiles

getLine' :: TestEnvironment String
getLine' = TestEnvironment . state $
  \gs -> (head $ inputLines gs, gs {inputLines = tail $ inputLines gs})

instance MonadIO TestEnvironment where
  putStr msg = TestEnvironment . state $
    \gs -> ((), gs { outputText = outputText gs ++ msg })
  getLine = do
    line <- getLine'
    putStrLn line
    return line
  doesFileExist filePath = isJust <$> findFile filePath
  findExecutable filePath = (>>= (\file -> if isExecutable file then Just (path file) else Nothing)) <$> findFile filePath
  exit _ = return () -- not used yet

runTestEnvironment :: FilePath -> [File] -> [String] -> TestEnvironment a -> Either Error (a, String)
runTestEnvironment pwd files inputLines (TestEnvironment m) = fmap outputText <$> runStateT m (GlobalState pwd files inputLines "")

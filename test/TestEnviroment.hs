{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TestEnviroment (
    module Enviroment.MonadError,
    module Enviroment.MonadIO,
    File(..),
    TestEnviroment,
    runTestEnviroment,
  ) where

import Enviroment.MonadError
import Enviroment.MonadIO

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

newtype TestEnviroment a = TestEnviroment (StateT GlobalState (Either Error) a)
  deriving (Functor, Applicative, Monad, ME.MonadError Error, MonadError)

findFile :: FilePath -> TestEnviroment (Maybe File)
findFile filePath = TestEnviroment . gets $
  \gs ->
    let allFiles = files gs
        absFilePath =
          if filePath == ""
            then ""
            else if head filePath == '/'
              then filePath
              else pwd gs ++ filePath
    in  listToMaybe $ filter ((absFilePath ==) . path) allFiles

getLine' :: TestEnviroment String
getLine' = TestEnviroment . state $
  \gs -> (head $ inputLines gs, gs {inputLines = tail $ inputLines gs})

instance MonadIO TestEnviroment where
  putStr msg = TestEnviroment . state $
    \gs -> ((), gs { outputText = outputText gs ++ msg })
  getLine = do
    line <- getLine'
    putStrLn line
    return line
  doesFileExist filePath = isJust <$> findFile filePath
  findExecutable filePath = (>>= (\file -> if isExecutable file then Just (path file) else Nothing)) <$> findFile filePath
  exit _ = return () -- not used yet

runTestEnviroment :: FilePath -> [File] -> [String] -> TestEnviroment a -> Either Error (a, String)
runTestEnviroment pwd files inputLines (TestEnviroment m) = fmap outputText <$> runStateT m (GlobalState pwd files inputLines "")

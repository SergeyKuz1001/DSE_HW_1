{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Phases.Executor.Tests (
    testsExecutor
) where

import Test.HUnit (Test(TestList, TestCase), assertEqual)
import Data.ImprovedPrimitive
import Phases.Executor (executor)
import Environment.MonadExit (ExitCode, MonadExit (exit))
import Environment.MonadVarsReader
import Environment.MonadIO as EnvIO ( MonadIO(..) )
import qualified Data.Map as Map
import Environment.MonadFS.Internal ( AbsFilePath(AbsFilePath) )
import Data.ByteString.Char8 (pack)
import Control.Monad.State hiding (MonadIO)

type TestFileInfo = (String, String)

data IOState = IOState
  { stdOut :: String,
    externalCommands :: [(String, [String], [(String, String)])],
    pwd :: AbsFilePath,
    vars :: Map.Map String String,
    exitCode :: Maybe ExitCode
  }
  deriving (Eq, Show)

newtype TestEnvironment a = TestEnvironment { runTestEnvironment :: State IOState a }
  deriving (Functor, Applicative, Monad)

instance MonadIO TestEnvironment where
  putStr str = TestEnvironment $ modify (\st -> st { stdOut = str })
  readFile (AbsFilePath filePath) = TestEnvironment $ return $ files Map.! filePath
  readFileFromBytes filePath = EnvIO.readFile filePath >>= (TestEnvironment . return . pack)
  createProcess (AbsFilePath filePath) args vars = TestEnvironment (modify (\st -> st { externalCommands = (filePath, args, vars) : externalCommands st })) >> return 0
  getLine = undefined -- Not used

instance MonadVarPwdReader TestEnvironment where
  getVarPwd = TestEnvironment $ gets pwd

instance MonadVarPathReader TestEnvironment where
  getVarPath = undefined -- Not used

instance MonadVarsReader TestEnvironment where
  getVar str = TestEnvironment $ gets ((Map.! str) . vars)
  getVars = TestEnvironment $ gets (Map.toList . vars)

instance MonadExit TestEnvironment where
  exit code = TestEnvironment $ modify (\st -> st { exitCode = Just code })

fileTestExample :: TestFileInfo
fileTestExample = ("Example.txt", "Some example text")
fileTestMSE :: TestFileInfo
fileTestMSE = ("MSE.txt", "I love MSE!")

files :: Map.Map String String
files = Map.fromList
  [fileTestExample, fileTestMSE]

checkState :: String -> IOState -> IOState -> Primitive -> Test
checkState textError excepted actual prim = TestCase $ assertEqual textError excepted $ execState (runTestEnvironment (executor prim)) actual

emptyState :: IOState
emptyState = IOState {
  stdOut = "",
  externalCommands = [],
  pwd = AbsFilePath "",
  vars = Map.empty,
  exitCode = Nothing
}

testsExecutor :: Test
testsExecutor = TestList [
  checkState "empty command" emptyState emptyState EmptyCommand,  
  checkState "exit - check exit code" (emptyState { exitCode = Just 2 }) emptyState $ Command $ Special $ Exit $ Just 2,

  let (filePath, fileText) = fileTestMSE
    in checkState "cat -- MSE file" (emptyState { stdOut = fileText ++ "\n" }) emptyState $ Command $ Common $ Internal $ Cat (AbsFilePath filePath),
  let (filePath, fileText) = fileTestExample
    in checkState "cat -- Example file" (emptyState { stdOut = fileText ++ "\n" }) emptyState $ Command $ Common $ Internal $ Cat (AbsFilePath filePath),

  let output = "Hello"
    in checkState "echo -- One word" (emptyState { stdOut = output ++ "\n" }) emptyState $ Command $ Common $ Internal $ Echo $ words output,
  let output = "It's a new day!"
    in checkState "echo -- Few word" (emptyState { stdOut = output ++ "\n" }) emptyState $ Command $ Common $ Internal $ Echo $ words output,

  let (filePath, _) = fileTestMSE
    in checkState "wc -- MSE file" (emptyState { stdOut = "0 3 11\n"}) emptyState $ Command $ Common $ Internal $ Wc (AbsFilePath filePath),
  let (filePath, _) = fileTestExample
    in checkState "wc -- example file" (emptyState { stdOut = "0 3 17\n"}) emptyState $ Command $ Common $ Internal $ Wc (AbsFilePath filePath),

  let absPwd@(AbsFilePath pwd) = AbsFilePath "/home/"
    in checkState "pwd" (emptyState { stdOut = pwd ++ "\n", pwd = absPwd }) (emptyState { pwd = absPwd }) $ Command $ Common $ Internal Pwd,

  let cmd@(name, args, vars) = ("ls", ["~/example/"], [("example", "test")])
    in let varsMap = Map.fromList vars
      in checkState "external 'ls'" (emptyState { externalCommands = [cmd], vars = varsMap }) (emptyState { vars = varsMap }) $ Command $ Common $ External $ Arguments (AbsFilePath name) args
  ]
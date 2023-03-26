{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Phases.Executor.Tests (
    testsExecutor
) where

import Data.LinkedPrimitive
import Data.AnalyzedPrimitive (GrepArgs(..))
import Data.Variable
import qualified Phases.Analyzer.Internal.Grep as A
import Phases.Linker.Pure
import Phases.Executor (executor)
import Data.ExitCode (ExitCode(..))
import Data.FSObjects (AbsFilePath(..), absFilePath)
import Data.Handles (InputHandle(..), OutputHandle (..))
import Monads.Exit (MonadExit(..))
import Monads.IO (MonadIO(..))
import Monads.PM (MonadPM(..))
import Monads.VarReader
import Monads.VarWriter (MonadVarWriter (..))
import Monads.PwdWriter (MonadPwdWriter (..))
import Monads.PathWriter (MonadPathWriter (..))

import Control.Monad.State hiding (MonadIO)
import qualified Data.Map as Map
import Test.HUnit (Test(TestList, TestCase), assertEqual)
import Data.Text.Lazy as TZ (Text, unpack, pack, empty, append, unlines, lines)

type TestFileInfo = (AbsFilePath, Text, Text)

data IOState = IOState
  { bufIn :: Text,
    bufOut :: Text,
    stdOut :: Text,
    pwd :: AbsFilePath,
    vars :: Map.Map Stable String,
    exitCode :: Maybe ExitCode,
    externalCommands :: [(String, [String])]
  }
  deriving (Eq, Show)

newtype TestEnvironment a = TestEnvironment { runTestEnvironment :: State IOState a }
  deriving (Functor, Applicative, Monad)

instance MonadIO TestEnvironment where
  putStr str = TestEnvironment $ modify (\st -> st { stdOut = pack str })
  getLine = undefined -- Not used

instance MonadPM TestEnvironment where
  type Stream TestEnvironment = IOState
  type Process TestEnvironment = IOState
  getDefaultStream = TestEnvironment get
  applyFuncToStream func hIn hOut stream = do
    strIn <- TestEnvironment $ case hIn of
      FromParentHandle -> do
        let oldStdIn = bufIn stream
        modify (\st -> st { bufOut = empty })
        return oldStdIn
      FromFile path -> return $ files Map.! path
      FromString str -> return str
    strOut <- func strIn
    TestEnvironment $ do
      case hOut of
        ToStdout -> modify (\st -> st { stdOut = stdOut st `append` strOut, bufIn = empty, bufOut = empty })
        ToNewPipe -> modify (\st -> st { bufIn = strOut, bufOut = empty })
        ToNowhere -> modify id
      get

  createProcess path args _ _ _ _ = TestEnvironment $ do
    modify (\st -> st { externalCommands = externalCommands st ++ [(asFilePath path, args)] })
    st <- get
    return (st, st)
  waitForProcess _ = TestEnvironment $ return $ ExitCode 0
  terminateProcess _ = TestEnvironment $ return ()

instance MonadPwdReader TestEnvironment where
  getVarPwd = TestEnvironment $ gets pwd

instance MonadPathReader TestEnvironment where
  getVarPath = undefined -- Not used

instance MonadVarReader TestEnvironment where
  getVar var = case var of
    Stable stable -> TestEnvironment $ gets ((Map.! stable) . vars)
    Volatile _ -> undefined -- Not used
  getVars = TestEnvironment $ gets (Map.toList . vars)

instance MonadPwdWriter TestEnvironment where
  setVarPwd path = TestEnvironment $ modify (\st -> st { vars = Map.insert varPwd (asFilePath path) (vars st) })

instance MonadPathWriter TestEnvironment where
  setVarPath paths = TestEnvironment $ modify (\st -> st { vars = Map.insert varPath (foldr (\x xs -> asFilePath x ++ ':' : xs) [] paths) (vars st) })

instance MonadVarWriter TestEnvironment where
  setVar var value = TestEnvironment $ modify (\st -> st { vars = Map.insert var value $ vars st })

instance MonadExit TestEnvironment where
  exit code = TestEnvironment $ modify (\st -> st { exitCode = Just code })

replace :: Eq a => a -> a -> [a] -> [a]
replace x y = foldr (\z -> if x == z then (y:) else (z:)) []

updAbsPath :: FilePath -> AbsFilePath
updAbsPath path = either (error "it isn't AbsFilePath") id . absFilePath $
  if pathSeparator == '/'
    then path
    else "C:" ++ replace '/' '\\' path

fileTestMSE :: TestFileInfo
fileTestMSE = (updAbsPath "/MSE.txt", "I love MSE!", "1\t3\t11\n")
fileTestExample :: TestFileInfo
fileTestExample = (updAbsPath "/Example.txt", "Some example text", "1\t3\t17\n")
fileWithManyLines :: TestFileInfo
fileWithManyLines = (updAbsPath "/ManyLines.txt", "First line\nSecond line\nThird line\nFourth line\nFifth line\nSixth line\nSeventh line\nEighth line\nNinth line\nTenth line\n", undefined)

files :: Map.Map AbsFilePath Text
files = Map.fromList $ map (\(path, text, _) -> (path, text))
  [fileTestMSE, fileTestExample, fileWithManyLines]

checkState :: String -> IOState -> IOState -> Primitive -> Test
checkState textError excepted actual prim = TestCase $ assertEqual textError excepted $ execState (runTestEnvironment (executor prim)) actual

defaultState :: IOState
defaultState = IOState {
  bufIn = empty,
  bufOut = empty,
  stdOut = empty,
  pwd = updAbsPath "/",
  vars = Map.fromList [(Specific LastExitCode, "0")],
  exitCode = Nothing,
  externalCommands = []
}

testsExecutor :: Test
testsExecutor = let
  catCommand = Internal $ Pure "cat" cat
  wcCommand = Internal $ Pure "wc" wc
  grepCommand isFullWords caseIsIgnored lineCount regexStr = Internal . Pure "grep" . grep . either undefined (\re -> GrepArgs False lineCount re Nothing) $ A.grepRegex isFullWords caseIsIgnored regexStr
  pwdCommand = Internal $ Impure Pwd
  getLines indexes file = let fileLines = TZ.lines file in TZ.unlines $ map (fileLines !!) indexes
  in TestList [
    checkState "empty command" defaultState defaultState $ Commons [],
    checkState "exit - check exit code" (defaultState { exitCode = Just $ ExitCode 2 }) defaultState $ Special $ Exit $ Just $ ExitCode 2,

    let (fileAbsPath, fileText, _) = fileTestMSE
      in checkState "cat -- MSE file" (defaultState { stdOut = fileText }) defaultState $ Commons [(catCommand, FromFile fileAbsPath, ToStdout)],
    let (fileAbsPath, fileText, _) = fileTestExample
      in checkState "cat -- Example file" (defaultState { stdOut = fileText }) defaultState $ Commons [(catCommand, FromFile fileAbsPath, ToStdout)],

    let output = "Hello"
      in checkState "echo -- One word" (defaultState { stdOut = output `append` "\n" }) defaultState $ Commons [(catCommand, FromString $ echo $ words (unpack output), ToStdout)],
    let output = "It's a new day!"
      in checkState "echo -- Few word" (defaultState { stdOut = output `append` "\n" }) defaultState $ Commons [(catCommand, FromString $ echo $ words (unpack output), ToStdout)],

    let (fileAbsPath, _, wcOutput) = fileTestMSE
      in checkState "wc -- MSE file" (defaultState { stdOut = wcOutput }) defaultState $ Commons [(wcCommand, FromFile fileAbsPath, ToStdout)],
    let (fileAbsPath, _, wcOutput) = fileTestExample
      in checkState "wc -- example file" (defaultState { stdOut = wcOutput }) defaultState $ Commons [(wcCommand, FromFile fileAbsPath, ToStdout)],

    let (fileAbsPath, fileText, _) = fileWithManyLines
      in checkState "grep -- check -i" (defaultState { stdOut = getLines [4] fileText }) defaultState $ Commons [(grepCommand False True 0 "Fifth", FromFile fileAbsPath, ToStdout)],
    let (fileAbsPath, fileText, _) = fileWithManyLines
      in checkState "grep -- without -w and regular expr" (defaultState { stdOut = getLines [6, 8, 9] fileText }) defaultState $ Commons [(grepCommand False False 0 "nth", FromFile fileAbsPath, ToStdout)],
    let (fileAbsPath, fileText, _) = fileWithManyLines
      in checkState "grep -- with -w and regular expr" (defaultState { stdOut = getLines [4] fileText }) defaultState $ Commons [(grepCommand True False 0 "Fifth", FromFile fileAbsPath, ToStdout)],
    let (fileAbsPath, fileText, _) = fileWithManyLines
      in checkState "grep -- with -A2 and regular expr" (defaultState { stdOut = getLines [1, 2, 3, 6, 7, 8, 9] fileText }) defaultState $ Commons [(grepCommand False False 2 "nth|nd", FromFile fileAbsPath, ToStdout)],

    let absPwd = updAbsPath "/home/"
      in checkState "pwd" (defaultState { stdOut = pack $ asFilePath absPwd ++ "\n", pwd = absPwd }) (defaultState { pwd = absPwd }) $ Commons [(pwdCommand, FromString "", ToStdout)],

    let (name, args, currentVars) = (updAbsPath "/usr/bin/example", ["~/example/"], [(varPath, "Test")])
        varsMap = Map.fromList currentVars `Map.union` vars defaultState
      in checkState "external command" (defaultState { externalCommands = [(asFilePath name, args)], vars = varsMap }) (defaultState { vars = varsMap }) $ Commons [(External (Arguments name args), FromParentHandle, ToStdout)],
    let [(name1, args1), (name2, args2)] = [(updAbsPath "/usr/bin/example1", ["~/example1/"]), (updAbsPath "/usr/bin/example1", ["~/example1/"])]
      in checkState "few external commands" (defaultState { externalCommands = [(asFilePath name1, args1), (asFilePath name2, args2)]}) defaultState $ Commons [(External (Arguments name1 args1), FromParentHandle, ToNewPipe), (External (Arguments name2 args2), FromParentHandle, ToNewPipe)],

    let (fileAbsPath, fileText, _) = fileTestMSE
      in checkState "many cat -- MSE file" (defaultState { stdOut = fileText }) defaultState $ Commons [(catCommand, FromFile fileAbsPath, ToNewPipe), (catCommand, FromParentHandle, ToNowhere), (catCommand, FromParentHandle, ToStdout)],
    let (fileAbsPath, _, wcOutput) = fileTestExample
      in checkState "cat Example.txt | wc" (defaultState { stdOut = wcOutput }) defaultState $ Commons [(catCommand, FromFile fileAbsPath, ToNewPipe), (wcCommand, FromParentHandle, ToStdout)],
    let (fileAbsPath, _, _) = fileTestExample
      in checkState "cat Example.txt | wc | wc" (defaultState { stdOut = "1\t3\t7\n" }) defaultState $ Commons [(catCommand, FromFile fileAbsPath, ToNewPipe), (wcCommand, FromParentHandle, ToNewPipe), (wcCommand, FromParentHandle, ToStdout)],
    let (fileAbsPath, fileText, _) = fileWithManyLines
      in checkState "cat ManyLines.txt | grep" (defaultState { stdOut = getLines [6, 8, 9] fileText }) defaultState $ Commons [(catCommand, FromFile fileAbsPath, ToNewPipe), (grepCommand False False 0 "nth", FromParentHandle, ToStdout)]
  ]

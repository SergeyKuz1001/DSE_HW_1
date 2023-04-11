{- |
Модуль предназначен для анализа корректности пользовательского запроса и
преобразования его в более выразительный формат.
-}
module Phases.Analyzer
  ( analyzer,
  ) where

import           Phases.Analyzer.Common
import           Phases.Analyzer.Internal.Grep

import           Data.AnalyzedPrimitive hiding (Primitive (..))
import qualified Data.AnalyzedPrimitive as AP
import           Data.ExitCode          (ExitCode (..))
import qualified Data.ParsedPrimitive   as PP
import           Data.Variable          (asStable, variable)
import           Monads.Error           (MonadError, throwError, (@:), (?>=), (@>=))
import           Monads.FS
import           Monads.VarReader
import           Data.FSObjects         (absFilePath, (</>))

import           Control.Monad          (forM, (>=>))
import           Data.List.NonEmpty     (NonEmpty (..))
import           Data.Maybe             (fromMaybe)
import           Text.Read              (readMaybe)

-- | Вспомогательный тип для одной команды.
data Command
  = Special Special
  | Common Common
  | Empty

-- | Анализ корректности и преобразование одной команды с аргументами.
commandAnalyzer :: (MonadError m, MonadFS m, MonadVarReader m) => [String] -> m Command
commandAnalyzer ("cat" : args) = do
  mFilePath <- checkOptionalArg "cat" args
  mAbsFilePath <- traverse findReadable mFilePath
  return . Common . Internal $ Cat mAbsFilePath
commandAnalyzer ("echo" : args) = do
  return . Common . Internal $ Echo args
commandAnalyzer ("wc" : args) = do
  mFilePath <- checkOptionalArg "wc" args
  mAbsFilePath <- traverse findReadable mFilePath
  return . Common . Internal $ Wc mAbsFilePath
commandAnalyzer ("pwd" : args) = do
  checkNoneArg "pwd" args
  return . Common $ Internal Pwd
commandAnalyzer ("grep" : args) = do
  grepArgs <- grep args
  return . Common . Internal $ Grep grepArgs
commandAnalyzer ("exit" : args) = do
  mArg <- checkOptionalArg "exit" args
  mEc <- forM mArg (\arg -> do
    ec <- readMaybe arg @: toError "argument of `exit` command must be integer"
    return $ ExitCode ec)
  return . Special $ Exit mEc
commandAnalyzer ("cd" : args) = do
  homeVar <- variable "HOME" >>= getVar
  filePath <- fromMaybe homeVar <$> checkOptionalArg "cd" args
  return . Special $ Cd filePath
commandAnalyzer ("ls" : args) = do
  curDir <- getVarPwd
  dirPath <- maybe curDir (curDir </>) <$> checkOptionalArg "ls" args
  checkIfDirExistByAbsPath dirPath ?>= toError "Path not found"  
  return . Common . Internal $ Ls dirPath
commandAnalyzer (name : args) = do
  absFilePath <- doesExecutableExist name @>= toError ("can't find executable file by path \"" ++ name ++ "\"")
  return . Common . External $ Arguments absFilePath args
commandAnalyzer [] = do
  return Empty

-- | Анализ корректности и преобразование пользовательского запроса.
analyzer :: (MonadError m, MonadFS m, MonadVarReader m) => PP.Primitive -> m AP.Primitive
analyzer (PP.Commands []) =
  return AP.Empty
analyzer (PP.Commands [command]) = do
  command' <- commandAnalyzer command
  return $ case command' of
    Special c -> AP.Special c
    Common c  -> AP.Commons $ c :| []
    Empty     -> AP.Empty
analyzer (PP.Commands (command : commands)) =
  AP.Commons <$> traverse (commandAnalyzer >=> asCommon) (command :| commands)
    where
      asCommon :: MonadError m => Command -> m Common
      asCommon (Common c) = return c
      asCommon _ = throwError $ toError "can't using non-common command with pipes"
analyzer (PP.Assignment name value) = do
  var <- variable name
  stVar <- asStable var @: toError "can't assign volatile variable"
  return $ AP.Assignment stVar value

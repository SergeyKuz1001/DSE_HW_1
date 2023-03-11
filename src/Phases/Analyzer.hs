{-# LANGUAGE NoImplicitPrelude #-}

{- |
Модуль предназначен для анализа корректности пользовательского запроса и
преобразования его в более выразительный формат.
-}
module Phases.Analyzer (
    analyzer,
  ) where

import qualified Data.Primitive as P
import Data.ImprovedPrimitive hiding (Primitive(..))
import qualified Data.ImprovedPrimitive as IP
import Data.Variable (variable, asStable)
import Environment.MonadError
import Environment.MonadFS
import Environment.MonadPathReader
import Environment.MonadPwdReader

import Control.Monad (forM, (>=>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (listToMaybe)
import Prelude hiding (error)
import Text.Read (readMaybe)

-- | Функция получения объекта-ошибки по информации об ошибке.
error :: String -> Error
error = Error "AnalyzingError"

-- | Вспомогательный тип для одной команды.
data Command
  = Special Special
  | Common Common
  | Empty

-- | Анализ корректности и преобразование одной команды с аргументами.
commandAnalyzer :: (MonadError m, MonadFS m, MonadPwdReader m, MonadPathReader m) => [String] -> m Command
commandAnalyzer ("cat" : args) = do
  length args == 1 ?: error "`cat` command must have only one argument"
  let filePath = head args
  absFilePath <- doesFileExist filePath @>= error ("can't find file by path \"" ++ filePath ++ "\"")
  isReadable absFilePath ?>= error ("file \"" ++ show absFilePath ++ "\" hasn't readable permission")
  return . Common . Internal $ Cat absFilePath
commandAnalyzer ("echo" : args) = do
  return . Common . Internal $ Echo args
commandAnalyzer ("wc" : args) = do
  length args == 1 ?: error "`wc` command must have only one argument"
  let filePath = head args
  absFilePath <- doesFileExist filePath @>= error ("can't find file by path \"" ++ filePath ++ "\"")
  isReadable absFilePath ?>= error ("file \"" ++ show absFilePath ++ "\" hasn't readable permission")
  return . Common . Internal $ Wc absFilePath
commandAnalyzer ("pwd" : args) = do
  null args ?: error "`pwd` command hasn't arguments"
  return . Common . Internal $ Pwd
commandAnalyzer ("exit" : args) = do
  length args <= 1 ?: error "too many arguments of `exit` command"
  let mArg = listToMaybe args
  mInt <- forM mArg (\arg ->
    readMaybe arg @: error "argument of `exit` command must be integer")
  return . Special $ Exit mInt
commandAnalyzer ("cd" : args) = do
  length args == 1 ?: error "`cd` command must have only one argument"
  let filePath = head args
  return . Special $ Cd filePath
commandAnalyzer (name : args) = do
  absFilePath <- doesExecutableExist name @>= error ("can't find executable file by path \"" ++ name ++ "\"")
  return . Common . External $ Arguments absFilePath args
commandAnalyzer [] = do
  return Empty

-- | Анализ корректности и преобразование пользовательского запроса.
analyzer :: (MonadError m, MonadFS m, MonadPwdReader m, MonadPathReader m) => P.Primitive -> m IP.Primitive
analyzer (P.Commands []) =
  return IP.Empty
analyzer (P.Commands [command]) = do
  command' <- commandAnalyzer command
  return $ case command' of
    Special c -> IP.Special c
    Common c  -> IP.Commons $ c :| []
    Empty     -> IP.Empty
analyzer (P.Commands (command : commands)) =
  IP.Commons <$> traverse (commandAnalyzer >=> asCommon) (command :| commands)
    where
      asCommon :: MonadError m => Command -> m Common
      asCommon (Common c) = return c
      asCommon _ = throwError $ error "can't using non-common command with pipes"
analyzer (P.Assignment name value) = do
  var <- variable name
  stVar <- asStable var @: error "can't assign volatile variable"
  return $ IP.Assignment stVar value

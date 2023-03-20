{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Модуль предназначен для анализа корректности пользовательского запроса и
-- преобразования его в более выразительный формат.
module Phases.Analyzer
  ( analyzer,
  )
where

import           Control.Monad          (forM, (>=>), join)
import           Data.List.NonEmpty     (NonEmpty (..))
import           Data.Maybe             (listToMaybe)
import           Monads.Error
import           Monads.FS
import           Monads.PathReader
import           Monads.PwdReader
import           Options.Applicative
import           Prelude                hiding (error)
import           Text.Read              (readMaybe)

import           Data.AnalyzedPrimitive hiding (Primitive (..))
import qualified Data.AnalyzedPrimitive as AP
import           Data.Error             (Error (..))
import           Data.ExitCode          (ExitCode (..))
import           Data.FSObjects         (AbsFilePath)
import qualified Data.ParsedPrimitive   as PP
import           Data.Variable          (asStable, variable)

-- | Функция получения объекта-ошибки по информации об ошибке.
error :: String -> Error
error = Error "AnalyzingError"

-- | Вспомогательный тип для одной команды.
data Command
  = Special Special
  | Common Common
  | Empty

findRFile :: (MonadError m, MonadFS m, MonadPwdReader m) => Maybe String -> m (Maybe AbsFilePath)
findRFile mFilePath = forM mFilePath $ \filePath -> do
  absFilePath <- doesFileExist filePath @>= error ("can't find file by path \"" ++ filePath ++ "\"")
  isReadable absFilePath ?>= error ("file \"" ++ show absFilePath ++ "\" hasn't readable permission")
  return absFilePath

-- | Анализ корректности и преобразование одной команды с аргументами.
commandAnalyzer :: (MonadError m, MonadFS m, MonadPwdReader m, MonadPathReader m) => [String] -> m Command
commandAnalyzer ("cat" : args) = do
  length args <= 1 ?: error "too many arguments of `cat` command"
  let mFilePath = listToMaybe args
  mAbsFilePath <- findRFile mFilePath
  return . Common . Internal $ Cat mAbsFilePath
commandAnalyzer ("echo" : args) = do
  return . Common . Internal $ Echo args
commandAnalyzer ("wc" : args) = do
  length args <= 1 ?: error "too many arguments of `wc` command"
  let mFilePath = listToMaybe args
  mAbsFilePath <- findRFile mFilePath
  return . Common . Internal $ Wc mAbsFilePath
commandAnalyzer ("pwd" : args) = do
  null args ?: error "`pwd` command hasn't arguments"
  return . Common $ Internal Pwd
commandAnalyzer ("grep" : args) = do
  case execParserPure defaultPrefs argparseInfo args of
    Success (GrepArgsRaw fw ic lc rx path) ->
      Common . Internal . Grep . GrepArgs fw ic lc rx
        <$> findRFile path
    Failure pf -> let (h, _, _) = execFailure pf "grep" in throwError . error $ show h
    CompletionInvoked _ -> throwError $ error "Impossible situation occured"
  where
    argparse :: Parser GrepArgsRaw
    argparse = GrepArgsRaw
      <$> switch (short 'w' <> long "words" <> help "Match only whole words")
      <*> switch (short 'i' <> long "ignorecase" <> help "Ignore case")
      <*> option auto (short 'A' <> long "lines" <> value 0 <> metavar "COUNT" <> help "How many lines to show around the matched ones")
      <*> strArgument (metavar "REGEX" <> help "Regular expression")
      <*> (pure Nothing <|> Just <$> strArgument (metavar "FILE" <> help "File to use as source (if not present, standard input is used)"))
      <**> helper
    argparseInfo :: ParserInfo GrepArgsRaw
    argparseInfo = info argparse fullDesc
commandAnalyzer ("exit" : args) = do
  length args <= 1 ?: error "too many arguments of `exit` command"
  let mArg = listToMaybe args
  mEc <- forM mArg (\arg -> do
    ec <- readMaybe arg @: error "argument of `exit` command must be integer"
    return $ ExitCode ec)
  return . Special $ Exit mEc
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
analyzer :: (MonadError m, MonadFS m, MonadPwdReader m, MonadPathReader m) => PP.Primitive -> m AP.Primitive
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
      asCommon _ = throwError $ error "can't using non-common command with pipes"
analyzer (PP.Assignment name value) = do
  var <- variable name
  stVar <- asStable var @: error "can't assign volatile variable"
  return $ AP.Assignment stVar value

data GrepArgsRaw = GrepArgsRaw Bool Bool Int String (Maybe String)
  deriving (Eq, Show)

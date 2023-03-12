{- |
Модуль для выполнения команд.
-}
{-# LANGUAGE LambdaCase #-}
module Phases.Executor (
    executor,
  ) where

import Data.ImprovedPrimitive
import Environment.MonadExit (MonadExit (exit), ExitCode (ExitCode))
import Environment.MonadIO as EnvIO
import Environment.MonadPM as PM
import Environment.MonadVarReader (MonadVarReader (..))
import Environment.MonadVarWriter (MonadVarWriter (..))
import Environment.MonadPwdReader (MonadPwdReader (..))
import Data.Variable (varPwd, getVarName)
import System.IO (Handle)
import System.IO as SIO (stderr, stdin)
import Environment.FSPrimitive (asFilePath)

import Data.Bool (bool)
import qualified Data.ByteString.Char8 as ByteStr
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Data.List.NonEmpty hiding (last, drop)
import Data.ByteString.Char8 (pack)

type WcOutputArguments = (Int, Int, Int, Bool)

-- | Функция принимает, разбирает и исполняет распарсшенный примитив.
executor :: (MonadIO m, MonadPM m, MonadExit m, MonadVarWriter m, MonadVarReader m) => Primitive -> m ()
executor = \case
  Special special -> executeSpecial special
  Commons commons -> do
    handles <- PM.createPipe
    executeCommons (SIO.stdin, handles, SIO.stderr) commons
  Assignment var value -> setVar var value
  Empty -> return ()

executeCommons :: (MonadIO m, MonadVarReader m, MonadPM m) => (Handle, (Handle, Handle), Handle) -> NonEmpty Common -> m ()
executeCommons allHandles@(_, (hIn, _), hErr) = \case
  x :| [] -> execCommon allHandles x >> putStrFromHandle hIn
  x :| (l : xs) -> do
    execCommon allHandles x
    newHandles <- PM.createPipe
    executeCommons (hIn, newHandles, hErr) (l :| xs)
  where
    execCommon (hInOld, (_, hOut), hErrCurrent) = \case
      Internal internal -> executeInternal (hInOld, hOut) internal
      External external -> executeExternal (hInOld, hOut, hErrCurrent) external

-- | Функция для исполнения специальных команд.
executeSpecial :: (MonadExit m, MonadVarWriter m) => Special -> m ()
executeSpecial = \case
  Cd filePath -> setVar varPwd (getVarName varPwd ++ '/' : filePath)
  Exit mb -> exit (ExitCode $ fromMaybe 0 mb)

-- | Функция для исполнения внутренних команд.
executeInternal :: (MonadIO m, MonadPwdReader m, MonadPM m) => (Handle, Handle) -> Internal -> m ()
executeInternal (hIn, hOut) = \case
  Cat maybeFilePath -> do
    file <- maybe (PM.hGetContents hIn) EnvIO.readFile maybeFilePath
    PM.hPutStr hOut file
  Echo ls -> do
    PM.hPutStr hOut $ drop 1 $ concatMap (' ' :) ls
  Wc maybeFilePath -> do
    file <- maybe (pack <$> PM.hGetContents hIn) EnvIO.readFileFromBytes maybeFilePath
    let (countLines, countWords, bytes, isPrevSpace) = ByteStr.foldl' wcgo (0, 0, 0, False) file
    let newCountWords = countWords + bool 1 0 isPrevSpace
    PM.hPutStr hOut $ show countLines ++ ' ' : show newCountWords ++ ' ' : show bytes
  Pwd -> do
    pwd <- asFilePath <$> getVarPwd
    PM.hPutStr hOut pwd

  where
    wcgo :: WcOutputArguments -> Char -> WcOutputArguments
    wcgo (countLines, countWords, bytes, isPrevSpace) c =
      (countLines + checkOnLine, countWords + checkOnWord, bytes + 1, isSpace c)
      where
        checkOnLine = bool 0 1 $ c == '\n'
        checkOnWord = bool 0 1 $ not isPrevSpace && isSpace c

-- | Функция для исполнения внешних команд.
executeExternal :: (MonadIO m, MonadVarReader m, MonadPM m) => (Handle, Handle, Handle) ->
  External -> m () -- Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle
executeExternal handles = \case
  Arguments pathToCmd args -> do
    vars <- getVars
    process <- createProcess pathToCmd args vars handles
    (ExitCode code) <- waitForProcess process
    if code == 0
    then return ()
    else terminateProcess process

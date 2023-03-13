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
import Environment.MonadPwdWriter (MonadPwdWriter (..))
import Data.Variable (varPwd, getVarName)
import System.IO (Handle)
import System.IO as SIO (stderr, stdin, stdout)
import Environment.FSPrimitive (asFilePath, (</>))

import Data.Bool (bool)
import qualified Data.ByteString.Char8 as ByteStr
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Data.List.NonEmpty hiding (last, drop)
import Data.ByteString.Char8 (pack)

type WcOutputArguments = (Int, Int, Int, Bool)

-- | Функция принимает, разбирает и исполняет распарсшенный примитив.
executor :: (MonadIO m, MonadPM m, MonadExit m, MonadVarWriter m, MonadVarReader m) => Primitive -> m ()
executor = undefined
{-executor = \case
  Special special -> executeSpecial special
  Commons commons -> executeCommons SIO.stdin commons
  Assignment var value -> setVar var value
  Empty -> return ()

executeCommons :: (MonadIO m, MonadVarReader m, MonadPM m) => Handle -> NonEmpty Common -> m ()
executeCommons hIn = \case
  x :| [] -> execCommon (hIn, SIO.stdout, SIO.stderr) x
  x :| (x' : xs) -> do
    (newHIn, newHOut) <- PM.createPipe
    execCommon (hIn, newHOut, SIO.stderr) x
    executeCommons newHIn (x' :| xs)
  where
    execCommon (hIn, hOut, hErr) = \case
      Internal internal -> executeInternal (hIn, hOut) internal
      External external -> executeExternal (hIn, hOut, hErr) external

-- | Функция для исполнения специальных команд.
executeSpecial :: (MonadExit m, MonadPwdReader m, MonadVarWriter m) => Special -> m ()
executeSpecial = \case
  Cd path -> do
    pwd <- getVarPwd
    setVarPwd (pwd </> path)
  Exit mec -> exit $ fromMaybe (ExitCode 0) mec

-- | Функция для исполнения внутренних команд.
executeInternal :: (MonadIO m, MonadPwdReader m, MonadPM m) => (Handle, Handle) -> Internal -> m ()
executeInternal (hIn, hOut) = \case
  Cat maybeFilePath -> do
    file <- maybe (PM.hGetContents hIn) EnvIO.readFile maybeFilePath
    PM.hPutStr hOut file
  Echo ls -> do
    PM.hPutStr hOut $ unwords ls ++ "\n"
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
    else terminateProcess process-}

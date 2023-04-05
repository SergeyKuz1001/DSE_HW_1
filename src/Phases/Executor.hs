{-# LANGUAGE LambdaCase #-}

{- |
Модуль для выполнения команд.
-}
module Phases.Executor (
    executor,
  ) where

import Phases.Executor.Impure

import Data.LinkedPrimitive
import Data.ExitCode (ExitCode (..))
import Data.FSObjects ((</>))
import Data.Variable
import Monads.Exit (MonadExit (exit))
import Monads.PM (MonadPM(..))
import Monads.VarReader (MonadVarReader(..), MonadPwdReader(..))
import Monads.VarWriter (MonadVarWriter(..), MonadPwdWriter(..))

import Data.Maybe (fromMaybe)

-- | Функция для исполнения примитива. Исполняет список команд "почти"
-- параллельно, а именно подряд идущие внешние команды выполняются
-- параллельно, а внутренние - последовательно. Такое поведение вызвано
-- ограничениями реальной реализации.
executor :: (MonadPM m, MonadExit m, MonadVarWriter m, MonadVarReader m) => Primitive -> m ()
executor = \case
  Special special      -> executeSpecial special
  Assignment var value -> setVar var value
  Commons commands     -> executeCommons commands

-- | Функция для исполнения специальных команд.
executeSpecial :: (MonadExit m, MonadPwdReader m, MonadPwdWriter m) => Special -> m ()
executeSpecial = \case
  Cd path -> do
    cd <- getVarPwd
    setVarPwd (cd </> path)
  Exit mec -> exit $ fromMaybe (ExitCode 0) mec

-- | Функция для исполнения обычных команд с указанными типами потоков ввода и
-- вывода.
executeCommons :: (MonadPM m, MonadVarReader m, MonadVarWriter m) => [CommonWithHandles] -> m ()
executeCommons cmns = do
  stream <- getDefaultStream
  ExitCode ec <- execProc stream [] cmns
  setVar (Specific LastExitCode) (show ec)

-- | Функция запуска и остановки процессов. Принимает поток с данными
-- предыдущего процесса, ссылки на уже запущенные процессы и команды с типами
-- потоков ввода/вывода, которые необходимо запустить. Возвращает код возврата
-- первой команды с ненулевым кодом или нулевой код иначе.
--
-- Запуск процессов возможен только для внешних команд, таким образом подряд
-- идущие внешние команды запускаются параллельно, а при встрече внутренней
-- команды (или конца списка команд) все запущенные процессы останавливаются (не
-- убиваются). При возникновении ошибки (ненулевой код возврата) все запущенные
-- процессы убиваются, а все незапущенные - не запускаются.
execProc :: (MonadPM m, MonadVarReader m) => Stream m -> [Process m] -> [CommonWithHandles] -> m ExitCode
execProc _ runned [] =
  waitProc $ reverse runned
execProc stream runned ((External (Arguments path args), hIn, hOut) : cmns) = do
  vars <- getVars
  (proc, stream') <- createProcess path args hIn hOut vars stream
  execProc stream' (proc : runned) cmns
execProc stream runned ((Internal int, hIn, hOut) : cmns) = do
  ec <- waitProc $ reverse runned
  if ec /= ExitCode 0
    then return ec
    else do
      let func = case int of
            Pure _ f   -> pure . f
            Impure Pwd -> pwd
      stream' <- applyFuncToStream func hIn hOut stream
      execProc stream' [] cmns

-- | Функция остановки запущенных процессов.
waitProc :: MonadPM m => [Process m] -> m ExitCode
waitProc [] =
  return $ ExitCode 0
waitProc (proc : procs) = do
  ec <- waitForProcess proc
  if ec == ExitCode 0
    then
      waitProc procs
    else do
      termProc procs
      return ec

-- | Функция убийства запущенных процессов.
termProc :: MonadPM m => [Process m] -> m ()
termProc [] =
  return ()
termProc (proc : procs) = do
  terminateProcess proc
  termProc procs

{-# LANGUAGE TypeFamilies #-}

{- |
В данном модуле объявлена монада @'MonadPM'@ для платформонезависимой работы с
процессами (запуск и остановка) и логическими потоками ввода/вывода (определение
потока по умолчанию и преобразование данных в потоке).
-}
module Monads.PM (
    MonadPM (..),
  ) where

import Data.ExitCode (ExitCode)
import Data.FSObjects (AbsFilePath)
import Data.Handles (InputHandle, OutputHandle)
import Data.Variable (Stable)

import Data.Kind (Type)
import Data.Text.Lazy (Text)

class Monad m => MonadPM m where
  -- | Логический поток, хранит вывод команд.
  type Stream  m :: Type
  -- | "Ссылка" на запущенный процесс.
  type Process m :: Type
  -- | Функция получения потока по умолчанию (предполагается, что это поток с
  -- вводом пользователя).
  getDefaultStream :: m (Stream m)
  -- | Преобразование данных в потоке с помощью функции.
  applyFuncToStream ::
    (Text -> m Text) ->
    InputHandle ->
    OutputHandle ->
    Stream m ->
    m (Stream m)
  -- | Запуск процесса по абсолютному пути до исполняемого файла с заданными
  -- аргументами, переменными окружения, типами потоков ввода/вывода, а также
  -- с логическим потоком, хранящим вывод предыдущей команды. Возвращает
  -- "ссылку" на запущенный процесс и логический поток с выводом.
  createProcess ::
    AbsFilePath ->
    [String] ->
    InputHandle ->
    OutputHandle ->
    [(Stable, String)] ->
    Stream m ->
    m (Process m, Stream m)
  -- | Ожидание завершения процесса. Возвращает код ошибки.
  waitForProcess :: Process m -> m ExitCode
  -- | Немедленное уничтожение процесса.
  terminateProcess :: Process m -> m ()

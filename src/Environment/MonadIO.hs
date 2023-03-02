{-# LANGUAGE NoImplicitPrelude #-}

{- |
В данном модуле объявлена монада @'MonadIO'@ для работы с потоками ввода/вывода.
-}
module Environment.MonadIO (
    MonadIO (..),
    putStrLn,
    print,
  ) where

import Prelude hiding (putStr, getLine, readFile, putStrLn, print)
import Environment.MonadFS.Internal ( AbsFilePath )
import qualified Data.ByteString.Char8 as ByteStr
import Environment.MonadExit (ExitCode)

-- | Монада для работы с потоками ввода/вывода.
class Monad m => MonadIO m where
  -- | Запись строки в поток вывода.
  putStr :: String -> m ()
  -- | Чтение строки из потока ввода.
  getLine :: m String
  -- | Чтение файла
  readFile :: AbsFilePath -> m String
  -- | Чтение файла бинарно.
  readFileFromBytes :: AbsFilePath -> m ByteStr.ByteString
  -- | Создание и исполнение исполняемого файла с данными аргументами и
  -- переменными. Возвращает код возврата процесса.
  createProcess :: AbsFilePath -> [String] -> [(String, String)] -> m ExitCode

-- | Запись строки в поток вывода и перевод строки.
putStrLn :: MonadIO m => String -> m ()
putStrLn = putStr . (++ "\n")

-- | Запись объекта в виде строки в поток вывода.
print :: (MonadIO m, Show a) => a -> m ()
print = putStrLn . show

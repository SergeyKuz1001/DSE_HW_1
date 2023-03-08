{-# LANGUAGE NoImplicitPrelude #-}

{- |
В данном модуле объявлена монада @'MonadIO'@ для работы с потоками ввода/вывода.
-}
module Environment.MonadIO (
    MonadIO (..),
    putStrLn,
    print,
  ) where

import Environment.FSPrimitive (AbsFilePath)

import Data.ByteString.Char8 (ByteString)
import Prelude hiding (putStr, getLine, readFile, putStrLn, print)

-- | Монада для работы с потоками ввода/вывода.
class Monad m => MonadIO m where
  -- | Запись строки в поток вывода.
  putStr :: String -> m ()
  -- | Чтение строки из потока ввода. Возвращает Nothing если достигнут конец
  -- ввода (EOF).
  getLine :: m (Maybe String)
  -- | Чтение файла.
  readFile :: AbsFilePath -> m String
  -- | Чтение файла бинарно.
  readFileFromBytes :: AbsFilePath -> m ByteString

-- | Запись строки в поток вывода и перевод строки.
putStrLn :: MonadIO m => String -> m ()
putStrLn = putStr . (++ "\n")

-- | Запись объекта в виде строки в поток вывода.
print :: (MonadIO m, Show a) => a -> m ()
print = putStrLn . show

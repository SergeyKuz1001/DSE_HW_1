{-# LANGUAGE NoImplicitPrelude #-}

{- |
В данном модуле объявлена монада @'MonadIO'@ для работы с потоками ввода/вывода.
-}
module Monads.IO (
    MonadIO (..),
    putStrLn,
    print,
  ) where

import Prelude hiding (putStr, getLine, readFile, putStrLn, print)

-- | Монада для работы с потоками ввода/вывода.
class Monad m => MonadIO m where
  -- | Запись строки в поток вывода.
  putStr :: String -> m ()
  -- | Чтение строки из потока ввода. Возвращает Nothing если достигнут конец
  -- ввода (EOF).
  getLine :: m (Maybe String)

-- | Запись строки в поток вывода и перевод строки.
putStrLn :: MonadIO m => String -> m ()
putStrLn = putStr . (++ "\n")

-- | Запись объекта в виде строки в поток вывода.
print :: (MonadIO m, Show a) => a -> m ()
print = putStrLn . show

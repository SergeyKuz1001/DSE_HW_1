{-# LANGUAGE NoImplicitPrelude #-}

{- |
В данном модуле объявлена монада @'MonadIO'@ для работы с потоками ввода/вывода.
-}
module Environment.MonadIO (
    MonadIO(..),
    putStrLn,
    print,
  ) where

import Prelude hiding (putStr, getLine, putStrLn, print)

-- | Монада для работы с потоками ввода/вывода.
class Monad m => MonadIO m where
  -- | Запись строки в поток вывода.
  putStr :: String -> m ()
  -- | Чтение строки из потока ввода.
  getLine :: m String

-- | Запись строки в поток вывода и перевод строки.
putStrLn :: MonadIO m => String -> m ()
putStrLn = putStr . (++ "\n")

-- | Запись объекта в виде строки в поток вывода.
print :: (MonadIO m, Show a) => a -> m ()
print = putStrLn . show

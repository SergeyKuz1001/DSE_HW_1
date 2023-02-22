{-# LANGUAGE NoImplicitPrelude #-}

module Environment.MonadIO (
    MonadIO,
    putStr,
    getLine,
    putStrLn,
    print,
  ) where

import Prelude hiding (putStr, getLine, putStrLn, print)

class Monad m => MonadIO m where
  putStr :: String -> m ()
  getLine :: m String

putStrLn :: MonadIO m => String -> m ()
putStrLn = putStr . (++ "\n")

print :: (MonadIO m, Show a) => a -> m ()
print = putStrLn . show

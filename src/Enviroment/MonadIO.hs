{-# LANGUAGE NoImplicitPrelude #-}

module Enviroment.MonadIO (
    MonadIO,
    putStr,
    putStrLn,
    getLine,
    doesFileExist,
    findExecutable,
    exit,
  ) where

import Prelude hiding (putStr, putStrLn, getLine)

class Monad m => MonadIO m where
  putStr :: String -> m ()
  putStrLn :: String -> m ()
  putStrLn = putStr . (++ "\n")
  getLine :: m String
  doesFileExist :: FilePath -> m Bool
  findExecutable :: FilePath -> m (Maybe FilePath)
  exit :: Int -> m ()

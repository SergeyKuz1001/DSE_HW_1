{-# LANGUAGE NoImplicitPrelude #-}

module Main (
    main,
  ) where

import Environment
import Phases

import Prelude hiding (print)
import System.IO (BufferMode(..), hSetBuffering, stdin, stdout, stderr)

main :: IO ()
main = do
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  runEnvironment main'

main' :: (MonadError m, MonadIO m, MonadFS m, MonadVarsReader m, MonadExit m) => m ()
main' = do
  mExitCode <- (
      stringReader >>= parser >>= analyzer >>= executor
    ) `catchError` (\err -> do
      print err
      return Nothing
    )
  maybe main' exit mExitCode

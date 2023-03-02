{-# LANGUAGE NoImplicitPrelude #-}

module Main (
    main,
  ) where

import Environment
import Phases

import Control.Monad (forever)
import Prelude hiding (print)
import System.IO (BufferMode(..), hSetBuffering, stdin, stdout, stderr)

main :: IO ()
main = do
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  runEnvironment main'

main' :: (MonadError m, MonadIO m, MonadFS m, MonadVarsReader m, MonadExit m) => m ()
main' = forever $ (
      stringReader >>= parser >>= analyzer >>= executor
    ) `catchError` print

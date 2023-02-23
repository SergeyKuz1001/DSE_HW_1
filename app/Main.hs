{-# LANGUAGE NoImplicitPrelude #-}

module Main (
    main,
  ) where

import Environment
import Phases

import Prelude hiding (print)

main :: IO ()
main = runEnvironment main'

main' :: (MonadError m, MonadIO m, MonadFS m, MonadExit m) => m ()
main' = do
  mExitCode <- (
      stringReader >>= parser >>= analyzer >>= executor
    ) `catchError` (\err -> do
      print err
      return Nothing
    )
  maybe main' exit mExitCode

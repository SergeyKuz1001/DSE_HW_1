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
  mExitCode <- (do
      string     <- stringReader
      primitive  <- parser string
      iPrimitive <- analyzer primitive
      executor iPrimitive
    ) `catchError` (\err -> do
      print err
      return Nothing
    )
  case mExitCode of
    Nothing -> main'
    Just ec -> exit ec

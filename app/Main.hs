{-# LANGUAGE NoImplicitPrelude #-}

module Main (
    main,
  ) where

import Enviroment
import Phases

import Prelude hiding (putStrLn)

main :: IO ()
main = runEnviroment main'

main' :: (MonadError m, MonadIO m) => m ()
main' = do
  mExitCode <- (do
      string     <- stringReader
      primitive  <- parser string
      iPrimitive <- analyzer primitive
      executor iPrimitive
    ) `catchError` (\err -> do
      putStrLn $ show err
      return Nothing
    )
  case mExitCode of
    Nothing -> main'
    Just ec -> exit ec

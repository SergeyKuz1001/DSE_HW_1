{-# LANGUAGE NoImplicitPrelude #-}

{- |
Модуль предназначен для чтения пользовательского ввода (запроса).
-}
module Phases.StringReader (
    stringReader,
  ) where

import Environment.MonadIO

import Prelude hiding (putStr, getLine)

-- | Чтение пользовательского запроса.
stringReader :: MonadIO m => m String
stringReader = do
  putStr "$ "
  getLine

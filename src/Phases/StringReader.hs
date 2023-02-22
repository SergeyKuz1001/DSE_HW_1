{-# LANGUAGE NoImplicitPrelude #-}

module Phases.StringReader (
    stringReader,
  ) where

import Environment.MonadIO

import Prelude hiding (putStr, getLine)

stringReader :: MonadIO m => m String
stringReader = do
  putStr "$ "
  getLine

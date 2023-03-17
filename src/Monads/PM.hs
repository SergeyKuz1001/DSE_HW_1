{-# LANGUAGE TypeFamilies #-}

module Monads.PM (
    MonadPM (..),
  ) where

import Data.ExitCode (ExitCode)
import Data.FSObjects (AbsFilePath)
import Data.Handles (InputHandle, OutputHandle)
import Data.Variable (Stable)

import Data.Kind (Type)
import Data.Text.Lazy (Text)

class Monad m => MonadPM m where
  type Stream  m :: Type
  type Process m :: Type
  getDefaultStream :: m (Stream m)
  applyFuncToStream ::
    (Text -> Text) ->
    InputHandle ->
    OutputHandle ->
    Stream m ->
    m (Stream m)
  createProcess ::
    AbsFilePath ->
    [String] ->
    InputHandle ->
    OutputHandle ->
    [(Stable, String)] ->
    Stream m ->
    m (Process m, Stream m)
  waitForProcess :: Process m -> m ExitCode
  terminateProcess :: Process m -> m ()

{-# LANGUAGE TypeFamilies #-}

module Environment.MonadPM (
    MonadPM (..),
  ) where

import Data.Variable (Stable)
import Data.Handles (InputHandle, OutputHandle)
import Environment.FSPrimitive (AbsFilePath)
import Environment.MonadExit (ExitCode)

class Monad m => MonadPM m where
  type Stream  m :: *
  type Process m :: *
  defaultStream :: m (Stream m)
  applyFuncToStream ::
    (String -> String) ->
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

module Environment.MonadPM (
    MonadPM (..),
  ) where

import Data.Variable (Stable)
import Environment.FSPrimitive (AbsFilePath)
import Environment.HandlePrimitive
import Environment.MonadExit (ExitCode)

class Monad m => MonadPM m where
  createProcess ::
    AbsFilePath ->
    [String] ->
    [(Stable, String)] ->
    (HandleAction, HandleAction, HandleAction) ->
    m (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
  waitForProcess :: ProcessHandle -> m ExitCode
  terminateProcess :: ProcessHandle -> m ()

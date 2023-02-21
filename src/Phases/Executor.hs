module Phases.Executor (
    executor,
  ) where

import Data.ImprovedPrimitive (Primitive)
import Enviroment.MonadIO

import System.Exit (ExitCode)

executor :: MonadIO m => Primitive -> m (Maybe ExitCode)
executor = undefined

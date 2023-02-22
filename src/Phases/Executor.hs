module Phases.Executor (
    executor,
  ) where

import Data.ImprovedPrimitive (Primitive)
import Enviroment.MonadIO

type ExitCode = Int

executor :: MonadIO m => Primitive -> m (Maybe ExitCode)
executor = undefined

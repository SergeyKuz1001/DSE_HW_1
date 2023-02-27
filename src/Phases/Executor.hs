module Phases.Executor (
    executor,
  ) where

import Data.ImprovedPrimitive (Primitive)
import Environment.MonadIO
import Environment.MonadVarPwdReader

type ExitCode = Int

executor :: (MonadIO m, MonadVarPwdReader m) => Primitive -> m (Maybe ExitCode)
executor = undefined

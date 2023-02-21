module Phase.Executor (
    executor,
  ) where

import Data.ImprovedPrimitive (Primitive)

import Control.Monad.IO.Class (MonadIO)

executor :: MonadIO m => Primitive -> m Bool
executor = undefined

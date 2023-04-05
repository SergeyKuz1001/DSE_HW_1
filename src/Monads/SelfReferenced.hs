module Monads.SelfReferenced (
    MonadSelfReferenced (..),
  ) where

import Data.FSObjects (AbsFilePath)
import Monads.FS (MonadFS)

class MonadFS m => MonadSelfReferenced m where
  getSelfPath :: m AbsFilePath

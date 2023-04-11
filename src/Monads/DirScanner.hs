module Monads.DirScanner (
    MonadDirScanner (..),
  ) where

import Data.FSObjects (AbsFilePath)


class Monad m => MonadDirScanner m where
  scanDir :: AbsFilePath -> m [FilePath]





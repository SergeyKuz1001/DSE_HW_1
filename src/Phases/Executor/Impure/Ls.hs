
module Phases.Executor.Impure.Ls (
    ls,
  ) where

import Data.FSObjects (AbsFilePath)
import Monads.DirScanner (MonadDirScanner(..))

import Data.Text.Lazy (Text, pack)


ls :: MonadDirScanner m => AbsFilePath -> Text -> m Text
ls path _ = do 
    filesAndDirs <- scanDir path
    return $ pack $ concatMap (++ "\n") filesAndDirs

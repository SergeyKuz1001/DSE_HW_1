module Phases.StringReader (
    stringReader,
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)

stringReader :: MonadIO m => m String
stringReader = do
  liftIO $ putStr "$ "
  liftIO getLine

module Environment.MonadExit (
    ExitCode,
    MonadExit,
    exit,
  ) where

type ExitCode = Int

class Monad m => MonadExit m where
  exit :: ExitCode -> m ()

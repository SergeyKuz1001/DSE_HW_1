{- |
В данном модуле объявлена монада @'MonadExit'@, предоставляющая возможность
выхода из данной программы.
-}
module Monads.Exit (
    MonadExit(..),
  ) where

import Data.ExitCode (ExitCode)

-- | Монада для выхода из программы с указанным кодом возврата.
class Monad m => MonadExit m where
  exit :: ExitCode -> m ()

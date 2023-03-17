{- |
В данном модуле объявлена монада @'MonadPwdReader'@ для записи значения
переменной PWD.
-}
module Monads.PwdWriter (
    MonadPwdWriter (..),
  ) where

import Data.FSObjects (AbsFilePath)

-- | Монада для записи абсолютного пути до директории в переменную PWD.
class Monad m => MonadPwdWriter m where
  setVarPwd :: AbsFilePath -> m ()

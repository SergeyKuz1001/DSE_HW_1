{- |
В данном модуле объявлена монада @'MonadPwdReader'@ для записи значения
переменной PWD.
-}
module Environment.MonadPwdWriter (
    MonadPwdWriter (..),
  ) where

import Environment.FSPrimitive (AbsFilePath)

-- | Монада для записи абсолютного пути до директории в переменную PWD.
class Monad m => MonadPwdWriter m where
  setVarPwd :: AbsFilePath -> m ()

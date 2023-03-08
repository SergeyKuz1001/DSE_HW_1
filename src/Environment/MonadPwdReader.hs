{- |
В данном модуле объявлена монада @'MonadPwdReader'@ для получения значения
переменной PWD.
-}
module Environment.MonadPwdReader (
    MonadPwdReader (..),
  ) where

import Environment.FSPrimitive (AbsFilePath)

-- | Монада для получения значения переменной PWD (то есть абсолютного пути до
-- текущей директории).
class Monad m => MonadPwdReader m where
  getVarPwd :: m AbsFilePath

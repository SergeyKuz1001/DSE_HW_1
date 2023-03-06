{- |
В данном модуле объявлена монада @'MonadVarsWriter'@ для записи значений
переменных.
-}
module Environment.MonadVarsWriter (
    module Environment.MonadVarPathWriter,
    module Environment.MonadVarPwdWriter,
    MonadVarsWriter (..),
    setVarPathDefault,
    setVarPwdDefault,
  ) where

import Environment.MonadVarPathWriter
import Environment.MonadVarPwdWriter

import Environment.MonadFS.Internal (AbsFilePath(..))

-- | Монада для записи значений переменных.
class (MonadVarPathWriter m, MonadVarPwdWriter m) => MonadVarsWriter m where
  -- | Запись значения переменной по её имени.
  setVar :: String -> String -> m ()

-- | Функция @'setVarPath'@, выраженная через функции монады
-- @'MonadVarsWriter'@.
setVarPathDefault :: MonadVarsWriter m => [AbsFilePath] -> m ()
setVarPathDefault = setVar "PATH" . formatVarPath

-- | Функция @'setVarPwd'@, выраженная через функции монады @'MonadVarsWriter'@.
setVarPwdDefault :: MonadVarsWriter m => AbsFilePath -> m ()
setVarPwdDefault = setVar "PWD" . asFilePath

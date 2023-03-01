{- |
В данном модуле объявлена монада @'MonadVarsReader'@ для получения значений
переменных.
-}
module Environment.MonadVarsReader (
    module Environment.MonadVarPathReader,
    module Environment.MonadVarPwdReader,
    MonadVarsReader (..),
    getVarPathDefault,
    getVarPwdDefault,
  ) where

import Environment.MonadVarPathReader
import Environment.MonadVarPwdReader

import Environment.MonadFS.Internal (AbsFilePath(..))

-- | Монада для получения значений переменных.
class (MonadVarPathReader m, MonadVarPwdReader m) => MonadVarsReader m where
  -- | Получение значения переменной по имени. Если переменной с таким именем
  -- нет, то необходимо вернуть пустую строку.
  getVar :: String -> m String
  -- | Получение значений всех объявленных переменных в виде списка пар (имя,
  -- значение).
  getVars :: m [(String, String)]

-- | Функция @'getVarPath'@, выраженная через функции монады
-- @'MonadVarsReader'@.
getVarPathDefault :: MonadVarsReader m => m [AbsFilePath]
getVarPathDefault = parseVarPath <$> getVar "PATH"

-- | Функция @'getVarPwd'@, выраженная через функции монады @'MonadVarsReader'@.
getVarPwdDefault :: MonadVarsReader m => m AbsFilePath
getVarPwdDefault = AbsFilePath <$> getVar "PWD"

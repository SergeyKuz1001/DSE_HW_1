{- |
В данном модуле объявлена монада @'MonadVarReader'@ для получения значений
переменных.
-}
module Monads.VarReader (
    module Monads.PathReader,
    module Monads.PwdReader,
    MonadVarReader (..),
    getVarPathDefault,
    getVarPwdDefault,
  ) where

import Monads.PathReader
import Monads.PwdReader

import Data.Variable (Variable(..), Stable, varPath, varPwd)
import Data.FSObjects (AbsFilePath, absFilePath)
import Monads.Error (MonadError)

-- | Монада для получения значений переменных.
class (MonadPathReader m, MonadPwdReader m) => MonadVarReader m where
  -- | Получение значения переменной по имени. Если переменной с таким именем
  -- нет, то необходимо вернуть пустую строку.
  getVar :: Variable -> m String
  -- | Получение значений всех стабильных переменных в виде списка пар (имя,
  -- значение).
  getVars :: m [(Stable, String)]

-- | Функция @'getVarPath'@, выраженная через функции монады @'MonadVarReader'@.
getVarPathDefault :: (MonadVarReader m, MonadError m) => m [AbsFilePath]
getVarPathDefault = getVar (Stable varPath) >>= parseVarPath

-- | Функция @'getVarPwd'@, выраженная через функции монады @'MonadVarReader'@.
getVarPwdDefault :: (MonadVarReader m, MonadError m) => m AbsFilePath
getVarPwdDefault = getVar (Stable varPwd) >>= absFilePath

{- |
В данном модуле объявлена монада @'MonadVarWriter'@ для записи значений
переменных.
-}
module Monads.VarWriter (
    module Monads.PathWriter,
    module Monads.PwdWriter,
    MonadVarWriter (..),
    setVarPathDefault,
    setVarPwdDefault,
  ) where

import Monads.PathWriter
import Monads.PwdWriter

import Data.Variable (Stable, varPath, varPwd)
import Data.FSObjects (AbsFilePath (asFilePath))

-- | Монада для записи значений переменных.
class (MonadPathWriter m, MonadPwdWriter m) => MonadVarWriter m where
  -- | Запись значения стабильной переменной.
  setVar :: Stable -> String -> m ()

-- | Функция @'setVarPath'@, выраженная через функции монады @'MonadVarWriter'@.
setVarPathDefault :: MonadVarWriter m => [AbsFilePath] -> m ()
setVarPathDefault absPaths = setVar varPath $ formatVarPath absPaths

-- | Функция @'setVarPwd'@, выраженная через функции монады @'MonadVarWriter'@.
setVarPwdDefault :: MonadVarWriter m => AbsFilePath -> m ()
setVarPwdDefault absPath = setVar varPwd $ asFilePath absPath

{- |
В данном модуле объявлена монада @'MonadVarWriter'@ для записи значений
переменных.
-}
module Environment.MonadVarWriter (
    module Environment.MonadPathWriter,
    module Environment.MonadPwdWriter,
    MonadVarWriter (..),
    setVarPathDefault,
    setVarPwdDefault,
  ) where

import Data.Variable (Stable, varPath, varPwd)
import Environment.FSPrimitive (AbsFilePath(..))
import Environment.MonadError (Error(..), MonadError, (@:))
import Environment.MonadPathWriter
import Environment.MonadPwdWriter

-- | Монада для записи значений переменных.
class (MonadPathWriter m, MonadPwdWriter m) => MonadVarWriter m where
  -- | Запись значения стабильной переменной.
  setVar :: Stable -> String -> m ()

-- | Функция @'setVarPath'@, выраженная через функции монады @'MonadVarWriter'@.
setVarPathDefault :: (MonadVarWriter m, MonadError m) => [AbsFilePath] -> m ()
setVarPathDefault absPaths = setVar varPath $ formatVarPath absPaths

-- | Функция @'setVarPwd'@, выраженная через функции монады @'MonadVarWriter'@.
setVarPwdDefault :: (MonadVarWriter m, MonadError m) => AbsFilePath -> m ()
setVarPwdDefault absPath = setVar varPwd $ asFilePath absPath

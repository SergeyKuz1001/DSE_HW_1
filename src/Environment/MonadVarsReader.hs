module Environment.MonadVarsReader (
    module Environment.MonadVarPathReader,
    module Environment.MonadVarPwdReader,
    MonadVarsReader,
    getVar,
    getVars,
    getVarPathDefault,
    getVarPwdDefault,
  ) where

import Environment.MonadVarPathReader
import Environment.MonadVarPwdReader

import Environment.MonadFS.Internal (AbsFilePath(..))

class (MonadVarPathReader m, MonadVarPwdReader m) => MonadVarsReader m where
  getVar :: String -> m String
  getVars :: m [(String, String)]

getVarPathDefault :: MonadVarsReader m => m [AbsFilePath]
getVarPathDefault = parseVarPath <$> getVar "PATH"

getVarPwdDefault :: MonadVarsReader m => m AbsFilePath
getVarPwdDefault = AbsFilePath <$> getVar "PWD"

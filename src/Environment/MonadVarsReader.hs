module Environment.MonadVarsReader (
    module Environment.MonadVarsReader.MonadVarPathReader,
    module Environment.MonadVarsReader.MonadVarPwdReader,
    MonadVarsReader,
    getVar,
    getVarPathDefault,
    getVarPwdDefault,
  ) where

import Environment.MonadVarsReader.MonadVarPathReader
import Environment.MonadVarsReader.MonadVarPwdReader

import Environment.MonadFS.Internal (AbsFilePath(..))

class (MonadVarPathReader m, MonadVarPwdReader m) => MonadVarsReader m where
  getVar :: String -> m String
  getVars :: m [(String, String)]

getVarPathDefault :: MonadVarsReader m => m [AbsFilePath]
getVarPathDefault = parseVarPath <$> getVar "PATH"

getVarPwdDefault :: MonadVarsReader m => m AbsFilePath
getVarPwdDefault = AbsFilePath <$> getVar "PWD"

module Environment.MonadVarsReader (
  ) where

import Environment.MonadVarsReader.MonadVarPathReader
import Environment.MonadVarsReader.MonadVarPwdReader

import Environment.MonadFS.Internal (AbsFilePath(..))

class (MonadVarPathReader m, MonadVarPwdReader m) => MonadVarsReader m where
  getVar :: String -> m String

getVarPathDefault :: MonadVarsReader m => m [AbsFilePath]
getVarPathDefault = parseVarPath <$> getVar "PATH"

getVarPwdDefault :: MonadVarsReader m => m AbsFilePath
getVarPwdDefault = AbsFilePath <$> getVar "PWD"

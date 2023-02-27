module Environment.MonadVarsReader.MonadVarPwdReader (
    MonadVarPwdReader,
    getVarPwd,
  ) where

import Environment.MonadFS (AbsFilePath)

class Monad m => MonadVarPwdReader m where
  getVarPwd :: m AbsFilePath

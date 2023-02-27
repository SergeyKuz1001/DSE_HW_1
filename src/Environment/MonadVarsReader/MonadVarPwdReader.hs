module Environment.MonadVarsReader.MonadVarPwdReader (
  ) where

import Environment.MonadFS (AbsFilePath)

class Monad m => MonadVarPwdReader m where
  getVarPwd :: m AbsFilePath

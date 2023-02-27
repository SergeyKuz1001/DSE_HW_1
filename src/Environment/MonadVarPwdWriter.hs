module Environment.MonadVarPwdWriter (
    MonadVarPwdWriter,
    setVarPwd,
  ) where

import Environment.MonadFS (AbsFilePath)

class Monad m => MonadVarPwdWriter m where
  setVarPwd :: AbsFilePath -> m ()

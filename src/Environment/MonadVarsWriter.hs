module Environment.MonadVarsWriter (
    module Environment.MonadVarsWriter.MonadVarPathWriter,
    module Environment.MonadVarsWriter.MonadVarPwdWriter,
    MonadVarsWriter,
    setVar,
    setVarPathDefault,
    setVarPwdDefault,
  ) where

import Environment.MonadVarsWriter.MonadVarPathWriter
import Environment.MonadVarsWriter.MonadVarPwdWriter

import Environment.MonadFS.Internal (AbsFilePath(..))

class (MonadVarPathWriter m, MonadVarPwdWriter m) => MonadVarsWriter m where
  setVar :: String -> String -> m ()

setVarPathDefault :: MonadVarsWriter m => [AbsFilePath] -> m ()
setVarPathDefault = setVar "PATH" . formatVarPath

setVarPwdDefault :: MonadVarsWriter m => AbsFilePath -> m ()
setVarPwdDefault = setVar "PWD" . asFilePath

module Environment.MonadVarsWriter.MonadVarPathWriter (
    MonadVarPathWriter,
    setVarPath,
    formatVarPath,
  ) where

import Environment.MonadFS.Internal (AbsFilePath(..))
import Environment.MonadVarsReader.MonadVarPathReader (varPathSeparator)

class Monad m => MonadVarPathWriter m where
  setVarPath :: [AbsFilePath] -> m ()

formatVarPath :: [AbsFilePath] -> String
formatVarPath [] = ""
formatVarPath afps = foldr1 (\x y -> x ++ pure varPathSeparator ++ y) $ map asFilePath afps

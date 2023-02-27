module Environment.MonadVarsReader.MonadVarPathReader (
    MonadVarPathReader,
    getVarPath,
    varPathSeparator,
    parseVarPath,
  ) where

import Environment.MonadFS.Internal (AbsFilePath(..))

import Data.List (unfoldr)
import System.FilePath (pathSeparator)

class Monad m => MonadVarPathReader m where
  getVarPath :: m [AbsFilePath]

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy sep = unfoldr $ (\(p, ps) -> if null p then Nothing else Just (p, drop 1 ps)) . span (/= sep)

varPathSeparator :: Char
varPathSeparator = if pathSeparator == '/' then ':' else ';'

parseVarPath :: String -> [AbsFilePath]
parseVarPath paths = AbsFilePath <$> splitBy varPathSeparator paths

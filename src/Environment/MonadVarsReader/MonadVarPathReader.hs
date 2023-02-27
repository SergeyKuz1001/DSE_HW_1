module Environment.MonadVarsReader.MonadVarPathReader (
    MonadVarPathReader,
    getVarPath,
    parseVarPath,
  ) where

import Environment.MonadFS.Internal (AbsFilePath(..))

class Monad m => MonadVarPathReader m where
  getVarPath :: m [AbsFilePath]

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy sep = unfoldr $ (\(p, ps) -> if null p then Nothing else Just (p, drop 1 ps)) . span (/= sep)

parseVarPath :: String -> [AbsFilePath]
parseVarPath "" = []
parseVarPath paths = AbsFilePath <$> splitBy (if head paths == '/' then ':' else ';') paths

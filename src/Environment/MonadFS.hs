module Environment.MonadFS (
    AbsFilePath,
    Permissions,
    File,
    MonadFS,
    findFile,
    findFileAsExecutable,
    parsePATHvariable,
    doesFileExist,
    doesExecutableExist,
    isReadable,
    isWritable,
    isExecutable,
  ) where

import Environment.MonadFS.Internal

import Data.List (unfoldr)

class Monad m => MonadFS m where
  findFile :: FilePath -> m (Maybe File)
  findFile = findFileAsExecutable []
  findFileAsExecutable :: [AbsFilePath] -> FilePath -> m (Maybe File)
  findFileAsExecutable _ = findFile

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy sep = unfoldr $ (\(p, ps) -> if null p then Nothing else Just (p, drop 1 ps)) . span (/= sep)

parsePATHvariable :: String -> [AbsFilePath]
parsePATHvariable "" = []
parsePATHvariable paths = AbsFilePath <$> splitBy (if head paths == '/' then ':' else ';') paths

doesFileExist :: MonadFS m => FilePath -> m (Maybe AbsFilePath)
doesFileExist path = fmap absFilePath <$> findFile path

doesExecutableExist :: MonadFS m => [AbsFilePath] -> FilePath -> m (Maybe AbsFilePath)
doesExecutableExist paths path = do
  mFile <- findFileAsExecutable paths path
  if fmap (execPerm . permissions) mFile == Just True
    then return $ absFilePath <$> mFile
    else return Nothing

hasPerm :: MonadFS m => (Permissions -> Bool) -> AbsFilePath -> m Bool
hasPerm p (AbsFilePath path) = (\mFile -> fmap (p . permissions) mFile == Just True) <$> findFile path

isReadable :: MonadFS m => AbsFilePath -> m Bool
isReadable = hasPerm readPerm

isWritable :: MonadFS m => AbsFilePath -> m Bool
isWritable = hasPerm writePerm

isExecutable :: MonadFS m => AbsFilePath -> m Bool
isExecutable = hasPerm execPerm

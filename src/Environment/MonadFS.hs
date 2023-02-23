module Environment.MonadFS (
    AbsFilePath,
    Permissions,
    File,
    MonadFS,
    findFile,
    findFileAsExecutable,
    doesFileExist,
    doesExecutableExist,
    isReadable,
    isWritable,
    isExecutable,
  ) where

import Environment.MonadFS.Internal

class Monad m => MonadFS m where
  findFile :: FilePath -> m (Maybe File)
  findFile = findFileAsExecutable
  findFileAsExecutable :: FilePath -> m (Maybe File)
  findFileAsExecutable = findFile

doesFileExist :: MonadFS m => FilePath -> m (Maybe AbsFilePath)
doesFileExist path = fmap absFilePath <$> findFile path

doesExecutableExist :: MonadFS m => FilePath -> m (Maybe AbsFilePath)
doesExecutableExist path = do
  mFile <- findFileAsExecutable path
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

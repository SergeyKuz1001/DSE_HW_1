module Environment.MonadFS (
    AbsFilePath,
    Permissions,
    File,
    MonadFS,
    findFile,
    doesFileExist,
    isReadable,
    isWritable,
    isExecutable,
  ) where

import Environment.MonadFS.Internal

import Data.Maybe (isJust)

class Monad m => MonadFS m where
  findFile :: FilePath -> m (Maybe File)

doesFileExist :: MonadFS m => FilePath -> m (Maybe AbsFilePath)
doesFileExist path = fmap absFilePath <$> findFile path

hasPerm :: MonadFS m => (Permissions -> Bool) -> AbsFilePath -> m Bool
hasPerm p (AbsFilePath path) = (\mFile -> fmap (p . permissions) mFile == Just True) <$> findFile path

isReadable :: MonadFS m => AbsFilePath -> m Bool
isReadable = hasPerm readPerm

isWritable :: MonadFS m => AbsFilePath -> m Bool
isWritable = hasPerm writePerm

isExecutable :: MonadFS m => AbsFilePath -> m Bool
isExecutable = hasPerm execPerm

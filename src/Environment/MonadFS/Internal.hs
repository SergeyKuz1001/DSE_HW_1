module Environment.MonadFS.Internal (
    AbsFilePath(..),
    Permissions(..),
    File(..),
  ) where

newtype AbsFilePath = AbsFilePath { asFilePath :: FilePath }
  deriving (Eq, Show)

data Permissions = Permissions
  { readPerm  :: Bool
  , writePerm :: Bool
  , execPerm  :: Bool
  }

data File = File
  { absFilePath :: AbsFilePath
  , permissions :: Permissions
  }

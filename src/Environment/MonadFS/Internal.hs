module Environment.MonadFS.Internal (
    AbsFilePath(..),
    (</>),
    Permissions(..),
    File(..),
  ) where

import qualified System.FilePath as FP

newtype AbsFilePath = AbsFilePath { asFilePath :: FilePath }
  deriving Eq

instance Show AbsFilePath where
  show (AbsFilePath filePath) = filePath

infixl 6 </>
(</>) :: AbsFilePath -> FilePath -> AbsFilePath
(</>) (AbsFilePath filePath) = AbsFilePath . (filePath FP.</>)

data Permissions = Permissions
  { readPerm  :: Bool
  , writePerm :: Bool
  , execPerm  :: Bool
  }

data File = File
  { absFilePath :: AbsFilePath
  , permissions :: Permissions
  }

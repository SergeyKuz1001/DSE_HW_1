module Data.Handles (
    InputHandle(..),
    OutputHandle(..),
  ) where

import Environment.FSPrimitive (AbsFilePath, asFilePath)

import System.IO (Handle, IOMode, openFile)
import System.Process (StdStream(..))
import qualified System.Process as PRC

data InputHandle
  = FromParentHandle
  | FromFile AbsFilePath
  | FromString String
  deriving (Eq, Show)

data OutputHandle
  = ToNewPipe
  | ToStdout
  | ToNowhere
  deriving (Eq, Show)

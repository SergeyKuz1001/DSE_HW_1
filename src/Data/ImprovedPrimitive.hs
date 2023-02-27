module Data.ImprovedPrimitive (
    Primitive(..),
    Command(..),
    Special(..),
    Common(..),
    Internal(..),
    External(..),
  ) where

import Environment.MonadFS.Internal (AbsFilePath)

data Primitive = Command Command | EmptyCommand
  deriving (Eq, Show)

data Command = Special Special | Common Common
  deriving (Eq, Show)

data Special = Exit (Maybe Int)
  deriving (Eq, Show)

data Common = Internal Internal | External External
  deriving (Eq, Show)

data Internal = Cat AbsFilePath | Echo [String] | Wc AbsFilePath | Pwd
  deriving (Eq, Show)

data External = Arguments AbsFilePath [String]
  deriving (Eq, Show)

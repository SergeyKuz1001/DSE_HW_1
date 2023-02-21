module Data.ImprovedPrimitive (
    Primitive(..),
    Command(..),
    Special(..),
    Common(..),
    Internal(..),
    External(..),
  ) where

data Primitive = Command Command
  deriving (Eq, Show)

data Command = Special Special | Common Common
  deriving (Eq, Show)

data Special = Exit (Maybe Int)
  deriving (Eq, Show)

data Common = Internal Internal | External External
  deriving (Eq, Show)

data Internal = Cat FilePath | Echo [String] | Wc FilePath | Pwd
  deriving (Eq, Show)

data External = Arguments FilePath [String]
  deriving (Eq, Show)

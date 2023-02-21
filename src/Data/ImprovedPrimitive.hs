module Data.ImprovedPrimitive (
    Primitive(..),
    Command(..),
    Special(..),
    Common(..),
    Internal(..),
    External(..),
  ) where

data Primitive = Command Command

data Command = Special Special | Common Common

data Special = Exit (Maybe Int)

data Common = Internal Internal | External External

data Internal = Cat FilePath | Echo [String] | Wc FilePath | Pwd

data External = Arguments FilePath [String]

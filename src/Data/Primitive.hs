module Data.Primitive (
    Primitive(..),
    Command(..),
  ) where

data Primitive = Command Command

data Command = Arguments [String]

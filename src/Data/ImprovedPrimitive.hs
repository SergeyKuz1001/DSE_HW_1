module Data.ImprovedPrimitive (
    Primitive(..),
    Command(..),
    Special(..),
    Common(..),
    Internal(..),
    External(..),
    Global(..),
    Local(..),
  ) where

import Data.List.NonEmpty (NonEmpty(..))

data Primitive = Command Command

data Command = Special Special | Common Common

data Special = Exit (Maybe Int)

data Common = Internal Internal | External External

data Internal = Echo [String] | Cat FilePath -- | ...

data External = Global Global | Local Local

type Global = NonEmpty String

type Local = NonEmpty String

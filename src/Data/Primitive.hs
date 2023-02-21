module Data.Primitive (
    Primitive(..),
    Command(..),
  ) where

import Data.List.NonEmpty (NonEmpty(..))

data Primitive = Command Command

type Command = NonEmpty String

module Data.Primitive (
    Primitive(..),
    Command(..),
  ) where

import Data.List.NonEmpty (NonEmpty(..))

data Primitive = Command Command
  deriving (Eq, Show)

type Command = NonEmpty String

module Data.LinkedPrimitive (
    Primitive (..),
    Special (..),
    CommandWithHandles,
    Command (..),
    Internal (..),
    External (..),
  ) where

import Data.Variable (Stable(..))
import Data.AnalyzedPrimitive (Special(..), External(..))
import Data.Handles

data Primitive
  = Special Special
  | Assignment Stable String
  | Commands [CommandWithHandles]
  deriving (Eq, Show)

type CommandWithHandles = (Command, InputHandle, OutputHandle)

data Command = External External | Internal Internal
  deriving (Eq, Show)

data Internal = Func String (String -> String)

instance Eq Internal where
  Func name1 _ == Func name2 _ = name1 == name2

instance Show Internal where
  show (Func name _)    = "Func(" ++ name ++ ")"

module Data.Primitive (
    VarName,
    Primitive (..),
  ) where

import Data.Primitive.Internal

import Data.List.NonEmpty (NonEmpty (..))

data Primitive
  = Command (NonEmpty String)
  | Assignment VarName String
  | EmptyCommand
  deriving (Eq, Show)

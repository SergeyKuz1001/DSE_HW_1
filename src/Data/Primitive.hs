module Data.Primitive
  ( Primitive (..),
  )
where

import Data.List.NonEmpty (NonEmpty (..))

data Primitive
  = Command (NonEmpty String)
  | Assignment String String
  | EmptyCommand
  deriving (Eq, Show)

module Data.Primitive
  ( Primitive (..),
  )
where

import           Data.List.NonEmpty (NonEmpty (..))

data Primitive
  = Command [String]
  | Assignment String String
  deriving (Eq, Show)

{- |
В данном модуле объявлены основные примитивы, в которые транслируется
пользовательский запрос после его парсинга.
-}
module Data.Primitive (
    VarName (getVarName),
    Primitive (..),
  ) where

import Data.Primitive.Internal

import Data.List.NonEmpty (NonEmpty (..))

-- | Примитив, полученный после парсинга пользовательского запроса.
data Primitive
  = Command (NonEmpty String)
  | Assignment VarName String
  | EmptyCommand
  deriving (Eq, Show)

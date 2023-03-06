{- |
В данном модуле объявлены основные примитивы, в которые транслируется
пользовательский запрос после его парсинга.
-}
module Data.Primitive (
    VarName (getVarName),
    Primitive (..),
  ) where

import Data.Primitive.Internal

-- | Примитив, полученный после парсинга пользовательского запроса.
data Primitive
  = Command [String]
  | Assignment VarName String
  deriving (Eq, Show)

{- |
В данном модуле объявлены основные примитивы, в которые транслируется
пользовательский запрос после его парсинга.
-}
module Data.Primitive (
    Primitive (..),
  ) where

-- | Примитив, полученный после парсинга пользовательского запроса.
data Primitive
  = Commands [[String]]
  | Assignment String String
  deriving (Eq, Show)

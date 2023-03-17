{- |
В данном модуле объявлены примитивы, в которые транслируется пользовательский
запрос после линковки.
-}
module Data.LinkedPrimitive (
    Primitive (..),
    Special (..),
    CommonWithHandles,
    Common (..),
    Internal (..),
    External (..),
  ) where

import Data.AnalyzedPrimitive (Special(..), External(..))
import Data.Handles
import Data.Variable (Stable(..))

import Data.Text.Lazy (Text)

-- | Примитив — это
--
--     * специальная команда,
--     * присваивание стабильной переменной или
--     * набор обычных команд с заданными типами потоков ввода и вывода.
data Primitive
  = Special Special
  | Assignment Stable String
  | Commons [CommonWithHandles]
  deriving (Eq, Show)

-- | Обычная команда с заданными типами потоков ввода и вывода.
type CommonWithHandles = (Common, InputHandle, OutputHandle)

-- | Обычная команда, может быть внутренней или внешней.
data Common = External External | Internal Internal
  deriving (Eq, Show)

-- | Внутренняя команда, представляется в виде чистой функции над текстом с её
-- названием. Название необходимо для сравнения двух функций. Обычно
-- выполняется, что две команды с одинковым названием имеют эквивалентные
-- функции, но этот инвариант никак не гарантируется.
data Internal = Func String (Text -> Text)

instance Eq Internal where
  Func name1 _ == Func name2 _ = name1 == name2

instance Show Internal where
  show (Func name _)    = "Func(" ++ name ++ ")"

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
    Impure (..),
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

-- | Внутренняя команда, представляется либо в виде чистой функции над текстом
-- с её названием, либо в виде нечистой команды.
--
-- Название в чистой функции необходимо для сравнения двух функций. Обычно
-- выполняется инвариант, что две команды с одинковым названием имеют
-- эквивалентные функции, но этот инвариант никак не гарантируется.
data Internal
  = Pure String (Text -> Text)
  | Impure Impure

instance Eq Internal where
  Pure name1 _ == Pure name2 _ = name1 == name2
  Impure imp1  == Impure imp2  = imp1  == imp2
  _            == _            = False

instance Show Internal where
  show (Pure name _) = "Pure (" ++ name ++ ")"
  show (Impure imp)  = show imp

-- | Внутренняя нечистая команда, пока ею является только @pwd@ (требует доступ
-- к классу @'MonadPwdReader'@).
--
-- Важно: пока предполагается, что любая нечистая команда не модифицирует
-- реальное окружение (не изменяет файлы, не делает что-либо на удалённом
-- сервере и т. д.), это используется как один из неявных инвариантов в работе
-- @'linker'@.
data Impure = Pwd
  deriving (Eq, Show)

{- |
В данном модуле объявлены примитивы, в которые транслируется пользовательский
запрос после этапа анализа.
-}
module Data.AnalyzedPrimitive (
    Primitive (..),
    Special (..),
    Common (..),
    Internal (..),
    External (..),
  ) where

import Data.Variable (Stable)
import Data.FSObjects (AbsFilePath)
import Data.ExitCode (ExitCode)

import Data.List.NonEmpty (NonEmpty)

-- | Примитив — это
--
--     * специальная команда (@exit@ или @cd@),
--     * непустой набор обычных команд, разделённых pipeами,
--     * присваивание стабильной переменной или
--     * пустая команда.
data Primitive
  = Special Special
  | Commons (NonEmpty Common)
  | Assignment Stable String
  | Empty
  deriving (Eq, Show)

-- | Специальная команда влияет на работу оболочки и может быть
--
--     * @exit@ — завершение работы оболочки с кодом возврата, принимаемым
--         опциональным параметром
--     * @cd@ — смена текущей директории
data Special = Exit (Maybe ExitCode) | Cd FilePath
  deriving (Eq, Show)

-- | Обычная команда имеет потоки ввода, вывода и поток ошибок.
-- Может быть либо внутренней (исполняется самой командной оболочкой), либо
-- внешней.
data Common = Internal Internal | External External
  deriving (Eq, Show)

-- | Внутренняя команда исполняется самой командной оболочкой и может быть
--
--     * @cat@ — вывод содержимого переданного файла;
--     * @echo@ — вывод аргументов через пробел;
--     * @wc@ — статистика для файла;
--     * @pwd@ — имя текущей директории.
data Internal
  = Cat (Maybe AbsFilePath)
  | Echo [String]
  | Wc (Maybe AbsFilePath)
  | Pwd
  deriving (Eq, Show)

-- | Внешняя команда вызывается по пути к исполняемому файлу с указанными
-- аргументами.
data External = Arguments AbsFilePath [String]
  deriving (Eq, Show)

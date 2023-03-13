{- |
В данном модуле объявлены примитивы, в которые транслируется пользовательский
запрос после этапа анализа.
-}
module Data.ImprovedPrimitive (
    Primitive (..),
    Special (..),
    Common (..),
    Internal (..),
    Blocking (..),
    Streaming (..),
    External (..),
  ) where

import Data.Variable (Stable)
import Environment.FSPrimitive (AbsFilePath)
import Environment.MonadExit (ExitCode)

import Data.List.NonEmpty (NonEmpty)

-- | Примитив является либо специальной командой, либо набором обычных,
-- разделённых pipeами, либо присваиванием стабильной переменной, либо пустой
-- командой.
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
--
-- Мы разделяем команды на блокирующие и потоковые по тому, могут ли они
-- запускаться с другими параллельно.
data Internal = Blocking Blocking | Streaming Streaming
  deriving (Eq, Show)

-- | Единственная блокирующая команда - это @wc@ без параметра, так как
-- необходимо подождать исполнение последней команды чтобы вычислить статистику
-- её вывода.
data Blocking = WcStdin
  deriving (Eq, Show)

-- | Обычно все внутренние команды являются потоковыми, то есть их работу мы
-- делаем во время запуска из-за быстроты их исполнения (не считая @wc@ с
-- большим файлом в качестве параметра, но и блокирующей эту команду назвать
-- нельзя).
data Streaming = Cat (Maybe AbsFilePath) | Echo [String] | WcFile AbsFilePath | Pwd
  deriving (Eq, Show)

-- | Внешняя команда вызывается по пути к исполняемому файлу с указанными
-- аргументами.
data External = Arguments AbsFilePath [String]
  deriving (Eq, Show)

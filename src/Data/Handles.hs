{- |
В этом модуле определены типы потоков ввода и вывода.
-}
module Data.Handles (
    InputHandle(..),
    OutputHandle(..),
  ) where

import Data.FSObjects (AbsFilePath)

-- | Тип типа потока ввода. Ввод бывает
--
--     * из потока, предоставляемого предыдущей командой;
--     * из файла;
--     * из строки.
--
-- Заметим, что ввод из стандартного потока ввода мы считаем частным случаем
-- ввода из потока, предоставляемого предыдущей командой.
data InputHandle
  = FromParentHandle
  | FromFile AbsFilePath
  | FromString String
  deriving (Eq, Show)

-- | Тип типа потока вывода. Вывод бывает
--
--     * в новый pipe;
--     * в стандартный поток вывода;
--     * вникуда (аналог @/dev/null@).
data OutputHandle
  = ToNewPipe
  | ToStdout
  | ToNowhere
  deriving (Eq, Show)

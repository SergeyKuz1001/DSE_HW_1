{- |
Модуль с определениями внутренних команд.
-}
module Phases.Linker.Commands (
    cat,
    echo,
    wc,
  ) where

import Data.Bool (bool)
import qualified Data.ByteString.Char8 as BS
import Data.Char (isSpace)

-- | Команда @cat@ принимает текст на вход и возвращает его.
cat :: String -> String
cat = id

-- | Команда @echo@ принимает набор строк-аргументов и возвращает их запись
-- через пробел.
echo :: [String] -> String
echo = (++ "\n") . unwords

-- | Команда @wc@ принимает текст и возвращает его статистику (тоже в виде
-- текста).
wc :: String -> String
wc text =
  let bytes = BS.pack text
      checkOnLine c = bool 0 1 $ c == '\n'
      checkOnWord c isPS = bool 0 1 $ not isPS && isSpace c
      wcgo (cLs, cWs, cBs, isPS) c =
        (cLs + checkOnLine c, cWs + checkOnWord c isPS, cBs + 1, isSpace c)
      (countLines, countWords, countBytes, isPrevSpace) =
        BS.foldl' wcgo (0 :: Int, 0 :: Int, 0 :: Int, False) bytes
      countWords' = countWords + bool 1 0 isPrevSpace
  in  show countLines ++ "\t" ++ show countWords' ++ "\t" ++ show countBytes ++ "\n"

-- Команды @pwd@ здесь нет так как
--
--   1. @pwd@ - не чистая команда (требует @'MonadsPwdReader'@)
--   2. мы считаем её эквивалентной @echo@ с одним аргументом

{- |
Модуль с определениями внутренних команд.
-}
module Phases.Linker.Commands (
    cat,
    echo,
    wc,
  ) where

import Data.Bool (bool)
import qualified Data.ByteString.Lazy as BS
import Data.Char (isSpace, chr)
import Data.Text.Lazy (Text, pack)
import Data.Text.Lazy.Encoding (encodeUtf8)

-- | Команда @cat@ принимает текст на вход и возвращает его.
cat :: Text -> Text
cat = id

-- | Команда @echo@ принимает набор строк-аргументов и возвращает их запись
-- через пробел.
echo :: [String] -> Text
echo = pack . (++ "\n") . unwords

-- | Команда @wc@ принимает текст и возвращает его статистику (тоже в виде
-- текста).
wc :: Text -> Text
wc text =
  let bytes = encodeUtf8 text
      checkOnLine c = bool 0 1 $ c == '\n'
      checkOnWord c isPS = bool 0 1 $ not isPS && isSpace c
      wcgo (cLs, cWs, cBs, isPS) w =
        let c = chr $ fromIntegral w
        in  (cLs + checkOnLine c, cWs + checkOnWord c isPS, cBs + 1, isSpace c)
      (countLines, countWords, countBytes, isPrevSpace) =
        BS.foldl' wcgo (0 :: Int, 0 :: Int, 0 :: Int, False) bytes
      countWords' = countWords + bool 1 0 isPrevSpace
  in  pack $
        "\t" ++ show countLines ++ "\t" ++ show countWords' ++ "\t" ++ show countBytes ++ "\n"

-- Команды @pwd@ здесь нет так как
--
--   1. @pwd@ - не чистая команда (требует @'MonadsPwdReader'@)
--   2. мы считаем её эквивалентной @echo@ с одним аргументом

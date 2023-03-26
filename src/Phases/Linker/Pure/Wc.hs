{- |
Модуль с внутренней чистой командой @wc@.
-}
module Phases.Linker.Pure.Wc(
    wc,
  ) where

import qualified Data.ByteString.Lazy as BS
import Data.Text.Lazy (Text, pack)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (encodeUtf8)

-- | Команда @wc@ принимает текст и возвращает его статистику (тоже в виде
-- текста).
wc :: Text -> Text
wc text =
  let bytes = encodeUtf8
  in  pack $
        "\t" ++ show (length $  T.lines text) ++
        "\t" ++ show (length $  T.words text) ++
        "\t" ++ show (BS.length $ bytes text) ++ "\n"

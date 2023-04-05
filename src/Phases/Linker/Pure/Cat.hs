{- |
Модуль с внутренней чистой командой @cat@.
-}
module Phases.Linker.Pure.Cat(
    cat,
  ) where

import Data.Text.Lazy (Text)

-- | Команда @cat@ принимает текст на вход и возвращает его.
cat :: Text -> Text
cat = id

{- |
Модуль с внутренней чистой командой @echo@.
-}
module Phases.Linker.Pure.Echo(
    echo,
  ) where

import Data.Text.Lazy (Text, pack)

-- | Команда @echo@ принимает набор строк-аргументов и возвращает их запись
-- через пробел.
echo :: [String] -> Text
echo = pack . (++ "\n") . unwords

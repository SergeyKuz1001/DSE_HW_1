{- |
Модуль для определения типа стандартной ошибки. Этот тип используется в качестве
ошибки на протяжении всего исполнения программы.
-}
module Data.Error (
    Error (..),
  ) where

import System.Console.ANSI

-- | Тип стандартной (для нашей системы) ошибки. Хранит стадию, на которой эта
-- ошибка произошла, а также дополнительную информацию.
data Error = Error String String
  deriving (Eq)

instance Show Error where
  show (Error type_ msg) =
    setSGRCode [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity] ++
    type_ ++
    setSGRCode [Reset] ++ ": " ++ msg

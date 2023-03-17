{- |
Модуль для работы с кодом возврата.
-}
module Data.ExitCode (
    ExitCode (ExitCode),
    toStandardEC,
    fromStandardEC,
  ) where

import qualified System.Exit as SE

-- | Код возврата. 0 означает штатное завершение, не 0 - завершение с ошибкой.
newtype ExitCode = ExitCode Int
  deriving (Eq, Show)

-- | Функция преобразования нашего кода возврата в стандартный.
toStandardEC :: ExitCode -> SE.ExitCode
toStandardEC (ExitCode 0) = SE.ExitSuccess
toStandardEC (ExitCode x) = SE.ExitFailure x

-- | Функция преобразования стандартного кода возврата в наш.
fromStandardEC :: SE.ExitCode -> ExitCode
fromStandardEC SE.ExitSuccess = ExitCode 0
fromStandardEC (SE.ExitFailure x) = ExitCode x

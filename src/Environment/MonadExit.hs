{- |
В данном модуле объявлена монада @'MonadExit'@, предоставляющая возможность
выхода из данной программы.
-}
module Environment.MonadExit (
    ExitCode (ExitCode),
    toStandardEC,
    fromStandardEC,
    MonadExit(..),
  ) where

import qualified System.Exit as SE

-- | Код возврата. 0 означает штатное завершение, не 0 - завершение с ошибкой.
newtype ExitCode = ExitCode Int

-- | Функция преобразования нашего кода возврата в стандартный.
toStandardEC :: ExitCode -> SE.ExitCode
toStandardEC (ExitCode 0) = SE.ExitSuccess
toStandardEC (ExitCode x) = SE.ExitFailure x

-- | Функция преобразования стандартного кода возврата в наш.
fromStandardEC :: SE.ExitCode -> ExitCode
fromStandardEC SE.ExitSuccess = ExitCode 0
fromStandardEC (SE.ExitFailure x) = ExitCode x

-- | Монада для выхода из программы с указанным кодом возврата.
class Monad m => MonadExit m where
  exit :: ExitCode -> m ()

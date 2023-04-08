{-
Модуль с часто используемыми функциями в анализаторе.
-}
module Phases.Analyzer.Common (
    toError,
    findReadable,
    checkOptionalArg,
    checkRequiredArg,
    checkNoneArg,
  ) where

import Data.Error (Error(..))
import Data.FSObjects (AbsFilePath)
import Monads.Error (MonadError, throwError, (?>=), (@>=))
import Monads.FS (MonadFS, doesFileExist, isReadable)
import Monads.PwdReader (MonadPwdReader)

-- | Функция получения объекта-ошибки по информации об ошибке.
toError :: String -> Error
toError = Error "AnalyzingError"

-- | Поиск файла по пути, а также проверка наличия разрешения на чтение.
findReadable :: (MonadError m, MonadFS m, MonadPwdReader m) => FilePath -> m AbsFilePath
findReadable filePath = do
  absFilePath <- doesFileExist filePath @>= toError ("can't find file by path \"" ++ filePath ++ "\"")
  isReadable absFilePath ?>= toError ("file \"" ++ show absFilePath ++ "\" hasn't readable permission")
  return absFilePath

-- | Функция проверки количества аргументов (0 или 1).
checkOptionalArg :: MonadError m => String -> [String] -> m (Maybe String)
checkOptionalArg _ []    = return Nothing
checkOptionalArg _ [arg] = return $ Just arg
checkOptionalArg cmd _   = throwError . toError $ "command `" ++ cmd ++ "` have one optional argument"

-- | Функция проверки количества аргументов (1).
checkRequiredArg :: MonadError m => String -> [String] -> m String
checkRequiredArg _ [arg] = return arg
checkRequiredArg cmd _   = throwError . toError $ "command `" ++ cmd ++ "` have one required argument"

-- | Функция проверки количества аргументов (0).
checkNoneArg :: MonadError m => String -> [String] -> m ()
checkNoneArg _ []  = return ()
checkNoneArg cmd _ = throwError . toError $ "command `" ++ cmd ++ "` have not arguments"

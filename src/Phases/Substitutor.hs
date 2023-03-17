{- |
Модуль для подстановки значений переменных в пользовательский запрос.
-}
module Phases.Substitutor (
    substitutor
  ) where

import           Data.Variable              (readVariable)
import           Data.Error                 (Error (..))
import           Monads.Error               (MonadError, throwError)
import           Monads.VarReader           (MonadVarReader (..))

-- | Сконструировать специфичную для модуля ошибку
modError :: String -> Error
modError = Error "VarSubstututorError"

-- | Выбросить специфичную для модуля ошибку
throwModError :: MonadError m => String -> m a
throwModError = throwError . modError

-- | Функция для подстановки переменных.
-- Преобразовывает строку с $varname и ${varname}
-- в строку без них.
-- Учитываются кавычки, разбор строки не производится.
substitutor :: (MonadError m, MonadVarReader m) => String -> m String
substitutor "" = pure ""
substitutor ('\\' : c : cs)  = ('\\' :) . (c :) <$> substitutor cs
substitutor ('\'' : cs) = ('\'' :) <$> singleQuotes cs
substitutor ('$' : cs) = parseVariable substitutor cs
substitutor ('\"' : cs) = ('\"' :) <$> doubleQuotes cs
substitutor (c : cs) = (c :) <$> substitutor cs

-- | Обработка внутри одинарных кавычек.
singleQuotes :: (MonadError m, MonadVarReader m) => String -> m String
singleQuotes "" = throwModError "Unexpected end of line in single quotes"
singleQuotes ('\\' : c : cs)  = ('\\' :) . (c :) <$> singleQuotes cs
singleQuotes ('\'' : cs) = ('\'' :) <$> substitutor cs
singleQuotes (c : cs) = (c :) <$> singleQuotes cs

-- | Обработка внутри двойных кавычек.
-- Отличается тем, что одинарные не обрабатываются отдельно.
doubleQuotes :: (MonadError m, MonadVarReader m) => String -> m String
doubleQuotes "" = throwModError "Unexpected end of line in double quotes"
doubleQuotes ('\\' : c : cs)  = ('\\' :) . (c :) <$> doubleQuotes cs
doubleQuotes ('$' : cs) = parseVariable doubleQuotes cs
doubleQuotes ('\"' : cs) = ('\"' :) <$> substitutor cs
doubleQuotes (c : cs) = (c :) <$> doubleQuotes cs

-- | Парсинг имени переменной. Принимает функцию для продолжения парсинга после
-- прочтения переменной и строку, префикс которой предположительно является
-- именем переменной.
parseVariable :: (MonadError m, MonadVarReader m) => (String -> m String) -> String -> m String
parseVariable _ "" = throwModError "Empty variable name"
parseVariable retTo ('{' : cs) = case readVariable cs of
  Just (var, '}' : r) -> getVar var >>= \val -> (escapeStr val ++) <$> retTo r
  Just _              -> throwModError "Expected }"
  Nothing             -> throwModError "Empty variable name"
parseVariable retTo cs = case readVariable cs of
  Just (var, r) -> getVar var >>= \val -> (escapeStr val ++) <$> retTo r
  Nothing       -> throwModError "Empty variable name"

-- | Экранирование строки.
escapeStr :: String -> String
escapeStr ""          = ""
escapeStr ('\\' : cs) = '\\' : '\\' : escapeStr cs
escapeStr ('\"' : cs) = '\\' : '\"' : escapeStr cs
escapeStr ('\'' : cs) = '\\' : '\'' : escapeStr cs
escapeStr (c : cs)    = c : escapeStr cs

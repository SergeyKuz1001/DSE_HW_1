module Phases.VarSubstitutor (varSubstitutor) where

import           Data.Variable
import           Environment.MonadError     (Error (..), MonadError, throwError)
import           Environment.MonadVarReader
import           Phases.Parser

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
varSubstitutor :: (MonadError m, MonadVarReader m) => String -> m String
varSubstitutor "" = pure ""
varSubstitutor ('\\' : '\'' : cs) = ("\\\'" ++) <$> varSubstitutor cs
varSubstitutor ('\'' : cs) = do
  (s, r) <- singleQuotes cs
  r' <- varSubstitutor r
  pure $ "'" ++ s ++ "'" ++ r'
varSubstitutor ('$' : '{' : cs) = case readVariable cs of
  Just (var, '}' : r) -> getVar var >>= \val -> (escapeStr val ++) <$> varSubstitutor r
  Just _ -> throwModError "Expected }"
  Nothing -> throwModError "Empty variable name"
varSubstitutor ('$' : cs) = case readVariable cs of
  Just (var, r) -> getVar var >>= \val -> (escapeStr val ++) <$> varSubstitutor r
  Nothing -> throwModError "Empty variable name"

-- Отдельно обрабатывать двойные кавычки не нужно,
-- они не влияют на подстановку переменных.
-- Семантически они обрабатываются на следующей стадии.
varSubstitutor (c : cs) = (c :) <$> varSubstitutor cs

-- | Экранирование строки.
escapeStr :: String -> String
escapeStr ""          = ""
escapeStr ('\\' : cs) = '\\' : '\\' : escapeStr cs
escapeStr ('\"' : cs) = '\\' : '\"' : escapeStr cs
escapeStr ('\'' : cs) = '\\' : '\'' : escapeStr cs
escapeStr (c : cs)    = c : escapeStr cs

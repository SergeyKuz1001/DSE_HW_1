{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

{- |
Модуль для чтения пользовательского запроса.
-}
module Phases.Reader (
    reader,
  ) where

import Data.Variable (Variable(..), variable, varPs1)
import Monads.IO (MonadIO (..), putStrLn)
import Monads.VarReader (MonadVarReader (..))

import Prelude hiding (putStr, putStrLn, getLine)

-- | Функция преобразования символа в значение соответствующей переменной.
codeToVar :: Char -> Maybe Variable
codeToVar = \case
  'u' -> e2m $ variable "USER"
  'w' -> e2m $ variable "PWD"
  'h' -> e2m $ variable "HOSTNAME"
  'd' -> e2m $ variable "DATE"
  't' -> e2m $ variable "TIME"
  _   -> fail "it isn't code of variable"
  where
    e2m = either (fail . show) return

-- | Преобразование значения переменной @PS1@.
parsePs1 :: MonadVarReader m => String -> m String
parsePs1 "" = return ""
parsePs1 "\\" = return ""
parsePs1 ('\\':'[':cs) = ("\ESC[" ++) <$> parsePs1 cs
parsePs1 ('\\':']':cs) = parsePs1 cs
parsePs1 ('\\':c:cs) = do
  let mVar = codeToVar c
  value <- maybe (return "") getVar mVar
  (value ++) <$> parsePs1 cs
parsePs1 (c:cs) = (c :) <$> parsePs1 cs

-- | Чтение пользовательского запроса. При конце потока ввода передаёт дальше
-- команду @exit@.
reader :: (MonadIO m, MonadVarReader m) => m String
reader = do
  ps1 <- getVar (Stable varPs1) >>= parsePs1
  putStr ps1
  mStr <- getLine
  maybe (putStrLn "exit" >> return "exit") return mStr

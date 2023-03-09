{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

{- |
Модуль предназначен для чтения пользовательского ввода (запроса).
-}
module Phases.StringReader (
    stringReader,
  ) where

import Data.Variable (Variable(..), variable, varPs1)
import Environment.MonadIO
import Environment.MonadVarReader

import Prelude hiding (putStr, putStrLn, getLine)

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

-- | Чтение пользовательского запроса.
stringReader :: (MonadIO m, MonadVarReader m) => m String
stringReader = do
  ps1 <- getVar (Stable varPs1) >>= parsePs1
  putStr ps1
  mStr <- getLine
  case mStr of
    Nothing -> do
      putStrLn "exit"
      return "exit"
    Just str ->
      return str

{-# LANGUAGE LambdaCase #-}

{- |
Модуль для выполнения команд.
-}
module Phases.Executor (
    executor,
  ) where

import Data.ImprovedPrimitive
import Environment.MonadExit (MonadExit (exit))
import Environment.MonadIO as EnvIO
import Environment.MonadVarPwdReader (MonadVarPwdReader (getVarPwd))
import Environment.MonadVarsReader (MonadVarsReader (getVars))

import qualified Data.ByteString.Lazy as ByteStr
import qualified Data.Text.Lazy as Txt
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Maybe (fromMaybe)

-- | Функция принимает, разбирает и исполняет распарсшенный примитив.
executor :: (MonadIO m, MonadExit m, MonadVarsReader m) => Primitive -> m ()
executor = \case
  Command typeCmd -> case typeCmd of
    Special special -> executeSpecial special
    Common common -> case common of
      Internal internal -> executeInternal internal
      External external -> executeExternal external
  EmptyCommand -> return ()

-- | Функция для исполнения специальных команд.
executeSpecial :: (MonadExit m) => Special -> m ()
executeSpecial = \case
  Exit mb -> exit (fromMaybe 0 mb)

-- | Функция для исполнения внутренних команд.
executeInternal :: (MonadIO m, MonadVarPwdReader m) => Internal -> m ()
executeInternal = \case
  Cat filePath -> do
    file <- EnvIO.readFile filePath
    EnvIO.putStrLn file
  Echo ls -> do
    EnvIO.putStrLn $ drop 1 $ concatMap (' ' :) ls
  Wc filePath -> do
    file <- EnvIO.readFile filePath
    EnvIO.putStrLn $ show (length $ lines file) ++ ' ' : show (length $ words file) ++ ' ' : show (ByteStr.length . encodeUtf8 $ Txt.pack file)
  Pwd -> do
    pwd <- getVarPwd
    EnvIO.print pwd

-- | Функция для исполнения внешних команд.
executeExternal :: (MonadIO m, MonadVarsReader m) => External -> m ()
executeExternal = \case
  Arguments pathToCmd args -> do
    vars <- getVars
    _ <- createProcess pathToCmd args vars
    return ()

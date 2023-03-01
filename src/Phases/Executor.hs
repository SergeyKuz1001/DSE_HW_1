{-# LANGUAGE LambdaCase #-}
module Phases.Executor (
    executor,
  ) where

import Data.ImprovedPrimitive
import Environment.MonadIO as EnvIO
import qualified Data.ByteString.Char8 as ByteStr
import Environment.MonadExit (ExitCode)
import Data.Char (isSpace)
import Data.Bool (bool)
import Environment.MonadFS.Internal ( AbsFilePath (AbsFilePath) )
import Environment (MonadVarPwdReader (getVarPwd))
import Control.Applicative ((<|>))
import Environment.MonadVarsReader (MonadVarsReader (getVars))

type WcOutputArguments = (Int, Int, Int, Bool)

executor :: (MonadIO m, MonadVarsReader m) => Primitive -> m (Maybe ExitCode)
executor = \case
  Command typeCmd -> case typeCmd of
    Special special -> executeSpecial special
    Common common -> case common of
      Internal internal -> executeInternal internal
      External external -> executeExternal external
  EmptyCommand -> return Nothing

executeSpecial :: (MonadIO m) => Special -> m (Maybe ExitCode)
executeSpecial = \case
  Exit mb -> return (mb <|> Just 0)

executeInternal :: (MonadIO m, MonadVarPwdReader m) => Internal -> m (Maybe ExitCode)
executeInternal = \case
  Cat filePath -> do
    file <- EnvIO.readFile filePath
    EnvIO.putStrLn file
    return Nothing
  Echo ls -> do
    EnvIO.putStrLn $ drop 1 $ concatMap (' ' :) ls
    return Nothing
  Wc filePath -> do
    file <- EnvIO.readFileFromBytes filePath
    let (countLines, countWords, bytes, isPrevSpace) = ByteStr.foldl' wcgo (1, 0, 0, False) file
    let newCountWords = countWords + bool 1 0 isPrevSpace
    EnvIO.putStrLn $ show countLines ++ ' ' : show newCountWords ++ ' ' : show bytes
    return Nothing
  Pwd -> do
    (AbsFilePath path) <- getVarPwd
    EnvIO.putStrLn path
    return Nothing

  where
    wcgo :: WcOutputArguments -> Char -> WcOutputArguments
    wcgo (countLines, countWords, bytes, isPrevSpace) c =
      (countLines + checkOnLine, countWords + checkOnWord, bytes + 1, isSpace c)
      where
        checkOnLine = bool 0 1 $ c == '\n'
        checkOnWord = bool 0 1 $ not isPrevSpace && isSpace c

executeExternal :: (MonadIO m, MonadVarsReader m) => External -> m (Maybe ExitCode)
executeExternal = \case
  Arguments pathToCmd args -> do
    vars <- getVars
    _ <- createProcess pathToCmd args vars
    return Nothing

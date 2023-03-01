{-# LANGUAGE NoImplicitPrelude #-}

module Environment.MonadIO (
    MonadIO,
    putStr,
    getLine,
    readFile,
    readFileFromBytes,
    createProcess,
    putStrLn,
    print,
  ) where

import Prelude hiding (putStr, getLine, readFile, putStrLn, print)
import Environment.MonadFS.Internal ( AbsFilePath )
import qualified Data.ByteString.Char8 as ByteStr
import Environment.MonadExit (ExitCode)

class Monad m => MonadIO m where
  putStr :: String -> m ()
  getLine :: m String
  readFile :: AbsFilePath -> m String
  readFileFromBytes :: AbsFilePath -> m ByteStr.ByteString
  createProcess :: AbsFilePath -> [String] -> [(String, String)] -> m ExitCode

putStrLn :: MonadIO m => String -> m ()
putStrLn = putStr . (++ "\n")

print :: (MonadIO m, Show a) => a -> m ()
print = putStrLn . show

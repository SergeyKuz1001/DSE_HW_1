module Environment.MonadPM (
    MonadPM (..),
    putStrFromHandle
  ) where

import Data.Variable (Stable)
import Environment.FSPrimitive (AbsFilePath)
import Environment.HandlePrimitive
import Environment.MonadExit (ExitCode)
import Environment.MonadIO (MonadIO, putStr)
import Prelude hiding (putStr)

class Monad m => MonadPM m where
  createProcess ::
    AbsFilePath ->
    [String] ->
    [(Stable, String)] ->
    (Handle, Handle, Handle) ->
    m ProcessHandle
  waitForProcess :: ProcessHandle -> m ExitCode
  terminateProcess :: ProcessHandle -> m ()
  hPutStr :: Handle -> String -> m ()
  hGetLine :: Handle -> m String
  createPipe :: m (Handle, Handle)
  hGetContents :: Handle -> m String

putStrFromHandle :: (MonadIO m, MonadPM m) => Handle -> m ()
putStrFromHandle hIn = hGetContents hIn >>= (\content -> putStr $
  if null content || last content /= '\n' 
  then content ++ "\n" 
  else content)

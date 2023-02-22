{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Environment (
    module Environment.MonadError,
    module Environment.MonadIO,
    Environment,
    runEnvironment,
  ) where

import Environment.MonadError
import Environment.MonadIO

import qualified Control.Monad.Except as ME
import qualified Control.Monad.IO.Class as MIO
import Prelude hiding (putStr, putStrLn, getLine)
import qualified Prelude as P
import qualified System.Directory as D
import System.Exit (exitWith, ExitCode(..))

newtype Environment a = Environment (ExceptT Error IO a)
  deriving (Functor, Applicative, Monad, ME.MonadError Error, MonadError, MIO.MonadIO)

instance MonadIO Environment where
  putStr         = Environment . liftIO . P.putStr
  putStrLn       = Environment . liftIO . P.putStrLn
  getLine        = Environment . liftIO $ P.getLine
  doesFileExist  = Environment . liftIO . D.doesFileExist
  findExecutable = Environment . liftIO . D.findExecutable
  exit code      = Environment . liftIO . exitWith $
    if code == 0 then ExitSuccess else ExitFailure code

runEnvironment :: Environment a -> IO a
runEnvironment (Environment m) = do
  eRes <- runExceptT m
  case eRes of
    Right res -> return res
    Left err  -> fail $ "UnexpectedError: " ++ show err

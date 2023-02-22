{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Enviroment (
    module Enviroment.MonadError,
    module Enviroment.MonadIO,
    Enviroment,
    runEnviroment,
  ) where

import Enviroment.MonadError
import Enviroment.MonadIO

import qualified Control.Monad.Except as ME
import qualified Control.Monad.IO.Class as MIO
import Prelude hiding (putStr, putStrLn, getLine)
import qualified Prelude as P
import qualified System.Directory as D
import System.Exit (exitWith, ExitCode(..))

newtype Enviroment a = Enviroment (ExceptT Error IO a)
  deriving (Functor, Applicative, Monad, ME.MonadError Error, MonadError, MIO.MonadIO)

instance MonadIO Enviroment where
  putStr         = Enviroment . liftIO . P.putStr
  putStrLn       = Enviroment . liftIO . P.putStrLn
  getLine        = Enviroment . liftIO $ P.getLine
  doesFileExist  = Enviroment . liftIO . D.doesFileExist
  findExecutable = Enviroment . liftIO . D.findExecutable
  exit code      = Enviroment . liftIO . exitWith $
    if code == 0 then ExitSuccess else ExitFailure code

runEnviroment :: Enviroment a -> IO a
runEnviroment (Enviroment m) = do
  eRes <- runExceptT m
  case eRes of
    Right res -> return res
    Left err  -> fail $ "UnexpectedError: " ++ show err

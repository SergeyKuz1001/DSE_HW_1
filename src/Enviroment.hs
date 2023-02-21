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

newtype Enviroment a = Enviroment (ExceptT Error IO a)
  deriving (Functor, Applicative, Monad, ME.MonadError Error, MonadError, MIO.MonadIO)

instance MonadIO Enviroment where
  putStr         = Enviroment . liftIO . P.putStr
  putStrLn       = Enviroment . liftIO . P.putStrLn
  getLine        = Enviroment . liftIO $ P.getLine
  doesFileExist  = Enviroment . liftIO . D.doesFileExist
  findExecutable = Enviroment . liftIO . D.findExecutable

runEnviroment :: a -> Enviroment a -> IO a
runEnviroment dv (Enviroment m) = runExceptT m >>= either (\e -> print e >> return dv) return

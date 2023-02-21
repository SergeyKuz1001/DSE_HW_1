module Enviroment (
    module Enviroment.Error,
    Enviroment,
    runEnviroment,
  ) where

import Enviroment.Error (Error)

import Control.Monad.Except (MonadError, ExceptT(..), runExceptT)
import Control.Monad.IO.Class (MonadIO)

newtype Enviroment a = Env (ExceptT Error IO a)
  deriving (Functor, Applicative, Monad, MonadError Error, MonadIO)

runEnviroment :: a -> Enviroment a -> IO a
runEnviroment dv (Env m) = runExceptT m >>= either (\e -> print e >> return dv) return

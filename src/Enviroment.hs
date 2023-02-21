module Enviroment (
    module Enviroment.Error,
    module Enviroment.Monads,
    Enviroment,
    runEnviroment,
  ) where

import Enviroment.Error (Error)
import Enviroment.Monads

newtype Enviroment a = Env (ExceptT Error IO a)
  deriving (Functor, Applicative, Monad, MonadError Error, MonadIO)

runEnviroment :: a -> Enviroment a -> IO a
runEnviroment dv (Env m) = runExceptT m >>= either (\e -> print e >> return dv) return

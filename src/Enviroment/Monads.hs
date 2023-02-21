module Enviroment.Monads (
    module Control.Monad.Except,
    module Control.Monad.IO.Class,
    (?:),
    (@:),
  ) where

import Control.Monad.Except
import Control.Monad.IO.Class

infix 0 ?:, @:

(?:) :: MonadError e m => Bool -> e -> m ()
False ?: err = throwError err
True  ?: _   = return ()

(@:) :: MonadError e m => Maybe a -> e -> m a
Nothing    @: err = throwError err
(Just res) @: _   = return res

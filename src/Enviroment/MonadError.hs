{-# LANGUAGE FlexibleContexts #-}

module Enviroment.MonadError (
    module Control.Monad.Except,
    Error(..),
    MonadError,
    (?:),
    (@:),
    (?>=),
    (@>=),
  ) where

import Control.Monad.Except hiding (MonadError, MonadIO)
import qualified Control.Monad.Except as ME

data Error = Error String String

instance Show Error where
  show (Error type_ msg) = type_ ++ ": " ++ msg

class ME.MonadError Error m => MonadError m

infix 0 ?:, @:, ?>=, @>=

(?:) :: MonadError m => Bool -> Error -> m ()
False ?: err = throwError err
True  ?: _   = return ()

(@:) :: MonadError m => Maybe a -> Error -> m a
Nothing    @: err = throwError err
(Just res) @: _   = return res

(?>=) :: MonadError m => m Bool -> Error -> m ()
m ?>= err = m >>= (?: err)

(@>=) :: MonadError m => m (Maybe a) -> Error -> m a
m @>= err = m >>= (@: err)

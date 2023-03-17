{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
В данном модуле объявлена монада @'MonadError'@ с основными функциями для
простой работы с ней.
-}
module Monads.Error (
    MonadError,
    (?:),
    (@:),
    (?>=),
    (@>=),
    throwError,
    catchError,
  ) where

import Data.Error (Error(..))

import Control.Monad.Except hiding (MonadError, MonadIO)
import qualified Control.Monad.Except as ME

-- | Синоним @'ME.MonadError' 'Error'@.
class ME.MonadError Error m => MonadError m

instance MonadError (Either Error)

infix 0 ?:, @:, ?>=, @>=

-- | Операция для вызова исключения если написанное слева выражение ложно.
(?:) :: MonadError m => Bool -> Error -> m ()
False ?: err = throwError err
True  ?: _   = return ()

-- | Операция для вызова исключения если написанное слева выражение не
-- является значением.
(@:) :: MonadError m => Maybe a -> Error -> m a
Nothing    @: err = throwError err
(Just res) @: _   = return res

-- | Аналог операции @'(?:)'@ для выражения в монаде.
(?>=) :: MonadError m => m Bool -> Error -> m ()
m ?>= err = m >>= (?: err)

-- | Аналог операции @'(\@:)'@ для выражения в монаде.
(@>=) :: MonadError m => m (Maybe a) -> Error -> m a
m @>= err = m >>= (@: err)

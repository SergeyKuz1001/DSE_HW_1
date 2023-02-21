{-# LANGUAGE FlexibleContexts #-}

module Phases.Parser (
    parser,
  ) where

import Enviroment.Error (Error(..))
import Data.Primitive (Primitive)

import Control.Monad.Except (MonadError)

parserError :: String -> Error
parserError = Error "ParsingError"

parser :: MonadError Error m => String -> m Primitive
parser = undefined

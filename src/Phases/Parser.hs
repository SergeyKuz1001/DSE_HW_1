{-# LANGUAGE NoImplicitPrelude #-}

module Phases.Parser (
    parser,
  ) where

import Data.Primitive (Primitive)
import Enviroment.MonadError

import Prelude hiding (error)

error :: String -> Error
error = Error "ParsingError"

parser :: MonadError m => String -> m Primitive
parser = undefined

{-# LANGUAGE NoImplicitPrelude #-}

module Phases.VarSubstitutor (
    varSubstitutor,
  ) where

import Environment.MonadError (Error(..), MonadError)

import Prelude hiding (error)

error :: String -> Error
error = Error "VarSubstitutorError"

varSubstitutor :: MonadError m => String -> m String
varSubstitutor = undefined

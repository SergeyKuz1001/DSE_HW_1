{-# LANGUAGE NoImplicitPrelude #-}

module Phase.Analyzer (
    analyzer,
  ) where

import qualified Data.Primitive as P
import Data.ImprovedPrimitive hiding (Primitive(..))
import qualified Data.ImprovedPrimitive as IP
import Enviroment.Error
import Enviroment.Monads

import Control.Monad.Except (MonadError)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (listToMaybe)
import Prelude hiding (error)
import Text.Read (readMaybe)

error :: String -> Error
error = Error "AnalizingError"

analyzer :: MonadError Error m => P.Primitive -> m IP.Primitive
analyzer (P.Command (command :| args)) =
  case command of
    "exit" -> do
      length args > 1 ?: error "too much arguments of `exit` command"
      let mArg = listToMaybe args
      mInt <- mapM (\arg ->
        readMaybe arg @: error "argument of `exit` command must be integer")
        mArg
      return . IP.Command . Special $ Exit mInt
    "echo" -> do
      return . IP.Command . Common . Internal $ Echo args
    "cat" -> do
      length args == 1 ?: error "`cat` command must have only one argument"
      let filePath = head args
      return . IP.Command . Common . Internal $ Cat filePath
    _ | '/' `elem` command -> do
      return . IP.Command . Common . External . Local $ command :| args
    _ -> do
      return . IP.Command . Common . External . Global $ command :| args

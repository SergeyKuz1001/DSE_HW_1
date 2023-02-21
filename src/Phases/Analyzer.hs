{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module Phases.Analyzer (
    analyzer,
  ) where

import qualified Data.Primitive as P
import Data.ImprovedPrimitive hiding (Primitive(..))
import qualified Data.ImprovedPrimitive as IP
import Enviroment.Error
import Enviroment.Monads

import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (listToMaybe)
import Prelude hiding (error)
import System.Directory (doesFileExist, findExecutable)
import Text.Read (readMaybe)

error :: String -> Error
error = Error "AnalizingError"

analyzer :: (MonadError Error m, MonadIO m) => P.Primitive -> m IP.Primitive
analyzer (P.Command (command :| args)) =
  case command of
    "cat" -> do
      length args == 1 ?: error "`cat` command must have only one argument"
      let filePath = head args
      return . IP.Command . Common . Internal $ Cat filePath
    "echo" -> do
      return . IP.Command . Common . Internal $ Echo args
    "wc" -> do
      length args == 1 ?: error "`wc` command must have only one argument"
      let filePath = head args
      return . IP.Command . Common . Internal $ Wc filePath
    "pwd" -> do
      null args ?: error "`pwd` command hasn't arguments"
      return . IP.Command . Common . Internal $ Pwd
    "exit" -> do
      length args > 1 ?: error "too many arguments of `exit` command"
      let mArg = listToMaybe args
      mInt <- mapM (\arg ->
        readMaybe arg @: error "argument of `exit` command must be integer")
        mArg
      return . IP.Command . Special $ Exit mInt
    _ -> do
      liftIO (doesFileExist command) ?>= error ("can't find file by path " ++ command)
      filePath <- liftIO (findExecutable command) @>= error ("can't find executable file by path " ++ command)
      return . IP.Command . Common . External $ Arguments filePath args

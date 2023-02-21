{-# LANGUAGE NoImplicitPrelude #-}

module Phases.Analyzer (
    analyzer,
  ) where

import qualified Data.Primitive as P
import Data.ImprovedPrimitive hiding (Primitive(..))
import qualified Data.ImprovedPrimitive as IP
import Enviroment.MonadError
import Enviroment.MonadIO

import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (listToMaybe)
import Prelude hiding (error)
import Text.Read (readMaybe)

error :: String -> Error
error = Error "AnalyzingError"

analyzer :: (MonadError m, MonadIO m) => P.Primitive -> m IP.Primitive
analyzer (P.Command (command :| args)) =
  case command of
    "cat" -> do
      length args == 1 ?: error "`cat` command must have only one argument"
      let filePath = head args
      doesFileExist filePath ?>= error ("can't find file by path " ++ filePath)
      return . IP.Command . Common . Internal $ Cat filePath
    "echo" -> do
      return . IP.Command . Common . Internal $ Echo args
    "wc" -> do
      length args == 1 ?: error "`wc` command must have only one argument"
      let filePath = head args
      doesFileExist filePath ?>= error ("can't find file by path " ++ filePath)
      return . IP.Command . Common . Internal $ Wc filePath
    "pwd" -> do
      null args ?: error "`pwd` command hasn't arguments"
      return . IP.Command . Common . Internal $ Pwd
    "exit" -> do
      length args <= 1 ?: error "too many arguments of `exit` command"
      let mArg = listToMaybe args
      mInt <- mapM (\arg ->
        readMaybe arg @: error "argument of `exit` command must be integer")
        mArg
      return . IP.Command . Special $ Exit mInt
    _ -> do
      doesFileExist command ?>= error ("can't find file by path " ++ command)
      filePath <- findExecutable command @>= error ("can't find executable file by path " ++ command)
      return . IP.Command . Common . External $ Arguments filePath args

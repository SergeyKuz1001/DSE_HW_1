{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Environment (
    module Environment.MonadError,
    module Environment.MonadExit,
    module Environment.MonadFS,
    module Environment.MonadIO,
    module Environment.MonadVarPathReader,
    module Environment.MonadVarPwdReader,
    Environment,
    runEnvironment,
  ) where

import Environment.MonadError
import Environment.MonadExit
import Environment.MonadFS
import Environment.MonadFS.Internal
import Environment.MonadIO
import Environment.MonadVarPathReader
import Environment.MonadVarPwdReader

import qualified Control.Monad.Except as ME
import qualified Control.Monad.IO.Class as MIO
import Prelude hiding (putStr, putStrLn, getLine)
import qualified Prelude as P
import qualified System.Directory as D
import System.Exit (exitWith, ExitCode(..))
import qualified System.FilePath as FP

newtype Environment a = Environment (ME.ExceptT Error IO a)
  deriving (Functor, Applicative, Monad, ME.MonadError Error, MonadError, MIO.MonadIO)

toEnv :: IO a -> Environment a
toEnv = Environment . MIO.liftIO

instance MonadIO Environment where
  putStr  = toEnv . P.putStr
  getLine = toEnv $ P.getLine

instance MonadVarPathReader Environment where
  getVarPath = undefined -- TODO

instance MonadVarPwdReader Environment where
  getVarPwd = undefined --TODO

instance MonadFS Environment where
  findFile path
    | FP.isAbsolute path = toEnv $ do
        exists <- D.doesFileExist path
        if not exists
          then return Nothing
          else do
            perms <- D.getPermissions path
            let perms' = (Permissions <$> D.readable <*> D.writable <*> D.executable) perms
            return . Just $ File (AbsFilePath path) perms'
    | otherwise = do
        absPath <- toEnv $ D.makeAbsolute path
        findFile absPath

instance MonadExit Environment where
  exit code = toEnv . exitWith $
    if code == 0 then ExitSuccess else ExitFailure code

runEnvironment :: Environment a -> IO a
runEnvironment (Environment m) = do
  eRes <- ME.runExceptT m
  case eRes of
    Right res -> return res
    Left err  -> fail $ "UnexpectedError: " ++ show err

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
В данном модуле объявлен тип @'Environment'@ как главный контекст, в котором
будут происходить все основные действия данной программы.
-}
module Environment (
    module Environment.MonadError,
    module Environment.MonadExit,
    module Environment.MonadFS,
    module Environment.MonadIO,
    module Environment.MonadVarPathReader,
    module Environment.MonadVarPwdReader,
    module Environment.MonadVarsReader,
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
import Environment.MonadVarsReader

import qualified Control.Monad.Except as ME
import qualified Control.Monad.IO.Class as MIO
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Prelude hiding (putStr, putStrLn, getLine, readFile)
import qualified Prelude as P
import qualified System.Directory as D
import System.Environment (lookupEnv, getEnvironment)
import System.Exit (exitWith, ExitCode(..))
import qualified System.FilePath as FP
import System.IO (isEOF)
import qualified System.Process as PRC

-- | Главный контекст для вычислений в программе.
newtype Environment a = Environment (ME.ExceptT Error IO a)
  deriving (Functor, Applicative, Monad, ME.MonadError Error, MonadError, MIO.MonadIO)

-- | Функция-обёртка действий из монады @'IO'@ в монаду @'Environment'@.
toEnv :: IO a -> Environment a
toEnv = Environment . MIO.liftIO

instance MonadIO Environment where
  putStr  = toEnv . P.putStr
  getLine = toEnv $ do
    eof <- isEOF
    if eof
      then return Nothing
      else Just <$> P.getLine
  readFile (AbsFilePath filePath) = toEnv $ P.readFile filePath
  readFileFromBytes (AbsFilePath filePath) = toEnv $ BS.readFile filePath
  createProcess (AbsFilePath filePath) args vars = toEnv $ do
    procHndl <- PRC.runProcess filePath args Nothing (Just vars) Nothing Nothing Nothing
    exitCode <- PRC.waitForProcess procHndl
    return $ case exitCode of
      ExitSuccess   -> 0
      ExitFailure x -> x

instance MonadVarPathReader Environment where
  getVarPath = getVarPathDefault

instance MonadVarPwdReader Environment where
  getVarPwd = getVarPwdDefault

instance MonadVarsReader Environment where
  getVar varName = toEnv $ fromMaybe "" <$> lookupEnv varName
  getVars = toEnv getEnvironment

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

-- | Функция для запуска вычислений.
runEnvironment :: Environment a -> IO a
runEnvironment (Environment m) = do
  eRes <- ME.runExceptT m
  case eRes of
    Right res -> return res
    Left err  -> fail $ "UnexpectedError: " ++ show err

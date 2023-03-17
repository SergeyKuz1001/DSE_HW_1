{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

{- |
В данном модуле объявлен тип @'Environment'@ как главный контекст, в котором
будут происходить все основные действия данной программы.
-}
module Environment (
    Environment,
    runEnvironment,
  ) where

import Data.Error
import Data.ExitCode
import Data.Handles
import Data.FSObjects
import Data.Variable
import Monads.Error
import Monads.Exit
import Monads.FS
import Monads.IO
import Monads.PM
import Monads.PathReader
import Monads.PathWriter
import Monads.PwdReader
import Monads.PwdWriter
import Monads.SelfReferenced
import Monads.VarReader
import Monads.VarWriter

import qualified Control.Monad.Except as ME
import qualified Control.Monad.IO.Class as MIO
import qualified Control.Monad.State as ST
import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe, catMaybes)
import qualified Data.Text.Lazy.IO as TIO
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (LocalTime, getCurrentTimeZone, utcToLocalTime)
import Prelude hiding (putStr, putStrLn, print, readFile, getLine)
import qualified Prelude as P
import qualified System.Directory as D
import System.Environment (getExecutablePath, getEnvironment)
import System.Exit (exitWith)
import qualified System.Process as PRC
import System.IO

-- | Главный контекст для вычислений в программе.
newtype Environment a = Environment (ST.StateT (Map Stable String) (ME.ExceptT Error IO) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , ME.MonadError Error
    , MonadError
    , ST.MonadState (Map Stable String)
    , MIO.MonadIO
    )

-- | Функция-обёртка действий из монады @'IO'@ в монаду @'Environment'@.
toEnv :: IO a -> Environment a
toEnv = Environment . MIO.liftIO

instance MonadFail Environment where
  fail msg = throwError $ Error "CrucialError" msg

instance MonadIO Environment where
  putStr  = toEnv . P.putStr
  getLine = toEnv $ do
    eof <- isEOF
    if eof
      then return Nothing
      else Just <$> P.getLine

instance MonadPM Environment where
  type Stream  Environment = Maybe Handle
  type Process Environment = (PRC.ProcessHandle, [Handle])
  getDefaultStream = return $ Just stdin
  applyFuncToStream func hIn hOut stream = do
    strIn <- case hIn of
      FromParentHandle -> do
        Just hndl <- return stream
        toEnv $ TIO.hGetContents hndl
      FromFile path -> do
        toEnv . TIO.readFile $ asFilePath path
      FromString str ->
        return str
    let strOut = func strIn
    case hOut of
      ToStdout -> do
        toEnv $ TIO.putStr strOut
        return Nothing
      ToNewPipe -> do
        (hndlIn, hndlOut) <- toEnv PRC.createPipe
        toEnv $ TIO.hPutStr hndlIn strOut
        return $ Just hndlOut
      ToNowhere ->
        return Nothing
  createProcess path args hIn hOut vars stream = do
    let vars' = map (\(var, value) -> (getVarName var, value)) vars
    let cmd   = asFilePath path
    (std_in, mHndl_in) <- case hIn of
      FromParentHandle -> do
        Just hndl <- return stream
        return (PRC.UseHandle hndl, Nothing)
      FromFile path' -> do
        hndl <- toEnv $ openFile (asFilePath path') ReadMode
        return (PRC.UseHandle hndl, Just hndl)
      FromString _ ->
        return (PRC.CreatePipe, Nothing)
    (std_out, mHndl_out) <- case hOut of
      ToStdout ->
        return (PRC.UseHandle stdout, Nothing)
      ToNewPipe ->
        return (PRC.CreatePipe, Nothing)
      ToNowhere ->
        if pathSeparator == '/'
          then do
            hndl <- toEnv $ openFile "/dev/null" WriteMode
            return (PRC.UseHandle hndl, Just hndl)
          else
            return (PRC.NoStream, Nothing)
    let proc = (PRC.proc cmd args) {
        PRC.env = Just vars',
        PRC.std_in = std_in,
        PRC.std_out = std_out
      }
    (mHndlIn, mHndlOut, _, procHndl) <- toEnv $ PRC.createProcess proc
    case hIn of
      FromString str -> do
        Just hndlIn <- return mHndlIn
        toEnv $ TIO.hPutStr hndlIn str
        toEnv $ hClose hndlIn
      _ -> return ()
    return ((procHndl, catMaybes [mHndl_in, mHndl_out]), mHndlOut)
  waitForProcess (procHndl, fileHndls) = toEnv $ do
    ec <- fromStandardEC <$> PRC.waitForProcess procHndl
    traverse_ hClose fileHndls
    return ec
  terminateProcess (procHndl, fileHndls) = toEnv $ do
    PRC.terminateProcess procHndl
    traverse_ hClose fileHndls

instance MonadPathReader Environment where
  getVarPath = getVarPathDefault

instance MonadPwdReader Environment where
  getVarPwd = getVarPwdDefault

-- | Функция для получения локального времени.
getLocalTime :: IO LocalTime
getLocalTime = do
  timeZone <- getCurrentTimeZone
  utcToLocalTime timeZone <$> getCurrentTime

instance MonadVarReader Environment where
  getVar (Volatile Date) = formatTime defaultTimeLocale "%e %B %Y" <$> toEnv getLocalTime
  getVar (Volatile Time) = formatTime defaultTimeLocale "%H:%M:%S" <$> toEnv getLocalTime
  getVar (Stable var) = do
      vars <- ST.get
      return . fromMaybe "" $ vars M.!? var
  getVars = ST.gets M.toList

instance MonadPathWriter Environment where
  setVarPath = setVarPathDefault

instance MonadPwdWriter Environment where
  setVarPwd = setVarPwdDefault

instance MonadVarWriter Environment where
  setVar var value = ST.modify $ M.insert var value

instance MonadFS Environment where
  findFileByAbsPath absPath = toEnv $ do
    let path = asFilePath absPath
    exists <- D.doesFileExist path
    if not exists
      then return Nothing
      else do
        perms <- D.getPermissions path
        let perms' = (Permissions <$> D.readable <*> D.writable <*> D.executable) perms
        return . Just $ File absPath perms'

instance MonadSelfReferenced Environment where
  getSelfPath = do
    path <- toEnv getExecutablePath
    Just absPath <- doesFileExist path
    return absPath

instance MonadExit Environment where
  exit = toEnv . exitWith . toStandardEC

-- | Функция для запуска вычислений.
runEnvironment :: Environment a -> IO a
runEnvironment (Environment m) = do
  varsFromIO <- getEnvironment
  let vars = M.fromList . mapMaybe (\(n, v) -> toMaybe (variable n) >>= asStable >>= return . (,v)) $ varsFromIO
  curDir <- D.getCurrentDirectory
  eRes <- ME.runExceptT $ ST.evalStateT m (
    M.insert varPs1 "\\[01;31m\\]\\t\\[00m\\]|\\[01;32m\\]\\u\\[00m\\]:\\[01;34m\\]\\w\\[00m\\]$ " $
    M.insert varPwd curDir vars)
  case eRes of
    Right res -> return res
    Left _    -> undefined -- считаем, что все действия в монаде были безошибочны
    where
      toMaybe :: Either e a -> Maybe a
      toMaybe = either (const Nothing) Just

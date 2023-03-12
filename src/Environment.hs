{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

{- |
В данном модуле объявлен тип @'Environment'@ как главный контекст, в котором
будут происходить все основные действия данной программы.
-}
module Environment (
    module Environment.FSPrimitive,
    module Environment.MonadError,
    module Environment.MonadExit,
    module Environment.MonadFS,
    module Environment.MonadIO,
    module Environment.MonadPM,
    module Environment.MonadPathReader,
    module Environment.MonadPwdReader,
    module Environment.MonadVarReader,
    module Environment.MonadPathWriter,
    module Environment.MonadPwdWriter,
    module Environment.MonadVarWriter,
    Environment,
    runEnvironment,
  ) where

import Data.Variable
import Environment.FSPrimitive
import Environment.HandlePrimitive
import Environment.MonadError
import Environment.MonadExit
import Environment.MonadFS
import Environment.MonadIO
import Environment.MonadPM
import Environment.MonadPathReader
import Environment.MonadPwdReader
import Environment.MonadVarReader
import Environment.MonadPathWriter
import Environment.MonadPwdWriter
import Environment.MonadVarWriter

import qualified Control.Monad.Except as ME
import qualified Control.Monad.IO.Class as MIO
import qualified Control.Monad.State as ST
import qualified Data.ByteString as BS
import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe, catMaybes)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (LocalTime, getCurrentTimeZone, utcToLocalTime)
import Prelude hiding (putStr, putStrLn, print, readFile, getLine)
import qualified Prelude as P
import qualified System.Directory as D
import System.Environment (getEnvironment)
import System.Exit (exitWith)
import System.IO (isEOF, IOMode(..), hClose)
import System.IO as SIO
import qualified System.Process as PRC
import System.Process (StdStream(UseHandle))

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

instance MonadIO Environment where
  putStr  = toEnv . P.putStr
  getLine = toEnv $ do
    eof <- isEOF
    if eof
      then return Nothing
      else Just <$> P.getLine
  readFile absPath = toEnv $ P.readFile $ asFilePath absPath
  readFileFromBytes absPath = toEnv $ BS.readFile $ asFilePath absPath

instance MonadPM Environment where
  createProcess absPath args vars (stdinA, stdoutA, stderrA) = toEnv $ do
    let vars' = map (\(var, value) -> (getVarName var, value)) vars
    let cmd   = asFilePath absPath
    -- (strIn,  mFileHndlIn)  <- handleActionToStdStream stdinA ReadMode
    -- (strOut, mFileHndlOut) <- handleActionToStdStream stdoutA WriteMode
    -- (strErr, mFileHndlErr) <- handleActionToStdStream stderrA WriteMode
    let proc = (PRC.proc cmd args) {
        PRC.env = Just vars',
        PRC.std_in = UseHandle stdinA,
        PRC.std_out = UseHandle stdoutA,
        PRC.std_err = UseHandle stderrA
      }
    (_, _, _, procHndl) <- PRC.createProcess proc
    return . ProcessHandle procHndl $ catMaybes []
  waitForProcess (ProcessHandle procHndl fileHndls) = toEnv $ do
    ec <- fromStandardEC <$> PRC.waitForProcess procHndl
    traverse_ hClose fileHndls
    return ec
  terminateProcess (ProcessHandle procHndl fileHndls) = toEnv $ do
    PRC.terminateProcess procHndl
    traverse_ hClose fileHndls
  hPutStr = (toEnv .) . SIO.hPutStr
  hGetLine = toEnv . SIO.hGetLine
  createPipe = toEnv PRC.createPipe
  hGetContents = toEnv . SIO.hGetContents

instance MonadPathReader Environment where
  getVarPath = getVarPathDefault

instance MonadPwdReader Environment where
  getVarPwd = getVarPwdDefault

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

throwAssignmentError :: MonadError m => String -> m ()
throwAssignmentError = throwError . Error "AssignmentError"

instance MonadVarWriter Environment where
  setVar (Specific LastExitCode) _ =
    throwAssignmentError "can't set value to special variable \"?\""
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

instance MonadExit Environment where
  exit = toEnv . exitWith . toStandardEC

-- | Функция для запуска вычислений.
runEnvironment :: Environment a -> IO a
runEnvironment (Environment m) = do
  varsFromIO <- getEnvironment
  let vars = M.fromList . mapMaybe (\(n, v) -> toMaybe (variable n) >>= asStable >>= return . (,v)) $ varsFromIO
  curDir <- D.getCurrentDirectory
  eRes <- ME.runExceptT $ ST.evalStateT m (
    M.insert varPs1 "\ESC[01;31m\\t\ESC[00m|\ESC[01;32m\\u\ESC[00m:\ESC[01;34m\\w\ESC[00m$ " $
    M.insert varPwd curDir vars)
  case eRes of
    Right res -> return res
    Left err  -> fail $ "UnexpectedError: " ++ show err
    where
      toMaybe :: Either e a -> Maybe a
      toMaybe = either (const Nothing) Just

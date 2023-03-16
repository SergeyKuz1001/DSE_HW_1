{-# LANGUAGE LambdaCase #-}

{- |
Модуль для выполнения команд.
-}
module Phases.Executor (
    executor,
  ) where

import Data.LinkedPrimitive
import Environment.MonadExit (MonadExit (exit), ExitCode (ExitCode))
import Environment.MonadIO as EnvIO
import Environment.MonadPM as PM
import Environment.MonadVarReader (MonadVarReader (..))
import Environment.MonadVarWriter (MonadVarWriter (..))
import Environment.MonadPwdReader (MonadPwdReader (..))
import Environment.MonadPwdWriter (MonadPwdWriter (..))
import Data.Variable
import System.IO (Handle)
import System.IO as SIO (stderr, stdin, stdout)
import Environment.FSPrimitive (asFilePath, (</>))

import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe, listToMaybe)

type WcOutputArguments = (Int, Int, Int, Bool)

-- | Функция принимает, разбирает и исполняет распарсшенный примитив.
executor :: (MonadIO m, MonadPM m, MonadExit m, MonadVarWriter m, MonadVarReader m) => Primitive -> m ()
executor = \case
  Special special      -> executeSpecial special
  Assignment var value -> setVar var value
  Commands commands    -> executeCommands commands

-- | Функция для исполнения специальных команд.
executeSpecial :: (MonadExit m, MonadPwdReader m, MonadVarWriter m) => Special -> m ()
executeSpecial = \case
  Cd path -> do
    pwd <- getVarPwd
    setVarPwd (pwd </> path)
  Exit mec -> exit $ fromMaybe (ExitCode 0) mec

executeCommands :: (MonadPM m, MonadVarReader m, MonadVarWriter m) => [CommandWithHandles] -> m ()
executeCommands cmds = do
  stream <- defaultStream
  ExitCode ec <- exec stream [] cmds
  setVar (Specific LastExitCode) (show ec)
    where
      exec stream runned [] =
        wait $ reverse runned
      exec stream runned ((External (Arguments path args), hIn, hOut) : commands) = do
        vars <- getVars
        (proc, stream') <- createProcess path args hIn hOut vars stream
        exec stream' (proc : runned) commands
      exec stream runned ((Internal (Func _ func), hIn, hOut) : commands) = do
        ec <- wait $ reverse runned
        if ec /= ExitCode 0
          then return ec
          else do
            stream' <- applyFuncToStream func hIn hOut stream
            exec stream' [] commands
      wait [] =
        return $ ExitCode 0
      wait (proc : procs) = do
        ec <- waitForProcess proc
        if ec == ExitCode 0
          then
            wait procs
          else do
            term procs
            return ec
      term [] =
        return ()
      term (proc : procs) = do
        terminateProcess proc
        term procs

{-spanMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
spanMaybe = go []
  where
    go acc f [] = (acc [], [])
    go acc f (x : xs) = case f x of
      Nothing -> (acc [], x : xs)
      Just y  -> go (acc . (x :)) f xs

planForCommons :: [Common] -> [Either Blocking [(External, (InputHandle, OutputHandle, OutputHandle))]]
planForCommons commons =
  let (streamings, other) = spanMaybe isStreaming commons
  in  case spanMaybe toStreaming commons of
        ()
  where
    toStreaming (Internal (Blocking _)) = Nothing
    toStreaming cmd = Just cmd

planForStreamings :: [Common] -> [(External, (InputHandle, OutputHandle, OutputHandle))]
planForStreamings = 

executeCommons :: (MonadIO m, MonadVarReader m, MonadPM m) => InputHandle -> [Common] -> m ()
exetuteCommons hIn (External cmd1 : Internal (Blocking cmd2) : commons) = do
executeCommons hIn = \case
  x :| [] ->
    execCommon (hIn, SIO.stdout, SIO.stderr) x
  x :| (x' : xs) -> do
    (newHIn, newHOut) <- PM.createPipe
    execCommon (hIn, newHOut, SIO.stderr) x
    executeCommons newHIn (x' :| xs)
  where
    execCommon (hIn, hOut, hErr) = \case
      Internal internal -> executeInternal (hIn, hOut) internal
      External external -> executeExternal (hIn, hOut, hErr) external

executeStreamings :: (MonadIO m) => (InputHandle, OutputHandle) -> [Either External Streaming] -> m ExitCode
executeStreamings _ _ [] = return $ ExitCode 0
executeStreamings hIn hOut commands = do
  mProcHndls' <- foldl (\inp command -> do
      (mProcHndl, nextOutp) <- runStreaming inp NewPipe command
    ) (return (hIn, [])) $ init commands
  

runExternals ::
  (MonadPM m, MonadVarReader m) =>
  [(External, (InputHandle, OutputHandle, OutputHandle))] ->
  m ([ProcessHandle], Maybe InputHandle)
runExternals [] = return ([], Nothing)
runExternals [(Arguments path args, hndls)] = do
  vars <- getVars
  createProcess path args vars hndls
runExternals ((Arguments path args, hndls) : (command, (hIn', hOut', hErr')) : commands) = do
  vars <- getVars
  (procHndl, mHIn') <- createProcess path args vars hndls
  (procHndls, mHIn) <- runExternals $ (command, (mHIn' <|> Just hIn', hOut', hErr')) : commands
  return (procHndl : procHndls, mHIn)

-- | Функция для исполнения внутренних команд.
executeInternal :: (MonadIO m, MonadPwdReader m, MonadPM m) => (Handle, Handle) -> Internal -> m ()
executeInternal (hIn, hOut) = \case
  Cat maybeFilePath -> do
    file <- maybe (PM.hGetContents hIn) EnvIO.readFile maybeFilePath
    PM.hPutStr hOut file
  Echo ls -> do
    PM.hPutStr hOut $ unwords ls ++ "\n"
  Wc maybeFilePath -> do
    file <- maybe (pack <$> PM.hGetContents hIn) EnvIO.readFileFromBytes maybeFilePath
    let (countLines, countWords, bytes, isPrevSpace) = ByteStr.foldl' wcgo (0, 0, 0, False) file
    let newCountWords = countWords + bool 1 0 isPrevSpace
    PM.hPutStr hOut $ show countLines ++ ' ' : show newCountWords ++ ' ' : show bytes
  Pwd -> do
    pwd <- asFilePath <$> getVarPwd
    PM.hPutStr hOut pwd
  where
    wcgo :: WcOutputArguments -> Char -> WcOutputArguments
    wcgo (countLines, countWords, bytes, isPrevSpace) c =
      (countLines + checkOnLine, countWords + checkOnWord, bytes + 1, isSpace c)
      where
        checkOnLine = bool 0 1 $ c == '\n'
        checkOnWord = bool 0 1 $ not isPrevSpace && isSpace c

-- | Функция для исполнения внешних команд.
executeExternal :: (MonadIO m, MonadVarReader m, MonadPM m) => (Handle, Handle, Handle) ->
  External -> m () -- Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle
executeExternal handles = \case
  Arguments pathToCmd args -> do
    vars <- getVars
    process <- createProcess pathToCmd args vars handles
    (ExitCode code) <- waitForProcess process
    if code == 0
    then return ()
    else terminateProcess process-}

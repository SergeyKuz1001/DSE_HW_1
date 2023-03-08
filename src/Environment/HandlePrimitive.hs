module Environment.HandlePrimitive (
    Handle,
    HandleAction(..),
    ProcessHandle,
    StdStream,
    handleActionToStdStream,
  ) where

import Environment.FSPrimitive (AbsFilePath, asFilePath)

import System.IO (Handle, IOMode, openFile)
import System.Process (ProcessHandle, StdStream(..))

-- | Тип для указания действия с потоком ввода/вывода.
data HandleAction
  = UseExisting Handle
  | OpenFile AbsFilePath
  | CreateNewPipe

handleActionToStdStream :: HandleAction -> IOMode -> IO StdStream
handleActionToStdStream ha md = case ha of
  UseExisting hndl -> return $ UseHandle hndl
  OpenFile absPath -> UseHandle <$> openFile (asFilePath absPath) md
  CreateNewPipe    -> return CreatePipe

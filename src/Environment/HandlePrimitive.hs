module Environment.HandlePrimitive (
    Handle,
    HandleAction(..),
    ProcessHandle(..),
    StdStream,
    handleActionToStdStream,
  ) where

import Environment.FSPrimitive (AbsFilePath, asFilePath)

import System.IO (Handle, IOMode, openFile)
import System.Process (StdStream(..))
import qualified System.Process as PRC

-- | Тип для указания действия с потоком ввода/вывода.
data HandleAction
  = UseExisting Handle
  | OpenFile AbsFilePath
  | CreateNewPipe

-- | Функция преобразования @'HandleAction'@ в @'StdStream'@. Также возвращает
-- @'Handle'@ открытого файла если какой-либо файл был открыт.
handleActionToStdStream :: HandleAction -> IOMode -> IO (StdStream, Maybe Handle)
handleActionToStdStream ha md = case ha of
  UseExisting hndl -> return (UseHandle hndl, Nothing)
  OpenFile absPath -> do
    hndl <- openFile (asFilePath absPath) md
    return (UseHandle hndl, Just hndl)
  CreateNewPipe    -> return (CreatePipe, Nothing)

-- | Тип для "ссылки" на запущенный процесс. Отличается от @'PRC.ProcessHandle'@
-- тем, что дополнительно хранит @'Handle'@ы на открытые потоки, соответствующие
-- файлам, чтобы при завершении процесса их закрыть.
data ProcessHandle = ProcessHandle PRC.ProcessHandle [Handle]

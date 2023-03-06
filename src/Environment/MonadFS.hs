{- |
В данном модуле объявлена монада @'MonadFS'@ для платформонезависимой работы с
файловой системой.
-}
module Environment.MonadFS (
    -- * Типы для работы с файловой системой
    AbsFilePath (asFilePath),
    Permissions (readPerm, writePerm, execPerm),
    File (absFilePath, permissions),
    -- * Монада для работы с файловой системой
    MonadFS (..),
    doesFileExist,
    doesExecutableExist,
    isReadable,
    isWritable,
    isExecutable,
  ) where

import Environment.MonadFS.Internal

import Environment.MonadVarPathReader (MonadVarPathReader)

-- | Монада для поиска файлов в файловой системе.
class MonadVarPathReader m => MonadFS m where
  {-# MINIMAL findFile | findFileAsExecutable #-}
  -- | Поиск файла в текущей директории (если указан относительный путь) или в
  -- корневой директории (если указан абсолютный).
  findFile :: FilePath -> m (Maybe File)
  findFile = findFileAsExecutable
  -- | Поиск файла, аналогичный @'findFile'@, но с использованием переменной PATH
  -- (через монаду @'MonadVarPathReader'@) если указано только имя файла.
  findFileAsExecutable :: FilePath -> m (Maybe File)
  findFileAsExecutable = findFile

-- | Проверка на то, что файл по этому пути (абсолютному или относительному)
-- существует. Если файл существует, то возвращается абсолютный путь до него.
doesFileExist :: MonadFS m => FilePath -> m (Maybe AbsFilePath)
doesFileExist path = fmap absFilePath <$> findFile path

-- | Аналогично @'doesFileExist'@, но при этом происходит поиск с использованием
-- переменной PATH и проверка, что найденный файл может быть выполнен.
doesExecutableExist :: MonadFS m => FilePath -> m (Maybe AbsFilePath)
doesExecutableExist path = do
  mFile <- findFileAsExecutable path
  if fmap (execPerm . permissions) mFile == Just True
    then return $ absFilePath <$> mFile
    else return Nothing

-- | Проверка на то, что разрешение файла удовлетворяет предикату. Возвращает
-- @False@ если файл не существует.
hasPerm :: MonadFS m => (Permissions -> Bool) -> AbsFilePath -> m Bool
hasPerm p (AbsFilePath path) = (\mFile -> fmap (p . permissions) mFile == Just True) <$> findFile path

-- | Проверка на то, что файл может быть прочитан. Возвращает @False@ если файл
-- не существует.
isReadable :: MonadFS m => AbsFilePath -> m Bool
isReadable = hasPerm readPerm

-- | Проверка на то, что файл может быть перезаписан. Возвращает @False@ если
-- файл не существует.
isWritable :: MonadFS m => AbsFilePath -> m Bool
isWritable = hasPerm writePerm

-- | Проверка на то, что файл может быть выполнен. Возвращает @False@ если файл
-- не существует.
isExecutable :: MonadFS m => AbsFilePath -> m Bool
isExecutable = hasPerm execPerm

{- |
В данном модуле объявлена монада @'MonadPathReader'@ для получения значения
переменной PATH.
-}
module Environment.MonadPathReader (
    MonadPathReader (..),
    varPathSeparator,
    parseVarPath,
  ) where

import Environment.FSPrimitive (AbsFilePath, absFilePath)
import Environment.MonadError (MonadError)

import Data.List (unfoldr)
import System.FilePath (pathSeparator, searchPathSeparator, splitSearchPath)

-- | Монада для получения списка путей к директориям с исполняемыми файлами,
-- хранящегося в переменной PATH.
class Monad m => MonadPathReader m where
  getVarPath :: m [AbsFilePath]

-- | Разделитель путей, записанных в переменной PATH.
--
-- @
-- (on Linux) varPathSeparator == \':\'
-- (on Windows) varPathSeparator == ';'
-- @
varPathSeparator :: Char
varPathSeparator = searchPathSeparator

-- | Парсер исходного значения переменной PATH в список абсолютных путей.
parseVarPath :: MonadError m => String -> m [AbsFilePath]
parseVarPath = traverse absFilePath . splitSearchPath

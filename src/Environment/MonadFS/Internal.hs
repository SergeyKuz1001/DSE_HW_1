{- |
__Warning__: Это Internal модуль, импортировать с осторожностью. Internal модули
используются для поддержания каких-либо важных инвариантов, не выраженных в
терминах типов.

В данном модуле объявлены основные примитивы для представляния файла из файловой
системы в данной программе.
-}
module Environment.MonadFS.Internal (
    AbsFilePath(..),
    (</>),
    Permissions(..),
    File(..),
  ) where

import qualified System.FilePath as FP

infixl 6 </>

-- | Абсолютный путь к файлу. Главный инвариант этого типа — абсолютность пути,
-- однако довольно часто можно утверждать, что это корректный путь к
-- существующей папке или файлу.
newtype AbsFilePath = AbsFilePath { asFilePath :: FilePath }
  deriving Eq

instance Show AbsFilePath where
  show (AbsFilePath filePath) = filePath

-- | Операция конкатенации пути. В отличие от @'FP.</>'@ конкатенируются
-- абсолютный и относительный пути, получая в итоге абсолютный.
(</>) :: AbsFilePath -> FilePath -> AbsFilePath
(</>) (AbsFilePath filePath) = AbsFilePath . (filePath FP.</>)

-- | Разрешения файла
data Permissions = Permissions
  { readPerm  :: Bool
  , writePerm :: Bool
  , execPerm  :: Bool
  }

-- | Файл в файловой системе — это абсолютный путь до него и разрешения.
data File = File
  { absFilePath :: AbsFilePath
  , permissions :: Permissions
  }

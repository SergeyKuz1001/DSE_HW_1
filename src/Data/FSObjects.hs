{- |
В данном модуле объявлены основные примитивы для платформонезависимой работы с
файловой системой.
-}
module Data.FSObjects (
    AbsFilePath (asFilePath),
    absFilePath,
    isBaseName,
    (</>),
    Permissions (..),
    File (..),
  ) where

import Data.Error (Error(..))
import Monads.Error (MonadError, (?:))

import qualified System.FilePath as FP

infixl 6 </>

-- | Абсолютный путь к файлу. Главный инвариант этого типа — абсолютность пути,
-- однако довольно часто можно утверждать, что это корректный путь к
-- существующей папке или файлу.
newtype AbsFilePath = AbsFilePath { asFilePath :: FilePath }
  deriving (Eq, Ord)

instance Show AbsFilePath where
  show (AbsFilePath path) = show path

-- | Функция для конструирования значения типа @'AbsFilePath'@. При
-- конструировании проверяется соблюдение инварианта, и в случае его нарушения
-- вызывается ошибка.
absFilePath :: MonadError m => FilePath -> m AbsFilePath
absFilePath path = do
  FP.isAbsolute path ?: Error "ViolationOfInvariantError" (
    "\"" ++ path ++ "\" is not absolute path")
  return $ AbsFilePath path

-- | Функция проверки того, является ли путь простым именем файла или нет.
isBaseName :: FilePath -> Bool
isBaseName = notElem FP.pathSeparator

-- | Операция конкатенации пути. В отличие от @'FP.</>'@ конкатенируются
-- абсолютный и относительный пути, получая в итоге абсолютный, сокращая при
-- этом специальные директории @.@ и @..@.
(</>) :: AbsFilePath -> FilePath -> AbsFilePath
(</>) (AbsFilePath path) = AbsFilePath . joinPath . reverse . foldl addToPath [] . splitPath [""] . (path FP.</>)
  where
    addToPath [] part = [part]
    addToPath parts "." = parts
    addToPath [part] ".." = [part]
    addToPath (_:parts) ".." = parts
    addToPath parts part = part : parts
    splitPath (p:ps) [] = reverse (if p == "" then ps else reverse p : ps)
    splitPath (p:ps) (c:cs) | c == FP.pathSeparator = splitPath ("" : (if p == "" && ps /= [] then ps else reverse p : ps)) cs
    splitPath (p:ps) (c:cs) = splitPath ((c:p):ps) cs
    joinPath = (\l -> if length l == 1 then head l else init $ concat l) . map (++ [FP.pathSeparator])

-- | Разрешения файла.
data Permissions = Permissions
  { readPerm  :: Bool
  , writePerm :: Bool
  , execPerm  :: Bool
  }

-- | Файл в файловой системе — это абсолютный путь до него и разрешения.
data File = File
  { filePath    :: AbsFilePath
  , permissions :: Permissions
  }

{- |
В данном модуле объявлена монада @'MonadPathWriter'@ для записи значения
переменной PATH.
-}
module Monads.PathWriter (
    MonadPathWriter (..),
    formatVarPath,
  ) where

import Data.FSObjects (AbsFilePath (asFilePath))
import Monads.PathReader (varPathSeparator)

-- | Монада для записи списка абсолютных путей в переменную PATH.
class Monad m => MonadPathWriter m where
  setVarPath :: [AbsFilePath] -> m ()

-- | Функция преобразования списка путей в строку, которая будет являться
-- корректным значением переменной PATH.
formatVarPath :: [AbsFilePath] -> String
formatVarPath [] = ""
formatVarPath afps = foldr1 (\x y -> x ++ [varPathSeparator] ++ y) $ map asFilePath afps

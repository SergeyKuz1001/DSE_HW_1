{- |
В данном модуле объявлен тип @'Variable'@ для хранения имени переменной и
вспомогательные функции для работы с ним.
-}
module Data.Variable (
    Variable (..),
    Stable (Specific),
    Volatile (..),
    Specific (..),
    variable,
    asStable,
    getVarName,
    varPath,
    varPwd,
  ) where

import Environment.MonadError (Error(..), MonadError, (?:))

data Variable = Stable Stable | Volatile Volatile
  deriving (Eq, Ord, Show)

data Stable = Specific Specific | Usual String
  deriving (Eq, Ord, Show)

data Volatile = Date | Time
  deriving (Eq, Ord, Show)

data Specific = LastExitCode
  deriving (Eq, Ord, Show)

-- | Функция для конструирования значения типа @'Variable'@. При конструировании
-- проверяется соблюдение инварианта для обычной переменной и в случае его
-- нарушения вызывается ошибка.
variable :: MonadError m => String -> m Variable
variable "?" = return . Stable $ Specific LastExitCode
variable "DATE" = return $ Volatile Date
variable "TIME" = return $ Volatile Time
variable str = do
  all isLCOU str ?: Error "ViolationOfInvariantError" (
    "\"" ++ str ++ "\" is not valid name of usual variable")
  return . Stable $ Usual str
    where
      isLCOU c = c `elem` (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['_'])

-- | Функция преобразования произвольной переменной в стабильную. Если данная
-- переменная стабильна, она возвращается в @'Just'@, иначе возвращается
-- @'Nothing'@.
asStable :: Variable -> Maybe Stable
asStable (Stable var) = Just var
asStable (Volatile _) = Nothing

-- | Функция получения имени стабильной переменной. У обычной переменной её имя
-- хранится в самом типе, а спецефичным будут даны другие имена.
getVarName :: Stable -> String
getVarName (Usual name) = name
getVarName (Specific LastExitCode) = "LAST_EXIT_CODE"

-- | Стабильная переменная PATH
varPath :: Stable
varPath = Usual "PATH"

-- | Стабильная переменная PWD
varPwd :: Stable
varPwd = Usual "PWD"

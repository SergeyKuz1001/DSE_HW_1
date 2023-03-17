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
    readVariable,
    asStable,
    getVarName,
    varPath,
    varPwd,
    varPs1,
  ) where

import Data.Error (Error(..))
import Monads.Error (MonadError, throwError)

-- | Тип переменной. В нашей программе переменные разделяются на
--
--     * стабильные — после установки их значения оно не меняется, и
--     * нестабильные — постоянно и/или неконтролируемо изменяется.
--
-- На практике обычно применяется то их различие, что стабильные можно хранить
-- в какой-либо структуре данных, а нестабильные — нельзя.
data Variable = Stable Stable | Volatile Volatile
  deriving (Eq, Ord, Show)

-- | Стабильная переменная может быть специфичной или обычной. Различие между
-- ними заключается в том, что у обычной переменной имя имеет строго
-- определённый вид, и этот вид гарантируется отсутствием конструктора @'Usual'@
-- вне этого модуля. Множество же специфичных переменных строго ограничено.
data Stable = Specific Specific | Usual String
  deriving (Eq, Ord, Show)

-- | Нестабильная переменная — это переменная, хранящая дату или время.
data Volatile = Date | Time
  deriving (Eq, Ord, Show)

-- | Специфичная переменная — это переменная, содержащая код возврата последнего
-- пользовательского запроса.
data Specific = LastExitCode
  deriving (Eq, Ord, Show)

-- | Функция для конструирования значения типа @'Variable'@. При конструировании
-- проверяется соблюдение инварианта для обычной переменной и в случае его
-- нарушения вызывается ошибка.
variable :: MonadError m => String -> m Variable
variable str = case readVariable str of
  Just (var, "") -> return var
  _ -> throwError $ Error "ViolationOfInvariantError" (
    "\"" ++ str ++ "\" is not valid name of variable")

-- | Функция безошибочного чтения имени переменной из строки. Возвращает, помимо
-- прочитанной переменной, остаток от строки. Возвращает @'Nothing'@ в случае
-- невозможности прочтения переменной (например из-за пустой строки на входе).
readVariable :: String -> Maybe (Variable, String)
readVariable "" = fail "empty input"
readVariable ('?':cs) = return (Stable $ Specific LastExitCode, cs)
readVariable (c:cs)
  | isLC c = let (gcs, bcs) = span isLCOU cs in return (toVar $ c : gcs, bcs)
  | otherwise = fail "can't get non-empty name"
    where
      isLCOU = (`elem` (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['_']))
      isLC = (`elem` (['A'..'Z'] ++ ['a'..'z']))
      toVar "DATE" = Volatile Date
      toVar "TIME" = Volatile Time
      toVar name   = Stable $ Usual name

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

-- | Стабильная переменная PATH.
varPath :: Stable
varPath = Usual "PATH"

-- | Стабильная переменная PWD.
varPwd :: Stable
varPwd = Usual "PWD"

-- | Стабильная переменная PS1.
varPs1 :: Stable
varPs1 = Usual "PS1"

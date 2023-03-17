{- |
Модуль для вывода отладочной информации.
-}
module Data.DebugInfo (
    DebugInfo (..),
    debugIfNecessary,
  ) where

import Data.Variable (variable)
import Monads.Error (MonadError)
import Monads.IO (MonadIO)
import qualified Monads.IO as MIO
import Monads.VarReader (MonadVarReader(..))

import Control.Monad (when)
import Data.Text.Lazy (unpack)
import System.Console.ANSI (setSGRCode, SGR(..), ConsoleLayer(..), ConsoleIntensity(..), ColorIntensity(..), Color(..))
import Text.Pretty.Simple (pShow)

-- | Тип стандартного отладочного сообщения. Состоит из стадии (про что это
-- информация) и само отладочное сообщение.
data DebugInfo = DebugInfo String String
  deriving Eq

instance Show DebugInfo where
  show (DebugInfo type_ msg) =
    setSGRCode [SetColor Foreground Vivid Green, SetConsoleIntensity BoldIntensity] ++
    type_ ++
    setSGRCode [Reset] ++
    ": " ++
    msg

-- | Функция печати отладочной информации. Принимает тип (стадию) отладки, код
-- (об этом чуть ниже) и тот объект, который необходимо вывести в качестве
-- отладочной информации. Предполагается, что эта функция будет использоваться
-- внутри конвейера различных преобразований, поэтому она возвращает последний
-- элемент, упакованный в монаду.
--
-- Определение того, нужно ли выводить отладочную информацию или нет, происходит
-- через переменную @DEBUG_OPTIONS@. Если эта переменная содержит код типа
-- отладки в своём значении, то происходит печать отладочной информации.
debugIfNecessary :: (MonadVarReader m, MonadError m, MonadIO m, Show a) => String -> Char -> a -> m a
debugIfNecessary type_ typeChar obj = do
  debugVariable <- variable "DEBUG_OPTIONS"
  debugOptions  <- getVar debugVariable
  when (typeChar `elem` debugOptions) $
    MIO.putStr $ show (DebugInfo type_ $ (unpack . pShow) obj) ++ "\n"
  return obj

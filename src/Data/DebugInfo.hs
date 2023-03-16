module Data.DebugInfo (
    DebugInfo (..),
    debugIfNecessary,
  ) where

import Data.Variable (variable)
import Environment.MonadError (MonadError)
import Environment.MonadIO (MonadIO)
import qualified Environment.MonadIO as MIO
import Environment.MonadVarReader (MonadVarReader(..))

import Control.Monad (when)
import Data.Text.Lazy (unpack)
import System.Console.ANSI (setSGRCode, SGR(..), ConsoleLayer(..), ConsoleIntensity(..), ColorIntensity(..), Color(..))
import Text.Pretty.Simple (pShow)

data DebugInfo = DebugInfo String String
  deriving Eq

instance Show DebugInfo where
  show (DebugInfo type_ msg) =
    setSGRCode [SetColor Foreground Vivid Green, SetConsoleIntensity BoldIntensity] ++
    type_ ++
    setSGRCode [Reset] ++
    ": " ++
    msg

debugIfNecessary :: (MonadVarReader m, MonadError m, MonadIO m, Show a) => String -> Char -> a -> m a
debugIfNecessary type_ typeChar obj = do
  debugVariable <- variable "DEBUG_OPTIONS"
  debugOptions  <- getVar debugVariable
  when (typeChar `elem` debugOptions) $
    MIO.putStr $ show (DebugInfo type_ $ (unpack . pShow) obj) ++ "\n"
  return obj

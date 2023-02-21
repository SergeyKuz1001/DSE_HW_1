module Enviroment.Error (
    Error(..),
  ) where

data Error = Error String String

instance Show Error where
  show (Error type_ msg) = type_ ++ ": " ++ msg

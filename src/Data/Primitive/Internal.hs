module Data.Primitive.Internal (
    VarName(..),
  ) where

newtype VarName = VarName { getVarName :: String }
  deriving Eq

instance Show VarName where
  show (VarName name) = name

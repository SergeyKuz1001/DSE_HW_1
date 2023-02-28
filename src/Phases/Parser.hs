{-# LANGUAGE NoImplicitPrelude #-}

module Phases.Parser ( parser ) where

import           Data.Primitive         (Primitive (..))
import           Data.Primitive.Internal (VarName (..))
import           Environment.MonadError (Error (..), MonadError, throwError)

import           Data.Bifunctor         (first)
import           Data.List.NonEmpty     (NonEmpty (..))
import           Prelude

error' :: String -> Error
error' = Error "ParsingError"

parser :: MonadError m => String -> m Primitive
parser str =
  splitBySpaces str
    >>= \wss -> case filter (not . null) wss of
      [] -> pure EmptyCommand
      -- Пока не поддерживаем команды, содержащие = как часть пути без /
      (w : ws) -> case break (== '=') w of
        ("", _) -> throwError $ error' "Variable name required"
        (_, "") -> pure . Command $ w :| ws
        (name, '=' : value) -> case ws of
          [] ->
            if '/' `elem` name
              then pure . Command $ w :| ws
              else pure $ Assignment (VarName name) value
          _ -> throwError $ error "Command calls with variable overriding are not supported"
        (_, _) -> error "Unreachable code detected"

splitBySpaces :: MonadError m => String -> m [String]
splitBySpaces "" = pure [""]
splitBySpaces "\\" = throwError $ error' "Unexpected end of line after \\"
splitBySpaces ('\\' : '\\' : cs) = headMap ('\\' :) <$> splitBySpaces cs
splitBySpaces ('\\' : '\"' : cs) = headMap ('\"' :) <$> splitBySpaces cs
splitBySpaces ('\\' : ' ' : cs) = headMap (' ' :) <$> splitBySpaces cs
splitBySpaces ('\'' : cs) = singleQuotes cs >>= \(s, r) -> headMap (s ++) <$> splitBySpaces r
splitBySpaces ('\"' : cs) = doubleQuotes cs >>= \(s, r) -> headMap (s ++) <$> splitBySpaces r
splitBySpaces (' ' : cs) = ("" :) <$> splitBySpaces cs
splitBySpaces ('\t' : cs) = ("" :) <$> splitBySpaces cs
splitBySpaces (c : cs) = headMap (c :) <$> splitBySpaces cs

headMap :: (a -> a) -> [a] -> [a]
headMap _ []       = []
headMap f (x : xs) = f x : xs

singleQuotes :: MonadError m => String -> m (String, String)
singleQuotes "" = throwError $ error' "Unexpected end of line in single quotes"
singleQuotes ('\'' : cs) = pure ("", cs)
singleQuotes (c : cs) = first (c :) <$> singleQuotes cs

doubleQuotes :: MonadError m => String -> m (String, String)
doubleQuotes "" = throwError $ error' "Unexpected end of line in double quotes"
doubleQuotes ('\"' : cs) = pure ("", cs)
doubleQuotes ('\\' : '\\' : cs) = first ('\\' :) <$> doubleQuotes cs
doubleQuotes ('\\' : '\"' : cs) = first ('\"' :) <$> doubleQuotes cs
doubleQuotes ('\\' : 'n' : cs) = first ('\n' :) <$> doubleQuotes cs
doubleQuotes ('\\' : 't' : cs) = first ('\t' :) <$> doubleQuotes cs
doubleQuotes (c : cs) = first (c :) <$> doubleQuotes cs

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Phases.Parser ( parser, splitBySpaces ) where

import           Control.Monad.Except   (throwError)
import           Data.Bifunctor         (first)
import           Data.Primitive         (Primitive (..))
import           Environment.MonadError (Error (..), MonadError)
import           Prelude

error' :: String -> Error
error' = Error "ParsingError"

-- | Функция для разбора строки после подстановки переменных.
-- Выделяет либо команду с аргументами,
-- либо пустую команду (пустая строка или строка только из пробелов),
-- либо присваивание переменной.
-- Кидает исключение, если строка имеет некорректный синтаксис.
parser :: MonadError m => String -> m Primitive
parser s = skipSpaces s >>= firstWord

skipSpaces :: Applicative f => String -> f String
skipSpaces (' '  : cs) = skipSpaces cs
skipSpaces ('\t' : cs) = skipSpaces cs
skipSpaces s           = pure s

firstWord :: MonadError m => String -> m Primitive
firstWord     ""                 = pure $ Command []
firstWord   ( '\\' : '\\' : cs ) = prependChar '\\' <$> firstWord cs
firstWord   ( '\\' : '\"' : cs ) = prependChar '\"' <$> firstWord cs
firstWord   ( '\\' : '\'' : cs ) = prependChar '\'' <$> firstWord cs
firstWord   ( '\\' : ' '  : cs ) = Command . filter (not . null) . headMap (' ' :) <$> splitBySpaces cs
firstWord   ( '\\' : '='  : cs ) = Command . filter (not . null) . headMap ('=' :) <$> splitBySpaces cs
firstWord   (        ' '  : cs ) = Command . ("" :) . filter (not . null)          <$> splitBySpaces cs
firstWord   (        '\t' : cs ) = firstWord (' ' : cs)
firstWord s@(        '\'' : _  ) = Command . filter (not . null) <$> splitBySpaces s
firstWord s@(        '\"' : _  ) = Command . filter (not . null) <$> splitBySpaces s
firstWord   (        '='  : cs ) = Assignment "" <$> parseValue cs
firstWord   (        c    : cs ) = prependChar c <$> firstWord cs

parseValue :: MonadError m => String -> m String
parseValue s = splitBySpaces s >>= \case
  [] -> pure ""
  [x] -> pure x
  (_ : _) -> throwError $ error' "Command calls with variable overriding are not supported"
  . filter (not . null)

prependChar :: Char -> Primitive -> Primitive
prependChar c (Assignment name value) = Assignment (c : name) value
prependChar c (Command [])            = Command [[c]]
prependChar c (Command (w : ws))      = Command $ (c : w) : ws

splitBySpaces :: MonadError m => String -> m [String]
splitBySpaces  ""                = pure [""]
splitBySpaces  "\\"              = throwError $ error' "Unexpected end of line after \\"
splitBySpaces ('\\' : '\\' : cs) = headMap ('\\' :) <$> splitBySpaces cs
splitBySpaces ('\\' : '\"' : cs) = headMap ('\"' :) <$> splitBySpaces cs
splitBySpaces ('\\' : '\'' : cs) = headMap ('\'' :) <$> splitBySpaces cs
splitBySpaces ('\\' : ' '  : cs) = headMap (' ' :) <$> splitBySpaces cs
splitBySpaces (       '\'' : cs) = singleQuotes cs >>= \(s, r) -> headMap (s ++) <$> splitBySpaces r
splitBySpaces (       '\"' : cs) = doubleQuotes cs >>= \(s, r) -> headMap (s ++) <$> splitBySpaces r
splitBySpaces (       ' '  : cs) = ("" :) <$> splitBySpaces cs
splitBySpaces (       '\t' : cs) = ("" :) <$> splitBySpaces cs
splitBySpaces (        c   : cs) = headMap (c :) <$> splitBySpaces cs

headMap :: (a -> a) -> [a] -> [a]
headMap _ []       = []
headMap f (x : xs) = f x : xs

singleQuotes :: MonadError m => String -> m (String, String)
singleQuotes "" = throwError $ error' "Unexpected end of line in single quotes"
singleQuotes ('\'' : cs) = pure ("", cs)
singleQuotes ( c   : cs) = first (c :) <$> singleQuotes cs

doubleQuotes :: MonadError m => String -> m (String, String)
doubleQuotes "" = throwError $ error' "Unexpected end of line in double quotes"
doubleQuotes ('\\' : '\\' : cs) = first ('\\' :) <$> doubleQuotes cs
doubleQuotes ('\\' : '\"' : cs) = first ('\"' :) <$> doubleQuotes cs
doubleQuotes ('\\' : '\'' : cs) = first ('\'' :) <$> doubleQuotes cs
doubleQuotes ('\\' : 'n'  : cs) = first ('\n' :) <$> doubleQuotes cs
doubleQuotes ('\\' : 't'  : cs) = first ('\t' :) <$> doubleQuotes cs
doubleQuotes (       '\"' : cs) = pure ("", cs)
doubleQuotes (        c   : cs) = first (c :) <$> doubleQuotes cs

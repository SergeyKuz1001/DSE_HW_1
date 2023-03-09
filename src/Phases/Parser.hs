{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Модуль для парсинга пользовательского запроса.

Весь этот модуль выглядел бы куда красивее на парсер-комбинаторах,
но их использовать явно запрещено в задании.
-}
module Phases.Parser ( parser ) where

import           Control.Monad
import           Data.Bifunctor         (first)
import           Data.Char
import qualified Data.Primitive         as P
import           Environment.MonadError (Error (..), MonadError, throwError)
import           Prelude

error' :: String -> Error
error' = Error "ParsingError"

data Primitive
  = Commands [[String]]
  | Assignment String String
  deriving (Eq, Show)

{- |
Функция для разбора строки после подстановки переменных.
Выделяет либо конвейер команд,
либо пустую команду (пустая строка или строка только из пробелов),
либо присваивание переменной.
Кидает исключение, если строка имеет некорректный синтаксис.
-}
parser :: MonadError m => String -> m P.Primitive
parser s = firstWord (dropWhile isSpace s) >>= removeNulls

{- |
Убирает пустые строки из команд,
затем проверяет, что все команды корректны,
т.е. содержат хотя бы одну строку.
-}
removeNulls :: MonadError m => Primitive -> m P.Primitive
removeNulls (Assignment x v) = pure $ P.Assignment x v
removeNulls (Commands [])    = pure $ P.Commands []
removeNulls (Commands lst)   = fmap P.Commands $ forM lst $ \args -> case filter (not . null) args of
  []       -> throwError $ error' "Empty command between pipes"
  (x : xs) -> pure $ x : xs

{- |
Определение первого слова.
Здесь происходит выбор типа строки:
пустая строка, конвейер команд, присваивание.
-}
firstWord :: MonadError m => String -> m Primitive
firstWord "" = pure $ Commands []
firstWord ('=' : cs) = Assignment "" <$> parseValue cs
firstWord (c : cs) | isIdent c = prependChar c <$> firstWord cs
firstWord (c : cs) | isSpace c = Commands . headMap ("" :) <$> parseCommands cs
firstWord s = Commands <$> parseCommands s

{- | Определяет корректные символы для идентификаторов -}
isIdent :: Char -> Bool
isIdent c = isAlphaNum c || isDigit c || c == '_'

{- |
Вспомогательная функция для сохранения первого слова в команде.
Конечный автомат сохраняет состояния в виде цепочки
>>> prependChar 'c' <$> prependChar 'm' <$> pure (Commands [["d", "arg1"]])
-}
prependChar :: Char -> Primitive -> Primitive
prependChar c (Assignment name value)      = Assignment (c : name) value
prependChar c (Commands [])                = Commands [[[c]]]
prependChar c (Commands ([] : cmds))       = Commands $ [[c]] : cmds
prependChar c (Commands ((w : ws) : cmds)) = Commands $ ((c : w) : ws) : cmds

{- | Чтение значения переменной в присваивании. -}
parseValue :: MonadError m => String -> m String
parseValue s = splitBySpaces s >>= \case
  ([], []) -> pure ""
  ([x], []) -> pure x
  (_ : _, _) -> throwError $ error' "Command calls with variable overriding are not supported"
  (_, _ : _) -> throwError $ error' "Unexpected pipe"
  . first (filter (not . null))

{- |
Разбиение конвейера команд с учётом пайпов.
Вызывается из firstWord либо начиная с последнего символа команды,
либо с символа после пробельного.
Не проверяет пустые команды, этим занимается уже removeNulls.
-}
parseCommands :: MonadError m => String -> m [[String]]
parseCommands "" = pure []
parseCommands s = do
  (args, rest) <- splitBySpaces s
  cmds <- parseCommands rest
  pure $ args : cmds

{- |
Разбиение строки по пробелам
с учётом кавычек (возможны пустые строки,
используется в связке с @filter (not . null)@.
Останавливается на символе конвейера @|@.
-}
splitBySpaces :: MonadError m => String -> m ([String], String)
splitBySpaces (        '|'  : cs ) = pure ([""], cs)
splitBySpaces   ""                 = pure ([""], "")
splitBySpaces   "\\"               = throwError $ error' "Unexpected end of line after \\"
splitBySpaces ( '\\' :  c   : cs ) = first (headMap (c :)) <$> splitBySpaces cs
splitBySpaces (        '\'' : cs ) = singleQuotes cs >>= \(s, r) -> first (headMap (s ++)) <$> splitBySpaces r
splitBySpaces (        '\"' : cs ) = doubleQuotes cs >>= \(s, r) -> first (headMap (s ++)) <$> splitBySpaces r
splitBySpaces (         c   : cs ) | isSpace c = first ("" :) <$> splitBySpaces cs
splitBySpaces (         c   : cs ) = first (headMap (c :)) <$> splitBySpaces cs

{- | Применение функции только к голове списка, если она есть. -}
headMap :: (a -> a) -> [a] -> [a]
headMap _ []       = []
headMap f (x : xs) = f x : xs

{- | Чтение фрагмента строки, заключённого в одинарные кавычки. -}
singleQuotes :: MonadError m => String -> m (String, String)
singleQuotes "" = throwError $ error' "Unexpected end of line in single quotes"
singleQuotes ('\'' : cs) = pure ("", cs)
singleQuotes ( c   : cs) = first (c :) <$> singleQuotes cs

{- | Чтение фрагмента строки, заключённого в двойные кавычки. -}
doubleQuotes :: MonadError m => String -> m (String, String)
doubleQuotes "" = throwError $ error' "Unexpected end of line in double quotes"
doubleQuotes (       '\"' : cs) = pure ("", cs)
doubleQuotes ('\\' : '\\' : cs) = first ('\\' :) <$> doubleQuotes cs
doubleQuotes ('\\' : '\"' : cs) = first ('\"' :) <$> doubleQuotes cs
doubleQuotes ('\\' : '\'' : cs) = first ('\'' :) <$> doubleQuotes cs
doubleQuotes ('\\' : 'n'  : cs) = first ('\n' :) <$> doubleQuotes cs
doubleQuotes ('\\' : 't'  : cs) = first ('\t' :) <$> doubleQuotes cs
doubleQuotes (        c   : cs) = first (c :) <$> doubleQuotes cs

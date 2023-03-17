{-# LANGUAGE LambdaCase #-}

{- | Модуль для парсинга пользовательского запроса.

Весь этот модуль выглядел бы куда красивее на парсер-комбинаторах,
но их использовать явно запрещено в задании.
-}
module Phases.Parser (parser) where

import qualified Data.ParsedPrimitive   as PP
import           Data.Error             (Error (..))
import           Monads.Error           (MonadError, throwError)

import           Control.Monad
import           Data.Bifunctor         (first)
import           Data.Char

-- | Сконструировать специфичную для модуля ошибку.
modError :: String -> Error
modError = Error "ParsingError"

-- | Выбросить специфичную для модуля ошибку.
throwModError :: MonadError m => String -> m a
throwModError = throwError . modError

-- | Приписать префикс только к голове списка,
-- если её нет, то создать.
headMappend :: Monoid a => a -> [a] -> [a]
headMappend x []       = [x]
headMappend x (y : ys) = x <> y : ys

-- | Наполовину готовый примитив, допустимы пустые строки в командах.
data Primitive
  = Commands [[String]]
  | Assignment String String
  deriving (Eq, Show)

-- | Функция для разбора строки после подстановки переменных.
-- Выделяет либо конвейер команд,
-- либо пустую команду (пустая строка или строка только из пробелов),
-- либо присваивание переменной.
-- Кидает исключение, если строка имеет некорректный синтаксис.
parser :: MonadError m => String -> m PP.Primitive
parser s = firstWord (dropWhile isSpace s) >>= removeNulls

-- | Убирает пустые строки из команд,
-- затем проверяет, что все команды корректны,
-- т.е. содержат хотя бы одну строку.
removeNulls :: MonadError m => Primitive -> m PP.Primitive
removeNulls (Assignment x v) = pure $ PP.Assignment x v
removeNulls (Commands []) = pure $ PP.Commands []
removeNulls (Commands lst) = fmap PP.Commands $ forM lst $ \case
  []       -> throwModError "Empty command between pipes"
  (x : xs) -> pure $ x : xs

-- | Определение первого слова.
-- Здесь происходит выбор типа строки:
-- пустая строка, конвейер команд, присваивание.
firstWord :: MonadError m => String -> m Primitive
firstWord "" = pure $ Commands []
firstWord ('=' : cs) = Assignment "" <$> parseValue cs
firstWord (c : cs) | isIdent c = prependChar c <$> firstWord cs
firstWord (c : cs) | isSpace c = Commands . headMappend [""] <$> parseCommands cs
firstWord s = Commands <$> parseCommands s

-- | Определяет корректные символы для идентификаторов
isIdent :: Char -> Bool
isIdent c = isAlphaNum c || isDigit c || c == '_'

-- | Вспомогательная функция для сохранения первого слова в команде.
-- Конечный автомат сохраняет состояния в виде цепочки
-- >>> prependChar 'c' <$> prependChar 'm' <$> pure (Commands [["d", "arg1"]])
-- Commands [["cmd","arg1"]]
prependChar :: Char -> Primitive -> Primitive
prependChar c (Assignment name value)      = Assignment (c : name) value
prependChar c (Commands [               ]) = Commands [[[c]]]
prependChar c (Commands ([      ] : cmds)) = Commands $ [[c]] : cmds
prependChar c (Commands ((w : ws) : cmds)) = Commands $ ((c : w) : ws) : cmds

-- | Чтение значения переменной в присваивании.
parseValue :: MonadError m => String -> m String
parseValue s =
  splitBySpaces s
    >>= \case
      ([ ], []) -> pure ""
      ([x], []) -> pure x
      (_ : _, _) -> throwModError "Command calls with variable overriding are not supported"
      (_, _ : _) -> throwModError "Unexpected pipe"
      . first (filter (not . null))

-- | Разбиение конвейера команд с учётом пайпов.
-- Вызывается из firstWord либо начиная с последнего символа команды,
-- либо с символа после пробельного.
-- Не проверяет пустые команды, этим занимается уже removeNulls.
parseCommands :: MonadError m => String -> m [[String]]
parseCommands "" = pure []
parseCommands s = do
  (args, rest) <- splitBySpaces $ dropWhile isSpace s
  cmds <- parseCommands rest
  pure $ args : cmds

-- | Разбиение строки по пробелам
-- с учётом кавычек (возможны пустые строки,
-- используется в связке с @filter (not . null)@.
-- Останавливается на символе конвейера @|@.
splitBySpaces :: MonadError m => String -> m ([String], String)
splitBySpaces "" = pure ([], "")
splitBySpaces ('|' : cs) = pure ([], cs)
splitBySpaces "\\" = throwModError "Unexpected end of line after \\"
splitBySpaces ('\\' : c : cs) = first (headMappend [c]) <$> splitBySpaces cs
splitBySpaces ('\'' : cs) = singleQuotes cs >>= \(s, r) -> first (headMappend s) <$> splitBySpaces r
splitBySpaces ('\"' : cs) = doubleQuotes cs >>= \(s, r) -> first (headMappend s) <$> splitBySpaces r
splitBySpaces (c : cs) | isSpace c = first ("" :) <$> splitBySpaces (dropWhile isSpace cs)
splitBySpaces (c : cs) = first (headMappend [c]) <$> splitBySpaces cs

-- | Чтение фрагмента строки, заключённого в одинарные кавычки.
singleQuotes :: MonadError m => String -> m (String, String)
singleQuotes "" = throwModError "Unexpected end of line in single quotes"
singleQuotes ('\'' : cs) = pure ("", cs)
singleQuotes (c : cs) = first (c :) <$> singleQuotes cs

-- | Чтение фрагмента строки, заключённого в двойные кавычки.
doubleQuotes :: MonadError m => String -> m (String, String)
doubleQuotes "" = throwModError "Unexpected end of line in double quotes"
doubleQuotes "\\" = throwModError "Unexpected end of line after \\"
doubleQuotes ('\"' : cs) = pure ("", cs)
doubleQuotes ('\\' : c : cs) = first (parseEscape c :) <$> doubleQuotes cs
doubleQuotes (c : cs) = first (c :) <$> doubleQuotes cs

-- | Распознавание экранированного символа.
parseEscape :: Char -> Char
parseEscape 'n' = '\n'
parseEscape 't' = '\t'
parseEscape c   = c

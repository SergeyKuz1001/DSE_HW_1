{-# LANGUAGE NoImplicitPrelude #-}

{- |
В данном модуле определена абстрактная точка входа в программу @'abstractMain'@.
Абстрактной она называется потому, что она описывает работу программы в терминах
абстрактной монады, являющейся также представителем различных классов,
добавляющих различные функции (функции для работы с переменными окружения,
функции для работы с файловой системой и прочие).
-}
module AbstractMain (
    abstractMain,
  ) where

import Data.DebugInfo
import Monads.Error (MonadError, catchError)
import Monads.Exit (MonadExit)
import Monads.IO (MonadIO, print)
import Monads.PM (MonadPM)
import Monads.DirScanner (MonadDirScanner)
import Monads.SelfReferenced (MonadSelfReferenced)
import Monads.VarReader (MonadVarReader)
import Monads.VarWriter (MonadVarWriter)
import Phases.Analyzer (analyzer)
import Phases.Executor (executor)
import Phases.Linker (linker)
import Phases.Parser (parser)
import Phases.Reader (reader)
import Phases.Substitutor (substitutor)

import Control.Monad (forever)
import Prelude hiding (print)

-- | Абстрактная точка входа в программу, главный цикл программы.
--
-- Каждая итерация цикла представляет собой последовательность обработчиков
-- @'reader'@ → @'substitutor'@ → @'parser'@ → @'analyzer'@ → @'linker'@ →
-- @'executor'@. При возникновении ошибки на любом из этапов происходит печать
-- ошибки и начало новой итерации. Цикл бесконечен, выход из него (и из
-- программы в целом) происходит посредством вызова функции @'exit'@ класса
-- @'MonadExit'@ в @'executor'@.
abstractMain ::
  ( MonadError m
  , MonadExit m
  , MonadIO m
  , MonadDirScanner m
  , MonadPM m
  , MonadSelfReferenced m
  , MonadVarReader m
  , MonadVarWriter m) =>
  m ()
abstractMain = forever $ (
    reader      >>= debugIfNecessary "Reader" 'r' >>=
    substitutor >>= debugIfNecessary "Substitutor" 's' >>=
    parser      >>= debugIfNecessary "Parser" 'p' >>=
    analyzer    >>= debugIfNecessary "Analyzer" 'a' >>=
    linker      >>= debugIfNecessary "Linker" 'l' >>=
    executor
  ) `catchError` print

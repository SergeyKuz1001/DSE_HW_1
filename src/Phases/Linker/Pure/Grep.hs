{-# LANGUAGE OverloadedStrings #-}

{- |
Модуль с внутренней чистой командой @grep@.
-}
module Phases.Linker.Pure.Grep(
    grep,
  ) where

import Data.Text.Lazy as L (append, Text, unlines, pack, lines, null)
import Text.Regex.TDFA
import Data.AnalyzedPrimitive (GrepArgs (..))
import Data.Bool (bool)

-- | Команда @grep@ принимает текст и регулярное выражение по которому
-- возвращает отфильтрованный текст.
grep :: GrepArgs -> Text -> Text
grep GrepArgs {
  fullWords = isFullWords,
  ignoreCase = ignoreCase,
  regex = regex,
  lineCount = lineCount
  } text =
      let getRegex =
            let compOut = defaultCompOpt { multiline = True, caseSensitive = not ignoreCase }
                execOptions = defaultExecOpt
                toRegex = makeRegexOpts compOut execOptions
                pattern = pack regex
            in if isFullWords
              then toRegex ("\\<" `append` pattern `append` "\\>")
              else toRegex pattern
          allLines = reverse . snd $ foldl
            (\(remainedCount, xs) line ->
              let isMatched = match getRegex line
              in if isMatched
                 then (lineCount, line : xs)
                 else (max (remainedCount - 1) 0,
                       bool xs (line : xs) (remainedCount > 0)))
            (0 :: Int, []) (L.lines text)
      in L.unlines allLines

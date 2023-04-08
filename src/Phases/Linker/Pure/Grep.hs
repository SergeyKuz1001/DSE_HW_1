{- |
Модуль с внутренней чистой командой @grep@.
-}
module Phases.Linker.Pure.Grep(
    grep,
  ) where

import Data.AnalyzedPrimitive (GrepArgs (..))

import Data.Array ((!))
import Data.Bool (bool)
import Data.Text.Lazy as L (Text, concat, unlines, lines, splitAt, pack)
import System.Console.ANSI
import Text.Regex.TDFA

-- | Команда @grep@ принимает текст и регулярное выражение по которому
-- возвращает отфильтрованный текст.
grep :: GrepArgs -> Text -> Text
grep GrepArgs {
  isColorized = isColorized,
  regex = (_, regex),
  lineCount = lineCount
  } text =
      let colorizeLine match line =
            let (start, len) = match ! 0
                (leftPart, middlePart') = L.splitAt (fromIntegral start) line
                (middlePart, rightPart) = L.splitAt (fromIntegral len) middlePart'
            in  L.concat [
                    leftPart,
                    L.pack $ setSGRCode [SetColor Foreground Dull Red, SetConsoleIntensity BoldIntensity],
                    middlePart,
                    L.pack $ setSGRCode [Reset],
                    rightPart
                  ]
          allLines = reverse . snd $ foldl
            (\(remainedCount, xs) line ->
              let matches = matchAll regex line
              in if not $ null matches
                 then (lineCount, foldr (bool (\_ -> id) colorizeLine isColorized) line matches : xs)
                 else (max (remainedCount - 1) 0,
                       bool xs (line : xs) (remainedCount > 0)))
            (0 :: Int, []) (L.lines text)
      in L.unlines allLines

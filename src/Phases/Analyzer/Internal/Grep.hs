{-# LANGUAGE TupleSections #-}

module Phases.Analyzer.Internal.Grep (
    GrepArgs (..),
    grepRegex,
    grep,
  ) where

import Data.FSObjects (AbsFilePath)
import Phases.Analyzer.Common (toError, findReadable)

import Monads.Error (MonadError, throwError, (?:), (|>=))
import Monads.FS (MonadFS)
import Monads.PwdReader (MonadPwdReader)

import Options.Applicative
import Text.Regex.TDFA

-- | Конфигурация команды @grep@.
data GrepArgs = GrepArgs
  { isColorized :: Bool
  , lineCount :: Int
  , regex :: (String, Regex)
  , inputFile :: Maybe AbsFilePath
  }

instance Eq GrepArgs where
  GrepArgs ic1 lc1 (re1, _) if1 == GrepArgs ic2 lc2 (re2, _) if2 =
    ic1 == ic2 && lc1 == lc2 && re1 == re2 && if1 == if2

instance Show GrepArgs where
  show (GrepArgs ic lc (re, _) if_) =
    "GrepArgs { " ++
    "isColorized = " ++ show ic ++ ", " ++
    "lineCount = " ++ show lc ++ ", " ++
    "regex = " ++ show re ++ ", " ++
    "inputFile = " ++ show if_ ++
    " }"

-- | Промежуточное представление конфигурации команды @grep@.
data GrepArgsRaw = GrepArgsRaw Bool Bool Int String (Maybe FilePath)
  deriving (Eq, Show)

grepRegex :: MonadError m => Bool -> Bool -> String -> m (String, Regex)
grepRegex isFullWords caseIsIgnored regexStr =
  let regexStr' =
        if isFullWords
          then "\\<" ++ regexStr ++ "\\>"
          else regexStr
      compOut = defaultCompOpt { multiline = True, caseSensitive = not caseIsIgnored }
      execOptions = defaultExecOpt
      regexStr'' =
        if not isFullWords && not caseIsIgnored
          then regexStr
          else "[:" ++ (if isFullWords then "w" else "") ++ (if caseIsIgnored then "i" else "") ++ "]" ++ regexStr
  in (regexStr'',) <$> (makeRegexOptsM compOut execOptions regexStr' |>= toError . ("incorrect regular expression: " ++))

-- | Функция парсинга аргументов командной строки в конфигурацию.
grep :: (MonadError m, MonadFS m, MonadPwdReader m) => [String] -> m GrepArgs
grep args =
  case execParserPure defaultPrefs argparseInfo args of
    Success (GrepArgsRaw isFullWords caseIsIgnored lineCount regexStr mPath) -> do
      lineCount >= 0 ?: toError "Negative count of lines after matched one is not allowed"
      (regexStr'', regex) <- grepRegex isFullWords caseIsIgnored regexStr
      mAbsPath <- traverse findReadable mPath
      return $ GrepArgs True lineCount (regexStr'', regex) mAbsPath
    Failure pf -> let (h, _, _) = execFailure pf "grep" in throwError . toError $ show h
    CompletionInvoked _ -> throwError $ toError "Impossible situation occured"
  where
    argparse :: Parser GrepArgsRaw
    argparse = GrepArgsRaw
      <$> switch (short 'w' <> long "words" <> help "Match only whole words")
      <*> switch (short 'i' <> long "ignorecase" <> help "Ignore case")
      <*> option auto (short 'A' <> long "lines" <> value 0 <> metavar "COUNT" <> help "How many lines to show around the matched ones")
      <*> strArgument (metavar "REGEX" <> help "Regular expression")
      <*> (pure Nothing <|> Just <$> strArgument (metavar "FILE" <> help "File to use as source (if not present, standard input is used)"))
      <**> helper
    argparseInfo :: ParserInfo GrepArgsRaw
    argparseInfo = info argparse fullDesc

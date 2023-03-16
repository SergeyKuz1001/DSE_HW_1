module Phases.Linker.Commands (
    cat,
    echo,
    wc,
  ) where

import Data.Bool (bool)
import qualified Data.ByteString.Char8 as BS
import Data.Char (isSpace)

cat :: String -> String
cat = id

echo :: [String] -> String
echo = (++ "\n") . unwords

wc :: String -> String
wc text =
  let bytes = BS.pack text
      checkOnLine c = bool 0 1 $ c == '\n'
      checkOnWord c isPS = bool 0 1 $ not isPS && isSpace c
      wcgo (cLs, cWs, cBs, isPS) c =
        (cLs + checkOnLine c, cWs + checkOnWord c isPS, cBs + 1, isSpace c)
      (countLines, countWords, countBytes, isPrevSpace) = BS.foldl' wcgo (0, 0, 0, False) bytes
      countWords' = countWords + bool 1 0 isPrevSpace
  in  show countLines ++ "\t" ++ show countWords' ++ "\t" ++ show countBytes ++ "\n"

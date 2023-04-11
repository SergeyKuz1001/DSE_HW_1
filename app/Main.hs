{- |
В данном модуле определена точка входа в программу @'main'@.
-}
module Main (
    main,
  ) where

import AbstractMain (abstractMain)
import Environment (runEnvironment)

import Data.Maybe (listToMaybe)
import System.Environment (getArgs)
import System.IO (BufferMode(..), hSetBuffering, stdin, stdout, stderr)

-- | Точка входа в программу. Здесь настраивается буфферизация стандартных
-- потоков и запускается более абстрактная точка входа @'abstractMain'@ в монаде
-- @'Environment'@.
--
-- При указании флага @--work-as-cat@ программа будет считывать содержимое из
-- стандартного входа и писать на стандартный выход.
main :: IO ()
main = do
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  args <- getArgs
  if listToMaybe args == Just "--work-as-cat"
    then getContents >>= putStr
    else runEnvironment abstractMain

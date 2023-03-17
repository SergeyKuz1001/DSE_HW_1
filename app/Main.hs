{- |
В данном модуле определена точка входа в программу @'main'@.
-}
module Main (
    main,
  ) where

import AbstractMain (abstractMain)
import Environment (runEnvironment)

import System.IO (BufferMode(..), hSetBuffering, stdin, stdout, stderr)

-- | Точка входа в программу. Здесь настраивается буфферизация стандартных
-- потоков и запускается более абстрактная точка входа @'abstractMain'@ в монаде
-- @'Environment'@.
main :: IO ()
main = do
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  runEnvironment abstractMain

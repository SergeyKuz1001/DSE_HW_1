module Main (
    main,
  ) where

import Enviroment
import Phases

import System.Exit (exitWith)

main :: IO ()
main = do
  mExitCode <- runEnviroment Nothing $
    stringReader >>= parser >>= analyzer >>= executor
  case mExitCode of
    Nothing -> main
    Just ec -> exitWith ec

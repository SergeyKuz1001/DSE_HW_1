module Phases.Linker.Tests (
    testsLinker,
  ) where

import qualified Data.AnalyzedPrimitive as AP
import Data.LinkedPrimitive
import Phases.Linker (linker)
import Phases.Linker.TestEnvironment
import Environment.FSPrimitive

import Data.List.NonEmpty (NonEmpty(..))
import System.FilePath (pathSeparator)
import Test.HUnit

replace :: Eq a => a -> a -> [a] -> [a]
replace x y = foldr (\z -> if x == z then (y:) else (z:)) []

updPath :: FilePath -> FilePath
updPath path =
  if pathSeparator == '/'
    then path
    else replace '/' '\\' path

updAbsPath :: FilePath -> AbsFilePath
updAbsPath path = either (error "it isn't AbsFilePath") id . absFilePath $
  if pathSeparator == '/'
    then path
    else "C:" ++ replace '/' '\\' path

env1 :: TestEnvironment a -> a
env1 = runTestEnvironment (updAbsPath "/home/user/")

mkTest :: [AP.Common] -> Primitive -> Test
mkTest [] lPrim = TestCase $ env1 (linker AP.Empty) @?= lPrim
mkTest (cmn : cmns) lPrim = TestCase $ env1 (linker . AP.Commons $ cmn :| cmns) @?= lPrim

testsLinker :: Test
testsLinker = TestList [
    mkTest [] $
      Commands []
  ]

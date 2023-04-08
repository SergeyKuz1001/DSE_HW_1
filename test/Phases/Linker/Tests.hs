module Phases.Linker.Tests (
    testsLinker,
  ) where

import Data.AnalyzedPrimitive
import qualified Data.LinkedPrimitive as LP
import Data.Handles
import Phases.Linker (linker)
import Phases.Linker.TestEnvironment

import Data.List.NonEmpty (NonEmpty(..))
import Data.Text.Lazy (pack)
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
env1 = runTestEnvironment

mkTest :: [Common] -> [LP.CommonWithHandles] -> Test
mkTest [] cwhs = TestCase $ env1 (linker Empty) @?= LP.Commons cwhs
mkTest (cmn : cmns) cwhs = TestCase $ env1 (linker . Commons $ cmn :| cmns) @?= LP.Commons cwhs

externC :: String -> [String] -> LP.Common
externC path args = LP.External $ LP.Arguments (updAbsPath path) args

selfAsCat :: LP.Common
selfAsCat = externC "/home/user/cli-exe" ["--work-as-cat"]

pureC :: String -> LP.Common
pureC name = LP.Internal $ LP.Pure name undefined

impurePwd :: LP.Common
impurePwd = LP.Internal $ LP.Impure LP.Pwd

testsLinker :: Test
testsLinker = TestList [
    mkTest []
      [],
    mkTest [Internal $ Cat Nothing]
      [(selfAsCat, FromParentHandle, ToStdout)],
    mkTest [Internal $ Cat (Just $ updAbsPath "/any_path.txt")]
      [(pureC "cat", FromFile $ updAbsPath "/any_path.txt", ToStdout)],
    mkTest [Internal $ Echo ["123","456","789","0"]]
      [(pureC "cat", FromString $ pack "123 456 789 0\n", ToStdout)],
    mkTest [Internal $ Echo []]
      [(pureC "cat", FromString $ pack "\n", ToStdout)],
    mkTest [Internal $ Wc Nothing]
      [(selfAsCat, FromParentHandle, ToNewPipe), (pureC "wc", FromParentHandle, ToStdout)],
    mkTest [Internal $ Wc (Just $ updAbsPath "/any_path.txt")]
      [(pureC "wc", FromFile $ updAbsPath "/any_path.txt", ToStdout)],
    mkTest [Internal $ Pwd]
      [(impurePwd, FromString $ pack "", ToStdout)],
    mkTest [External $ Arguments (updAbsPath "/usr/bin/some_cmd") ["1", "1grwe d"]]
      [(externC "/usr/bin/some_cmd" ["1", "1grwe d"], FromParentHandle, ToStdout)],
    mkTest [Internal $ Cat Nothing , Internal $ Cat Nothing]
      [(selfAsCat, FromParentHandle, ToStdout)],
    mkTest [External $ Arguments (updAbsPath "/usr/bin/some_cmd") [], Internal $ Cat Nothing]
      [(externC "/usr/bin/some_cmd" [], FromParentHandle, ToStdout)],
    mkTest [Internal $ Cat Nothing, External $ Arguments (updAbsPath "/usr/bin/some_cmd") ["2"]]
      [(externC "/usr/bin/some_cmd" ["2"], FromParentHandle, ToStdout)],
    mkTest [Internal $ Cat Nothing, External $ Arguments (updAbsPath "/usr/bin/some_cmd") ["3"], Internal $ Cat Nothing]
      [(externC "/usr/bin/some_cmd" ["3"], FromParentHandle, ToStdout)],
    mkTest [External $ Arguments (updAbsPath "/usr/bin/cmd1") [], Internal $ Cat Nothing, External $ Arguments (updAbsPath "/usr/bin/cmd2") []]
      [(externC "/usr/bin/cmd1" [], FromParentHandle, ToNewPipe), (externC "/usr/bin/cmd2" [], FromParentHandle, ToStdout)],
    mkTest [Internal Pwd, Internal Pwd, Internal Pwd]
      [(impurePwd, FromString $ pack "", ToStdout)],
    mkTest [Internal Pwd, Internal $ Cat Nothing, Internal Pwd, Internal Pwd]
      [(impurePwd, FromString $ pack "", ToStdout)],
    mkTest [Internal Pwd, Internal $ Cat (Just $ updAbsPath "/any_path.txt"), Internal Pwd, Internal Pwd]
      [(impurePwd, FromString $ pack "", ToStdout)],
    mkTest [Internal Pwd, External $ Arguments (updAbsPath "/usr/bin/cat") [], Internal Pwd, Internal Pwd]
      [(impurePwd, FromString $ pack "", ToNewPipe), (externC "/usr/bin/cat" [], FromParentHandle, ToNowhere), (impurePwd, FromString $ pack "", ToStdout)],
    mkTest [External $ Arguments (updAbsPath "/usr/bin/cat") [], Internal Pwd]
      [(externC "/usr/bin/cat" [], FromParentHandle, ToNowhere), (impurePwd, FromString $ pack "", ToStdout)],
    mkTest [Internal $ Echo ["143", "advs r "], Internal $ Cat (Just $ updAbsPath "/some_dir/some_file.some_ext")]
      [(pureC "cat", FromFile $ updAbsPath "/some_dir/some_file.some_ext", ToStdout)],
    mkTest [External $ Arguments (updAbsPath "/cmd") ["1"], External $ Arguments (updAbsPath "/cmd") ["2"], External $ Arguments (updAbsPath "/cmd") ["3"]]
      [(externC "/cmd" ["1"], FromParentHandle, ToNewPipe), (externC "/cmd" ["2"], FromParentHandle, ToNewPipe), (externC "/cmd" ["3"], FromParentHandle, ToStdout)],
    mkTest [Internal $ Cat (Just $ updAbsPath "/text1.txt"), Internal $ Cat Nothing, Internal $ Echo ["123", "61 f"], Internal $ Cat (Just $ updAbsPath "/text2.txt"), Internal $ Cat Nothing]
      [(pureC "cat", FromFile $ updAbsPath "/text2.txt", ToStdout)],
    mkTest [Internal $ Echo ["6"], Internal $ Cat (Just $ updAbsPath "/text1.txt"), Internal $ Cat Nothing, Internal $ Cat Nothing, External $ Arguments (updAbsPath "/cmd1") ["qqq"], Internal Pwd, Internal $ Echo ["1y3", "71g 2"], Internal $ Cat Nothing, Internal $ Cat (Just $ updAbsPath "/text2.txt"), Internal Pwd, External $ Arguments (updAbsPath "/cmd2") ["www"], Internal $ Cat Nothing, Internal $ Wc Nothing, Internal $ Wc Nothing, Internal $ Wc (Just $ updAbsPath "/text3.txt")]
      [(externC "/cmd1" ["qqq"], FromFile (updAbsPath "/text1.txt"), ToNowhere), (impurePwd, FromString $ pack "", ToNewPipe), (externC "/cmd2" ["www"], FromParentHandle, ToNewPipe), (pureC "wc.wc", FromParentHandle, ToNowhere), (pureC "wc", FromFile $ updAbsPath "/text3.txt", ToStdout)],
    mkTest [Internal $ Cat Nothing, Internal Pwd, Internal $ Cat Nothing]
      [(selfAsCat, FromParentHandle, ToNowhere), (impurePwd, FromString $ pack "", ToStdout)]
  ]

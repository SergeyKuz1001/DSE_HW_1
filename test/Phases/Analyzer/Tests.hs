module Phases.Analyzer.Tests (
    testsAnalyzer,
  ) where

import qualified Data.Primitive as P
import Data.ImprovedPrimitive hiding (Primitive(..))
import qualified Data.ImprovedPrimitive as IP
import Phases.Analyzer (analyzer)
import TestEnviroment

import Data.List.NonEmpty (NonEmpty(..))
import Test.HUnit hiding (test)

env1 :: TestEnviroment a -> Either Error a
env1 = (fmap . fmap) fst $ runTestEnviroment "/home/user/" [
    File "/bin/vim" True,
    File "/home/user/Documents/lessons_schedule.txt" False,
    File "/home/user/my_game.py" True,
    File "/home/user/.vimrc" False,
    File "/GitHub_PASSWORD.txt" False
  ] []

test :: (Eq b, Show b) => (TestEnviroment IP.Primitive -> Either Error b) -> [String] -> Maybe b -> Test
test _ [] _ = TestCase $ fail "Incorrect test"
test f (command : args) expected = TestCase $
  (eitherToMaybe . f) (analyzer . P.Command $ command :| args) @?= expected
    where
      eitherToMaybe (Right x) = Just x
      eitherToMaybe (Left _) = Nothing

testsAnalyzer :: Test
testsAnalyzer = TestList [
    test env1 ["exit", "6"] $
      Just (IP.Command . Special . Exit $ Just 6),
    test env1 ["echo", "1", "2"] $
      Just (IP.Command . Common . Internal $ Echo ["1", "2"]),
    test env1 ["cat", ".vimrc"] $
      Just (IP.Command . Common . Internal $ Cat ".vimrc"),
    test env1 ["cat", "/GitHub_PASSWORD.txt"] $
      Just (IP.Command . Common . Internal $ Cat "/GitHub_PASSWORD.txt"),
    test env1 ["pwd"] $
      Just (IP.Command . Common . Internal $ Pwd),
    test env1 ["/bin/vim", "-O", ".vimrc", "my_game.py"] $
      Just (IP.Command . Common . External $ Arguments "/bin/vim" ["-O", ".vimrc", "my_game.py"]),
    test env1 ["my_game.py"] $
      Just (IP.Command . Common . External $ Arguments "/home/user/my_game.py" []),
    test env1 ["exit", "hahaha"] Nothing,
    test env1 ["cat"] Nothing,
    test env1 ["cat", "123"] Nothing,
    test env1 ["wc", ".vimrc", "my_game.py"] Nothing,
    test env1 ["emacs"] Nothing,
    test env1 ["pwd", "."] Nothing
  ]

module Phases.Analyzer.Tests (
    testsAnalyzer,
  ) where

import qualified Data.Primitive as P
import Data.ImprovedPrimitive hiding (Primitive(..))
import qualified Data.ImprovedPrimitive as IP
import Environment.MonadFS.Internal
import Phases.Analyzer (analyzer)
import Phases.Analyzer.TestEnvironment

import Data.List.NonEmpty (NonEmpty(..))
import Test.HUnit hiding (test)

env1 :: TestEnvironment a -> Either Error a
env1 = runTestEnvironment (AbsFilePath "/home/user/") [
    File (AbsFilePath "/bin/vim") $ Permissions True True True,
    File (AbsFilePath "/home/user/Documents/lessons_schedule.txt") $ Permissions True True False,
    File (AbsFilePath "/home/user/my_game.py") $ Permissions True True True,
    File (AbsFilePath "/home/user/.vimrc") $ Permissions True True False,
    File (AbsFilePath "/GitHub_PASSWORD.txt") $ Permissions True True False
  ]

test :: (Eq b, Show b) => (TestEnvironment IP.Primitive -> Either Error b) -> [String] -> Maybe b -> Test
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
      Just (IP.Command . Common . Internal . Cat $ AbsFilePath "/home/user/.vimrc"),
    test env1 ["cat", "/GitHub_PASSWORD.txt"] $
      Just (IP.Command . Common . Internal . Cat $ AbsFilePath "/GitHub_PASSWORD.txt"),
    test env1 ["pwd"] $
      Just (IP.Command . Common . Internal $ Pwd),
    test env1 ["/bin/vim", "-O", ".vimrc", "my_game.py"] $
      Just (IP.Command . Common . External $ Arguments (AbsFilePath "/bin/vim") ["-O", ".vimrc", "my_game.py"]),
    test env1 ["my_game.py"] $
      Just (IP.Command . Common . External $ Arguments (AbsFilePath "/home/user/my_game.py") []),
    test env1 ["exit", "hahaha"] Nothing,
    test env1 ["cat"] Nothing,
    test env1 ["cat", "123"] Nothing,
    test env1 ["wc", ".vimrc", "my_game.py"] Nothing,
    test env1 ["emacs"] Nothing,
    test env1 ["pwd", "."] Nothing
  ]

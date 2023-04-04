module Phases.Analyzer.Tests (
    testsAnalyzer,
  ) where

import qualified Data.Primitive as P
import Data.ImprovedPrimitive
import Data.VarName
import Phases.Analyzer (analyzer)
import Phases.Analyzer.TestEnvironment
import Environment.MonadExit (ExitCode(..))

import Data.List.NonEmpty (fromList)
import Data.Maybe (fromMaybe)
import System.FilePath (pathSeparator)
import Test.HUnit hiding (path)

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

updAbsPath' :: FilePath -> FilePath
updAbsPath' = asFilePath . updAbsPath

file :: FilePath -> (Bool, Bool, Bool) -> File
file path (rp, wp, ep) = File (updAbsPath path) (Permissions rp wp ep)

env1 :: TestEnvironment a -> Either Error a
env1 = runTestEnvironment (updAbsPath "/home/user/") [updAbsPath "/bin", updAbsPath "/usr/bin", updAbsPath "/home/user/.local/bin"] [
    file "/GitHub_PASSWORD.txt" (False, True, False),
    file "/bin/vim" (True, True, True),
    file "/bin/qsh" (True, True, False),
    file "/bin/sh" (False, False, True),
    file "/home/user/Documents/algo04.pdf" (True, True, False),
    file "/home/user/Documents/lessons_schedule.txt" (True, True, False),
    file "/home/user/my_game.py" (True, True, True),
    file "/home/user/some_doc.djvu" (True, False, False),
    file "/home/user/something_unreadable.txt" (False, True, False),
    file "/home/user/.local/zsh" (True, False, True),
    file "/home/user/.local/.stack_settings" (True, False, False),
    file "/home/user/.vimrc" (True, True, False),
    file "/some_VERY_VERY_IMPORTANT_file.txt" (True, True, False),
    file "/usr/bin/emacs-28.1/do_something" (True, True, True),
    file "/usr/bin/emacs-28.1" (True, True, True)
  ]

mkTest :: P.Primitive -> Maybe Primitive -> Test
mkTest prim expected = TestCase $ (eitherToMaybe . env1) (analyzer prim) @?= expected
  where
    eitherToMaybe (Right x) = Just x
    eitherToMaybe (Left _) = Nothing

mkTestOneCommand :: [String] -> Maybe Primitive -> Test
mkTestOneCommand = mkTest . P.Command

mkTestAssignment :: String -> String -> Maybe Primitive -> Test
mkTestAssignment var value = mkTest $ P.Assignment (either undefined id $ varName var) value

testsAnalyzer :: Test
testsAnalyzer = TestList [
  -- command
  --   special
  --     exit
    mkTestOneCommand ["exit"] $
      Just (Command . Special . Exit $ Nothing),
    mkTestOneCommand ["exit", "6"] $
      Just (Command . Special . Exit $ Just 6),
    mkTestOneCommand ["exit", "hahaha"]
      Nothing,
  --   internal
  --     cat
    mkTestOneCommand ["cat"] $
      Nothing,
    mkTestOneCommand ["cat", "123"]
      Nothing,
    mkTestOneCommand ["cat", ".vimrc"] $
      Just (Command . Common . Internal . Cat $ updAbsPath "/home/user/.vimrc"),
    mkTestOneCommand ["cat", "./.vimrc"] $
      Just (Command . Common . Internal . Cat $ updAbsPath "/home/user/.vimrc"),
    mkTestOneCommand ["cat", "vim"]
      Nothing,
    mkTestOneCommand ["cat", "something_unreadable.txt"]
      Nothing,
    mkTestOneCommand ["cat", updAbsPath' "/GitHub_PASSWORD.txt"] $
      Nothing,
    mkTestOneCommand ["cat", updAbsPath' "/some_VERY_VERY_IMPORTANT_file.txt"] $
      Just (Command . Common . Internal . Cat $ updAbsPath "/some_VERY_VERY_IMPORTANT_file.txt"),
    mkTestOneCommand ["cat", updPath "../../some_VERY_VERY_IMPORTANT_file.txt"] $
      Just (Command . Common . Internal . Cat $ updAbsPath "/some_VERY_VERY_IMPORTANT_file.txt"),
    mkTestOneCommand ["cat", updPath "Documents/algo04.pdf"] $
      Just (Command . Common . Internal . Cat $ updAbsPath "/home/user/Documents/algo04.pdf"),
    mkTestOneCommand ["cat", updAbsPath' "/usr/bin/emacs-28.1/do_something"] $
      Just (Command . Common . Internal . Cat $ updAbsPath "/usr/bin/emacs-28.1/do_something"),
    mkTestOneCommand ["cat", ".vimrc", ".vimrc"]
      Nothing,
    mkTestOneCommand ["cat", ".vimrc", "my_game.py"]
      Nothing,
    mkTestOneCommand ["cat", "some_doc.djvu", ".vimrc"]
      Nothing,
  --     echo
    mkTestOneCommand ["echo"] $
      Just (Command . Common . Internal $ Echo []),
    mkTestOneCommand ["echo", "bf q43h wui"] $
      Just (Command . Common . Internal $ Echo ["bf q43h wui"]),
    mkTestOneCommand ["echo", "1", "2"] $
      Just (Command . Common . Internal $ Echo ["1", "2"]),
  --     wc
    mkTestOneCommand ["wc"] $
      Nothing,
    mkTestOneCommand ["wc", "123"]
      Nothing,
    mkTestOneCommand ["wc", ".vimrc"] $
      Just (Command . Common . Internal . Wc $ updAbsPath "/home/user/.vimrc"),
    mkTestOneCommand ["wc", "./.vimrc"] $
      Just (Command . Common . Internal . Wc $ updAbsPath "/home/user/.vimrc"),
    mkTestOneCommand ["wc", "vim"]
      Nothing,
    mkTestOneCommand ["wc", "something_unreadable.txt"]
      Nothing,
    mkTestOneCommand ["wc", updAbsPath' "/GitHub_PASSWORD.txt"] $
      Nothing,
    mkTestOneCommand ["wc", updAbsPath' "/some_VERY_VERY_IMPORTANT_file.txt"] $
      Just (Command . Common . Internal . Wc $ updAbsPath "/some_VERY_VERY_IMPORTANT_file.txt"),
    mkTestOneCommand ["wc", updPath "../../some_VERY_VERY_IMPORTANT_file.txt"] $
      Just (Command . Common . Internal . Wc $ updAbsPath "/some_VERY_VERY_IMPORTANT_file.txt"),
    mkTestOneCommand ["wc", updPath "Documents/algo04.pdf"] $
      Just (Command . Common . Internal . Wc $ updAbsPath "/home/user/Documents/algo04.pdf"),
    mkTestOneCommand ["wc", updAbsPath' "/usr/bin/emacs-28.1/do_something"] $
      Just (Command . Common . Internal . Wc $ updAbsPath "/usr/bin/emacs-28.1/do_something"),
    mkTestOneCommand ["wc", ".vimrc", ".vimrc"]
      Nothing,
    mkTestOneCommand ["wc", ".vimrc", "my_game.py"]
      Nothing,
    mkTestOneCommand ["wc", "some_doc.djvu", ".vimrc"]
      Nothing,
  --     pwd
    mkTestOneCommand ["pwd"] $
      Just (Command . Common $ Internal Pwd),
    mkTestOneCommand ["pwd", "."]
      Nothing,
    mkTestOneCommand ["pwd", "F", "1"]
      Nothing,
  --   external
    mkTestOneCommand [updAbsPath' "/bin/vim", "-O", ".vimrc", "my_game.py"] $
      Just (Command . Common . External $ Arguments (updAbsPath "/bin/vim") ["-O", ".vimrc", "my_game.py"]),
    mkTestOneCommand ["vim", ".vimrc", "my_game.py"] $
      Just (Command . Common . External $ Arguments (updAbsPath "/bin/vim") [".vimrc", "my_game.py"]),
    mkTestOneCommand [updPath "././.././user/../../bin/./vim", ".vimrc"] $
      Just (Command . Common . External $ Arguments (updAbsPath "/bin/vim") [".vimrc"]),
    mkTestOneCommand ["my_game.py"]
      Nothing,
    mkTestOneCommand [updPath "./my_game.py"] $
      Just (Command . Common . External $ Arguments (updAbsPath "/home/user/my_game.py") []),
    mkTestOneCommand ["some_doc.djvu"]
      Nothing,
    mkTestOneCommand ["emacs"]
      Nothing,
    mkTestOneCommand ["emacs-28.1"] $
      Just (Command . Common . External $ Arguments (updAbsPath "/usr/bin/emacs-28.1") []),
    mkTestOneCommand [updPath "emacs-28.1/do_something"]
      Nothing,
    mkTestOneCommand [updAbsPath' "/GitHub_PASSWORD.txt"]
      Nothing,
    mkTestOneCommand [updAbsPath' "/bin/qsh"]
      Nothing,
    mkTestOneCommand [updAbsPath' "/bin/sh"] $
      Just (Command . Common . External $ Arguments (updAbsPath "/bin/sh") []),
    mkTestOneCommand ["sh"] $
      Just (Command . Common . External $ Arguments (updAbsPath "/bin/sh") []),
    mkTestOneCommand ["sh", "12321", "", "ava"] $
      Just (Command . Common . External $ Arguments (updAbsPath "/bin/sh") ["12321", "", "ava"]),
    mkTestOneCommand [""]
      Nothing,
    mkTestOneCommand ["", "123", "456"]
      Nothing,
  --   empty
    mkTestOneCommand [] $
      Just EmptyCommand
  ]

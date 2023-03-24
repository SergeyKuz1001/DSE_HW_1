module Phases.Analyzer.Tests (
    testsAnalyzer,
  ) where

import qualified Data.ParsedPrimitive as PP
import Data.AnalyzedPrimitive
import Data.ExitCode (ExitCode(..))
import Data.Variable
import Phases.Analyzer (analyzer)
import Phases.Analyzer.TestEnvironment

import Data.List.NonEmpty (fromList)
import Data.Maybe (fromMaybe)
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

stable' :: String -> Stable
stable' = fromMaybe (error "it isn't Stable") . either (error "it isn't Variable") asStable . variable

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

mkTest :: PP.Primitive -> Maybe Primitive -> Test
mkTest prim expected = TestCase $ (eitherToMaybe . env1) (analyzer prim) @?= expected
  where
    eitherToMaybe (Right x) = Just x
    eitherToMaybe (Left _) = Nothing

mkTestOneCommand :: [String] -> Maybe Primitive -> Test
mkTestOneCommand = mkTest . PP.Commands . pure

mkTestManyCommands :: [[String]] -> Maybe Primitive -> Test
mkTestManyCommands = mkTest . PP.Commands

mkTestAssignment :: String -> String -> Maybe Primitive -> Test
mkTestAssignment var value = mkTest $ PP.Assignment var value

mkTestGrep :: [String] -> GrepArgs -> Test
mkTestGrep args prim = mkTestOneCommand ("grep" : regex prim : args) $ Just $ Commons $ fromList [Internal $ Grep prim]

testsAnalyzer :: Test
testsAnalyzer = TestList [
  -- one command
  --   special
  --     exit
    mkTestOneCommand ["exit"] $
      Just (Special . Exit $ Nothing),
    mkTestOneCommand ["exit", "6"] $
      Just (Special . Exit . Just $ ExitCode 6),
    mkTestOneCommand ["exit", "hahaha"]
      Nothing,
  --     cd
    mkTestOneCommand ["cd"]
      Nothing,
    mkTestOneCommand ["cd", "!@e32riofuhw o3gq23rio q2kj fn2fak.slfaA#WGJ#Q3f   e "] $
      Just (Special $ Cd "!@e32riofuhw o3gq23rio q2kj fn2fak.slfaA#WGJ#Q3f   e "),
    mkTestOneCommand ["cd", "123", "456"]
      Nothing,
  --   internal
  --     cat
    mkTestOneCommand ["cat"] $
      Just (Commons $ fromList [Internal $ Cat Nothing]),
    mkTestOneCommand ["cat", "123"]
      Nothing,
    mkTestOneCommand ["cat", ".vimrc"] $
      Just (Commons $ fromList [Internal . Cat . Just $ updAbsPath "/home/user/.vimrc"]),
    mkTestOneCommand ["cat", "./.vimrc"] $
      Just (Commons $ fromList [Internal . Cat . Just $ updAbsPath "/home/user/.vimrc"]),
    mkTestOneCommand ["cat", "vim"]
      Nothing,
    mkTestOneCommand ["cat", "something_unreadable.txt"]
      Nothing,
    mkTestOneCommand ["cat", updAbsPath' "/GitHub_PASSWORD.txt"]
      Nothing,
    mkTestOneCommand ["cat", updAbsPath' "/some_VERY_VERY_IMPORTANT_file.txt"] $
      Just (Commons $ fromList [Internal . Cat . Just $ updAbsPath "/some_VERY_VERY_IMPORTANT_file.txt"]),
    mkTestOneCommand ["cat", updPath "../../some_VERY_VERY_IMPORTANT_file.txt"] $
      Just (Commons $ fromList [Internal . Cat . Just $ updAbsPath "/some_VERY_VERY_IMPORTANT_file.txt"]),
    mkTestOneCommand ["cat", updPath "Documents/algo04.pdf"] $
      Just (Commons $ fromList [Internal . Cat . Just $ updAbsPath "/home/user/Documents/algo04.pdf"]),
    mkTestOneCommand ["cat", updAbsPath' "/usr/bin/emacs-28.1/do_something"] $
      Just (Commons $ fromList [Internal . Cat . Just $ updAbsPath "/usr/bin/emacs-28.1/do_something"]),
    mkTestOneCommand ["cat", ".vimrc", ".vimrc"]
      Nothing,
    mkTestOneCommand ["cat", ".vimrc", "my_game.py"]
      Nothing,
    mkTestOneCommand ["cat", "some_doc.djvu", ".vimrc"]
      Nothing,
  --     echo
    mkTestOneCommand ["echo"] $
      Just (Commons $ fromList [Internal $ Echo []]),
    mkTestOneCommand ["echo", "bf q43h wui"] $
      Just (Commons $ fromList [Internal $ Echo ["bf q43h wui"]]),
    mkTestOneCommand ["echo", "1", "2"] $
      Just (Commons $ fromList [Internal $ Echo ["1", "2"]]),
  --     wc
    mkTestOneCommand ["wc"] $
      Just (Commons $ fromList [Internal $ Wc Nothing]),
    mkTestOneCommand ["wc", "123"]
      Nothing,
    mkTestOneCommand ["wc", ".vimrc"] $
      Just (Commons $ fromList [Internal . Wc . Just $ updAbsPath "/home/user/.vimrc"]),
    mkTestOneCommand ["wc", "./.vimrc"] $
      Just (Commons $ fromList [Internal . Wc . Just $ updAbsPath "/home/user/.vimrc"]),
    mkTestOneCommand ["wc", "vim"]
      Nothing,
    mkTestOneCommand ["wc", "something_unreadable.txt"]
      Nothing,
    mkTestOneCommand ["wc", updAbsPath' "/GitHub_PASSWORD.txt"]
      Nothing,
    mkTestOneCommand ["wc", updAbsPath' "/some_VERY_VERY_IMPORTANT_file.txt"] $
      Just (Commons $ fromList [Internal . Wc . Just $ updAbsPath "/some_VERY_VERY_IMPORTANT_file.txt"]),
    mkTestOneCommand ["wc", updPath "../../some_VERY_VERY_IMPORTANT_file.txt"] $
      Just (Commons $ fromList [Internal . Wc . Just $ updAbsPath "/some_VERY_VERY_IMPORTANT_file.txt"]),
    mkTestOneCommand ["wc", updPath "Documents/algo04.pdf"] $
      Just (Commons $ fromList [Internal . Wc . Just $ updAbsPath "/home/user/Documents/algo04.pdf"]),
    mkTestOneCommand ["wc", updAbsPath' "/usr/bin/emacs-28.1/do_something"] $
      Just (Commons $ fromList [Internal . Wc . Just $ updAbsPath "/usr/bin/emacs-28.1/do_something"]),
    mkTestOneCommand ["wc", ".vimrc", ".vimrc"]
      Nothing,
    mkTestOneCommand ["wc", ".vimrc", "my_game.py"]
      Nothing,
    mkTestOneCommand ["wc", "some_doc.djvu", ".vimrc"]
      Nothing,
  --     pwd
    mkTestOneCommand ["pwd"] $
      Just (Commons $ fromList [Internal Pwd]),
    mkTestOneCommand ["pwd", "."]
      Nothing,
    mkTestOneCommand ["pwd", "F", "1"]
      Nothing,
  -- grep
    mkTestOneCommand ["grep"] Nothing,
    mkTestOneCommand ["grep", "-A=10", "example"] Nothing,
    mkTestOneCommand ["grep", "-A", "-1", "example"] Nothing,
    mkTestGrep [] $ GrepArgs False False 0 "example" Nothing,
    mkTestGrep ["-w"] $ GrepArgs True False 0 "example" Nothing,
    mkTestGrep ["-w", "-i"] $ GrepArgs True True 0 "example" Nothing,
    mkTestGrep ["-wi"] $ GrepArgs True True 0 "example" Nothing,
    mkTestGrep ["-iw"] $ GrepArgs True True 0 "example" Nothing,
    mkTestGrep ["-w", "--ignorecase"] $ GrepArgs True True 0 "example" Nothing,
    mkTestGrep ["--words", "--ignorecase"] $ GrepArgs True True 0 "example" Nothing,
    mkTestGrep [".vimrc"] $ GrepArgs False False 0 "example" $ Just $ updAbsPath "/home/user/.vimrc",
    mkTestGrep [updAbsPath' "/home/user/.vimrc"] $ GrepArgs False False 0 "example" $ Just $ updAbsPath "/home/user/.vimrc",
    mkTestGrep ["-iw", ".vimrc"] $ GrepArgs True True 0 "example" $ Just $ updAbsPath "/home/user/.vimrc",
    mkTestGrep ["-iw", updAbsPath' "/home/user/.vimrc"] $ GrepArgs True True 0 "example" $ Just $ updAbsPath "/home/user/.vimrc",
    mkTestGrep ["-A", "10"] $ GrepArgs False False 10 "example" Nothing,
    mkTestGrep ["--lines=10"] $ GrepArgs False False 10 "example" Nothing,
    mkTestGrep ["--lines", "10"] $ GrepArgs False False 10 "example" Nothing,
  --   external
    mkTestOneCommand [updAbsPath' "/bin/vim", "-O", ".vimrc", "my_game.py"] $
      Just (Commons $ fromList [External $ Arguments (updAbsPath "/bin/vim") ["-O", ".vimrc", "my_game.py"]]),
    mkTestOneCommand ["vim", ".vimrc", "my_game.py"] $
      Just (Commons $ fromList [External $ Arguments (updAbsPath "/bin/vim") [".vimrc", "my_game.py"]]),
    mkTestOneCommand ["././.././user/../../bin/./vim", ".vimrc"] $
      Just (Commons $ fromList [External $ Arguments (updAbsPath "/bin/vim") [".vimrc"]]),
    mkTestOneCommand ["my_game.py"]
      Nothing,
    mkTestOneCommand ["./my_game.py"] $
      Just (Commons $ fromList [External $ Arguments (updAbsPath "/home/user/my_game.py") []]),
    mkTestOneCommand ["some_doc.djvu"]
      Nothing,
    mkTestOneCommand ["emacs"]
      Nothing,
    mkTestOneCommand ["emacs-28.1"] $
      Just (Commons $ fromList [External $ Arguments (updAbsPath "/usr/bin/emacs-28.1") []]),
    mkTestOneCommand [updPath "emacs-28.1/do_something"]
      Nothing,
    mkTestOneCommand [updAbsPath' "/GitHub_PASSWORD.txt"]
      Nothing,
    mkTestOneCommand [updAbsPath' "/bin/qsh"]
      Nothing,
    mkTestOneCommand [updAbsPath' "/bin/sh"] $
      Just (Commons $ fromList [External $ Arguments (updAbsPath "/bin/sh") []]),
    mkTestOneCommand ["sh"] $
      Just (Commons $ fromList [External $ Arguments (updAbsPath "/bin/sh") []]),
    mkTestOneCommand ["sh", "12321", "", "ava"] $
      Just (Commons $ fromList [External $ Arguments (updAbsPath "/bin/sh") ["12321", "", "ava"]]),
    mkTestOneCommand [""]
      Nothing,
    mkTestOneCommand ["", "123", "456"]
      Nothing,
  --   empty
    mkTestOneCommand [] $
      Just Empty,
  -- many commands
    mkTestManyCommands [["pwd"], ["echo"]] $
      Just (Commons $ fromList [Internal Pwd, Internal $ Echo []]),
    mkTestManyCommands [] $
      Just Empty,
    mkTestManyCommands [[], []]
      Nothing,
    mkTestManyCommands [[], [], [], [], []]
      Nothing,
    mkTestManyCommands [["pwd"], ["exit"]]
      Nothing,
    mkTestManyCommands [["exit"], ["pwd"]]
      Nothing,
    mkTestManyCommands [["vim"], ["emacs-28.1"]] $
      Just (Commons $ fromList [External $ Arguments (updAbsPath "/bin/vim") [], External $ Arguments (updAbsPath "/usr/bin/emacs-28.1") []]),
    mkTestManyCommands [["sh"], ["pwd"], ["vim"]] $
      Just (Commons $ fromList [External $ Arguments (updAbsPath "/bin/sh") [], Internal Pwd, External $ Arguments (updAbsPath "/bin/vim") []]),
    mkTestManyCommands [["./my_game.py", "123", "61"], ["echo"], ["cat", "my_game.py"]] $
      Just (Commons $ fromList [External $ Arguments (updAbsPath "/home/user/my_game.py") ["123", "61"], Internal $ Echo [], Internal . Cat . Just $ updAbsPath "/home/user/my_game.py"]),
    mkTestManyCommands [["echo", "1"], ["echo"], ["echo"]] $
      Just (Commons $ fromList [Internal $ Echo ["1"], Internal $ Echo [], Internal $ Echo []]),
    mkTestManyCommands [["echo", "1"], ["exit"], ["echo"]]
      Nothing,
    mkTestManyCommands [["echo", "1"], ["echo"], ["exit"], ["echo"]]
      Nothing,
    mkTestManyCommands [["echo", "1"], ["echo"], [], ["echo"]]
      Nothing,
    mkTestManyCommands [["echo", "1"], [], ["echo"]]
      Nothing,
  -- assignment
    mkTestAssignment "a1_y5" "123" $
      Just (Assignment (stable' "a1_y5") "123"),
    mkTestAssignment "xyz" "" $
      Just (Assignment (stable' "xyz") ""),
    mkTestAssignment "" "abc"
      Nothing,
    mkTestAssignment "123" "123"
      Nothing,
    mkTestAssignment "Am__3_1V_18a4_l" "asdfghjkl" $
      Just (Assignment (stable' "Am__3_1V_18a4_l") "asdfghjkl"),
    mkTestAssignment "r.p" "q"
      Nothing,
    mkTestAssignment "a=" "b"
      Nothing,
    mkTestAssignment "l4" "3hor2 qo  qw@$Q WK A<V>Fafohql fw\\ae s//* f465 ase + _!@+ 13r#@ rwjk sQFA K3Aw4fad as\"d ;a[] apjwi\'ohf uf aw" $
      Just (Assignment (stable' "l4") "3hor2 qo  qw@$Q WK A<V>Fafohql fw\\ae s//* f465 ase + _!@+ 13r#@ rwjk sQFA K3Aw4fad as\"d ;a[] apjwi\'ohf uf aw")
  ]

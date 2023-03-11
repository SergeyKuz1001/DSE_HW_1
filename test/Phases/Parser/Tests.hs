{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Phases.Parser.Tests (testsParser) where

import           Control.Monad.Except
import           Data.Primitive
import qualified Environment.MonadError as E
import           Phases.Parser
import           Test.HUnit

newtype TestEnvironment a = TestEnvironment {unwrap :: Either E.Error a}
  deriving (Functor, Applicative, Monad, MonadError E.Error, E.MonadError)

mkTest :: String -> String -> Maybe Primitive -> Test
mkTest name arg expected = TestCase . assertEqual name expected . eitherToMaybe . unwrap $ parser arg
  where
    eitherToMaybe (Right x) = Just x
    eitherToMaybe (Left _)  = Nothing

testsParser :: Test
testsParser = TestList [
  mkTest "Empty spaces"                     "  \t  \t "                             $ Just $ Commands [],
  mkTest "Single command"                   "cmd"                                   $ Just $ Commands [["cmd"]],
  mkTest "Single command with spaces"       "   cmd   "                             $ Just $ Commands [["cmd"]],
  mkTest "Other single command"             "qwer"                                  $ Just $ Commands [["qwer"]],
  mkTest "Other single command with spaces" "   qwer   "                            $ Just $ Commands [["qwer"]],
  mkTest "Empty assignment"                 "x="                                    $ Just $ Assignment "x" "",
  mkTest "Empty assignment with spaces"     "   x=   "                              $ Just $ Assignment "x" "",
  mkTest "Assignment"                       "x=1234"                                $ Just $ Assignment "x" "1234",
  mkTest "Assignment with spaces"           "   x=1234   "                          $ Just $ Assignment "x" "1234",
  mkTest "Assignment failure"               "x=1234 cmd"                            $ Nothing,
  mkTest "Command with arguments"           "cmd arg1 arg2 arg3"                    $ Just $ Commands [["cmd", "arg1", "arg2", "arg3"]],
  mkTest "Single quotes"                    "'x='"                                  $ Just $ Commands [["x="]],
  mkTest "Double quotes"                    "\"x=\""                                $ Just $ Commands [["x="]],
  mkTest "Backslash ="                      "x\\="                                  $ Just $ Commands [["x="]],
  mkTest "Quoted command"                   "c'm 'd a\"rg1' ar\"g2' arg3'"          $ Just $ Commands [["cm d", "arg1' arg2 arg3"]],
  mkTest "Quotes and spaces"                " cmd 'arg 1' \"arg 2\" "               $ Just $ Commands [["cmd", "arg 1", "arg 2"]],
  mkTest "Terminated single quote"          "cmd 'arg1"                             $ Nothing,
  mkTest "Terminated double quote"          "cmd \"arg1 \\\""                       $ Nothing,
  mkTest "Backslashes"                      "cmd arg\\ 1 arg\\ 2"                   $ Just $ Commands [["cmd", "arg 1", "arg 2"]],
  mkTest "Backslash space"                  "    \\    "                            $ Just $ Commands [[" "]],
  mkTest "Escaped quotes"                   "cmd \\\"arg1\\\"\\ arg2"               $ Just $ Commands [["cmd", "\"arg1\" arg2"]],
  mkTest "Basic pipes"                      "cmd1|cmd2|cmd3"                        $ Just $ Commands [["cmd1"], ["cmd2"], ["cmd3"]],
  mkTest "Pipes with args"                  "cmd1 arg1|cmd2 arg2|cmd3 arg3"         $ Just $ Commands [["cmd1", "arg1"], ["cmd2", "arg2"], ["cmd3", "arg3"]],
  mkTest "Pipes with spaces"                "  cmd1 | cmd2 | cmd3  "                $ Just $ Commands [["cmd1"], ["cmd2"], ["cmd3"]],
  mkTest "Pipes with args and spaces"       "  cmd1 arg1 | cmd2 arg2 | cmd3 arg3  " $ Just $ Commands [["cmd1", "arg1"], ["cmd2", "arg2"], ["cmd3", "arg3"]],
  mkTest "Empty"                            ""                                      $ Just $ Commands [],
  mkTest "Cmd with empty str in dq as arg"  "cmd a1 \"\" a3"                        $ Just $ Commands [["cmd", "a1", "", "a3"]],
  mkTest "Cmd with empty str in dq as arg"  "cmd a1 \'\' a3"                        $ Just $ Commands [["cmd", "a1", "", "a3"]],
  mkTest "Cmd with double quote as arg"     "cmd a1 \\\" a3"                        $ Just $ Commands [["cmd", "a1", "\"", "a3"]],
  mkTest "Cmd with single quote as arg"     "cmd a1 \\\' a3"                        $ Just $ Commands [["cmd", "a1", "\'", "a3"]],
  mkTest "Empty string in double q. as cmd" "\"\" b aa"                             $ Just $ Commands [["", "b", "aa"]],
  mkTest "Empty string in single q. as cmd" "\'\' b aa"                             $ Just $ Commands [["", "b", "aa"]]
  ]

module Phases.Substitutor.Tests (
    testsSubstitutor
  ) where

import           Phases.Substitutor.TestEnvironment
import           Phases.Substitutor (substitutor)

import           Test.HUnit

mkTest :: String -> String -> Maybe String -> Test
mkTest name arg expected = TestCase . assertEqual name expected . eitherToMaybe . runTestEnvironment $ substitutor arg
  where
    eitherToMaybe (Right x) = Just x
    eitherToMaybe (Left _)  = Nothing

testsSubstitutor :: Test
testsSubstitutor = TestList [
  mkTest "Open single quote"      "asdf asdf ' zxcv zxcv"                       Nothing,
  mkTest "Single quote"           "asdf asdf ' zxcv zxcv '"                   $ Just "asdf asdf ' zxcv zxcv '",
  mkTest "Variable X"             "$mockX"                                    $ Just "value X",
  mkTest "Variable Y"             "$mockY"                                    $ Just "va\\\'lu\\\'eY",
  mkTest "Variable Z"             "$mockZ"                                    $ Just "\\\"v a l u e Z\\\"",
  mkTest "Extra characters"       "$mockXYZ"                                  $ Just "",
  mkTest "Variable {X}"           "${mockX}"                                  $ Just "value X",
  mkTest "Variable {Y}"           "${mockY}"                                  $ Just "va\\\'lu\\\'eY",
  mkTest "Variable {Z}"           "${mockZ}"                                  $ Just "\\\"v a l u e Z\\\"",
  mkTest "Extra {characters}"     "${mockX}YZ"                                $ Just "value XYZ",
  mkTest "\"Variable X\""         "\"$mockX\""                                $ Just "\"value X\"",
  mkTest "\"Variable Y\""         "\"$mockY\""                                $ Just "\"va\\\'lu\\\'eY\"",
  mkTest "\"Variable Z\""         "\"$mockZ\""                                $ Just "\"\\\"v a l u e Z\\\"\"",
  mkTest "\"Extra characters\""   "\"$mockXYZ\""                              $ Just "\"\"",
  mkTest "\"Variable {X}\""       "\"${mockX}\""                              $ Just "\"value X\"",
  mkTest "\"Variable {Y}\""       "\"${mockY}\""                              $ Just "\"va\\\'lu\\\'eY\"",
  mkTest "\"Variable {Z}\""       "\"${mockZ}\""                              $ Just "\"\\\"v a l u e Z\\\"\"",
  mkTest "\"Extra {characters}\"" "\"${mockX}YZ\""                            $ Just "\"value XYZ\"",
  mkTest "Single quoted"          "'$mockX ${mockX} \"$mockX\" \"${mockX}\"'" $ Just "'$mockX ${mockX} \"$mockX\" \"${mockX}\"'",
  mkTest "Single quoted {}"       "'${' '}'"                                  $ Just "'${' '}'",
  mkTest "Unclosed {"             "${asdf"                                      Nothing,
  mkTest "Character before }"     "${asdf }"                                    Nothing,
  mkTest "Quoted }"               "${' '}'"                                     Nothing,
  mkTest "Single in double quote" "\"'$mockX'\""                              $ Just "\"'value X'\"",
  mkTest "Nothing special"        "as\"df as\"df \\ \\ \\\"zxcv\\\" zxcv"     $ Just "as\"df as\"df \\ \\ \\\"zxcv\\\" zxcv"
  ]

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Phases.VarSubstitutor.Tests (testsSubstitutor) where

import           Control.Monad.Except
import           Data.Variable
import qualified Environment           as E
import           Phases.VarSubstitutor
import           Test.HUnit

modError :: String -> E.Error
modError = E.Error "Tests"

throwModError :: E.MonadError m => String -> m a
throwModError = E.throwError . modError

newtype TestEnvironment a = TestEnvironment {unwrap :: Either E.Error a}
  deriving (Functor, Applicative, Monad, MonadError E.Error, E.MonadError)

instance E.MonadPathReader TestEnvironment where
  getVarPath = throwModError "Inaccessible mock method"

instance E.MonadPwdReader TestEnvironment where
  getVarPwd = throwModError "Inaccessible mock method"

instance E.MonadVarReader TestEnvironment where
  getVar var
    | Right var == variable "mockX" = pure "value X"
    | Right var == variable "mockY" = pure "va'lu'eY"
    | Right var == variable "mockZ" = pure "\"v a l u e Z\""
    | otherwise = pure ""
  getVars = throwModError "Inaccessible mock method"

mkTest :: String -> String -> Maybe String -> Test
mkTest name arg expected = TestCase . assertEqual name expected . eitherToMaybe . unwrap $ varSubstitutor arg
  where
    eitherToMaybe (Right x) = Just x
    eitherToMaybe (Left _)  = Nothing

testsSubstitutor :: Test
testsSubstitutor = TestList [
  mkTest "Open single quote"      "asdf asdf ' zxcv zxcv"                     $ Nothing,
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
  mkTest "Unclosed {"             "${asdf"                                    $ Nothing,
  mkTest "Character before }"     "${asdf }"                                  $ Nothing,
  mkTest "Quoted }"               "${' '}'"                                   $ Nothing,
  mkTest "Nothing special"    "as\"df as\"df \\ \\ \\\"zxcv\\\" zxcv" $ Just "as\"df as\"df \\ \\ \\\"zxcv\\\" zxcv"
  ]

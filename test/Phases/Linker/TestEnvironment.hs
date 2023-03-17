{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Phases.Linker.TestEnvironment (
    module Data.FSObjects,
    module Monads.FS,
    module Monads.SelfReferenced,
    TestEnvironment,
    runTestEnvironment,
  ) where

import Data.FSObjects
import Monads.FS
import Monads.SelfReferenced

import Data.Functor.Identity
import System.FilePath (pathSeparator)

newtype TestEnvironment a = TestEnvironment (Identity a)
  deriving (Functor, Applicative, Monad)

instance MonadFS TestEnvironment where
  findFileByAbsPath = undefined

replace :: Eq a => a -> a -> [a] -> [a]
replace x y = foldr (\z -> if x == z then (y:) else (z:)) []

updAbsPath :: FilePath -> AbsFilePath
updAbsPath path = either (error "it isn't AbsFilePath") id . absFilePath $
  if pathSeparator == '/'
    then path
    else "C:" ++ replace '/' '\\' path

instance MonadSelfReferenced TestEnvironment where
  getSelfPath = TestEnvironment . Identity $ updAbsPath "/home/user/cli-exe"

runTestEnvironment :: TestEnvironment a -> a
runTestEnvironment (TestEnvironment m) = runIdentity m

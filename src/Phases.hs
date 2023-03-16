{- |
В данном модуле объявлены все фазы работы данной программы.
-}
module Phases (
    analyzer,
    executor,
    linker,
    parser,
    stringReader,
    varSubstitutor,
  ) where

import Phases.Analyzer
import Phases.Executor
import Phases.Linker
import Phases.Parser
import Phases.StringReader
import Phases.VarSubstitutor

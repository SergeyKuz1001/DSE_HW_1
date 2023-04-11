import           Phases.Analyzer.Tests       (testsAnalyzer)
import           Phases.Executor.Tests       (testsExecutor)
import           Phases.Linker.Tests         (testsLinker)
import           Phases.Parser.Tests         (testsParser)
import           Phases.Substitutor.Tests    (testsSubstitutor)
import           System.Environment

import           Test.HUnit

main :: IO ()
main = getArgs >>= runTestTTAndExit . tests

tests :: [String] -> Test
tests names = TestList $ map (uncurry TestLabel) $ filter (\(x, _) -> null names || x `elem` names) [
    ( "Analyzer"    , testsAnalyzer    ),
    ( "Substitutor" , testsSubstitutor ),
    ( "Parser"      , testsParser      ),
    ( "Executor"    , testsExecutor    ),
    ( "Linker"      , testsLinker      )
  ]

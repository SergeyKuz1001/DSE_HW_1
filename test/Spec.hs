import           Phases.Analyzer.Tests (testsAnalyzer)
import           Phases.Parser.Tests   (testsParser)

import           Test.HUnit

main :: IO ()
main = runTestTTAndExit $ TestList [
    TestLabel "Analyzer" testsAnalyzer,
    TestLabel "Parser" testsParser
  ]

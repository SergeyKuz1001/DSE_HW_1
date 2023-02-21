import Phases.Analyzer.Tests (testsAnalyzer)

import Test.HUnit

main :: IO ()
main = runTestTTAndExit $ TestList [
    TestLabel "Analyzer" testsAnalyzer
  ]

module RuntimeSuite (tests) where

import Data.List (nub)
import Monk.Tooling.Runtime.Suite
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "runtime suite registry"
    [ testCase "all legacy names round trip uniquely" $ do
        let suites = [minBound .. maxBound]
        map (parseSuite . suiteName) suites @?= map Right suites
        length (nub (map suiteName suites)) @?= length suites,
      testCase "native inventory includes transport and excludes the separate digest checker" $ do
        assertBool "transport missing" (ChildTransport `elem` allSuites)
        assertBool "digest selected for native runtime" (Digest `notElem` allSuites)
        assertBool "receipt suite missing" (all (`elem` allSuites) requiredReceiptSuites),
      testCase "translator-dependent suites require the translator" $
        filter suiteNeedsTranslator allSuites @?= [CallbackDiagnostics, DirectOutput, DirectorySignals, NativeLauncher]
    ]

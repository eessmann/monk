{-# LANGUAGE OverloadedStrings #-}

module Unit.SemanticAdmission
  ( unitSemanticAdmissionTests,
  )
where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Monk.Translation
  ( Diagnostic (..),
    DiagnosticCode (..),
    DiagnosticPhase (..),
    TranslationFailure (..),
    renderTranslation,
    strictConfig,
    translateBashScript,
  )
import ShellSupport
  ( RunResult (..),
    Shell (..),
    prepareEnv,
    runShell,
    shouldRunIntegration,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as H

unitSemanticAdmissionTests :: TestTree
unitSemanticAdmissionTests =
  testGroup
    "Semantic admission"
    [ testGroup "exact" (map exactFixtureTest exactFixtures),
      testGroup "rejected" (map rejectedFixtureTest rejectedFixtures),
      testGroup "positive controls" (map exactFixtureTest positiveFixtures)
    ]

data SemanticFixture = MkSemanticFixture
  { sfName :: String,
    sfPath :: FilePath
  }

exactFixtures :: [SemanticFixture]
exactFixtures =
  [ MkSemanticFixture "untaken option branch" "test/fixtures/semantic/untaken-option.bash",
    MkSemanticFixture "uncalled option function" "test/fixtures/semantic/uncalled-option-function.bash",
    MkSemanticFixture "multi-character IFS" "test/fixtures/semantic/ifs-set.bash",
    MkSemanticFixture "embedded quoted argv" "test/fixtures/semantic/quoted-argv-adjacent.bash",
    MkSemanticFixture "lazy case pattern" "test/fixtures/semantic/case-unreached-effect.bash",
    MkSemanticFixture "case fallthrough" "test/fixtures/semantic/case-fallthrough.bash",
    MkSemanticFixture "integer intermediate division" "test/fixtures/semantic/arithmetic-integral-intermediate.bash",
    MkSemanticFixture "dynamic caller local" "test/fixtures/semantic/dynamic-local.bash",
    MkSemanticFixture "constant dynamic command" "test/fixtures/semantic/dynamic-command.bash"
  ]

rejectedFixtures :: [SemanticFixture]
rejectedFixtures =
  [ MkSemanticFixture "sparse mixed array" "test/fixtures/semantic/array-mixed.bash",
    MkSemanticFixture "eval" "test/fixtures/semantic/eval-bash-syntax.bash"
  ]

positiveFixtures :: [SemanticFixture]
positiveFixtures =
  [ MkSemanticFixture "simple command" "test/fixtures/semantic/positive-simple-command.bash",
    MkSemanticFixture "control flow" "test/fixtures/semantic/positive-control-flow.bash",
    MkSemanticFixture "function call" "test/fixtures/semantic/positive-function-call.bash"
  ]

exactFixtureTest :: SemanticFixture -> TestTree
exactFixtureTest MkSemanticFixture {sfName, sfPath} = H.testCaseSteps sfName $ \step -> do
  runnable <- shouldRunIntegration
  case runnable of
    Left reason -> step ("skipped: " <> reason)
    Right () -> do
      bashSrc <- TIO.readFile sfPath
      translated <- translateBashScript strictConfig sfPath bashSrc
      case translated of
        Left failure -> H.assertFailure ("strict translation failed: " <> show failure)
        Right translation -> do
          env <- prepareEnv
          bashResult <- runShell ShellBash env bashSrc
          fishResult <- runShell ShellFish env (renderTranslation translation)
          assertEquivalent sfName bashResult fishResult

rejectedFixtureTest :: SemanticFixture -> TestTree
rejectedFixtureTest MkSemanticFixture {sfName, sfPath} = H.testCase sfName $ do
  bashSrc <- TIO.readFile sfPath
  translated <- translateBashScript strictConfig sfPath bashSrc
  case translated of
    Left (MkTranslationFailure diagnostics) ->
      H.assertBool
        "expected a located semantic rejection, not a parse/infrastructure failure"
        ( any
            ( \diagnostic ->
                diagnosticPhase diagnostic == PhaseTranslate
                  && "monk.semantic." `T.isPrefixOf` diagnosticCodeText (diagnosticCode diagnostic)
                  && isJust (diagnosticRange diagnostic)
            )
            diagnostics
        )
    Right translation ->
      H.assertFailure
        ( "strict translation accepted a rejected semantic case:\n"
            <> toString (renderTranslation translation)
        )

assertEquivalent :: String -> RunResult -> RunResult -> H.Assertion
assertEquivalent fixtureName bashResult fishResult = do
  H.assertEqual (fixtureName <> ": exit status") (rrExit bashResult) (rrExit fishResult)
  H.assertEqual (fixtureName <> ": stdout") (rrStdout bashResult) (rrStdout fishResult)
  H.assertEqual (fixtureName <> ": stderr") (rrStderr bashResult) (rrStderr fishResult)

{-# LANGUAGE OverloadedStrings #-}

module Unit.PlannedFlowFacts
  ( unitPlannedFlowFactTests,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Monk.Source
  ( SourceEnvironment (..),
    SourceGraph,
    SourceGraphFailure (..),
    translateSourceGraphWithEnvironment,
  )
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
import SourceTestSupport (renderGraph, withSources)
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (cwd, env), proc, readCreateProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as H

unitPlannedFlowFactTests :: TestTree
unitPlannedFlowFactTests =
  testGroup
    "Planned function and source flow facts"
    [ rejectedSourceCase
        "a function definition after a possible source return is not definite"
        ". ./dependency.bash stop\necho -- -n\n"
        "if test \"$1\" = stop; then return 0; fi\necho() { command printf 'custom\\n'; }\n"
        (MkDiagnosticCode "monk.semantic.call-binding"),
      rejectedSourceCase
        "a constant command head after a possible source return is not definite"
        ". ./dependency.bash stop\n$cmd -- -n\n"
        "if test \"$1\" = stop; then return 0; fi\ncmd=echo\n"
        (MkDiagnosticCode "monk.semantic.dynamic-command"),
      rejectedSourceCase
        "a command head after a possible case-arm source return is not definite"
        ". ./dependency.bash stop\n$cmd -- -n\n"
        "case \"$1\" in stop) return 0 ;; *) true ;; esac\ncmd=echo\n"
        (MkDiagnosticCode "monk.semantic.dynamic-command"),
      rejectedSourceCase
        "a later sourced function invalidates an earlier implicit external dispatch"
        "a() { f; }\n. ./dependency.bash\na\n"
        "f() { printf 'function\\n'; }\n"
        (MkDiagnosticCode "monk.semantic.call-binding-context"),
      exactSourceCase
        "updates before a possible source return remain visible"
        "value=outer\n. ./dependency.bash stop\nsource_status=$?\nprintf '%s:%s\\n' \"$source_status\" \"$value\"\n"
        "value=before\nif test \"$1\" = stop; then return 7; fi\nvalue=after\n"
        "7:before\n",
      exactSourceCase
        "functions defined before a possible source return remain callable"
        ". ./dependency.bash stop\nsource_status=$?\ngreet\nprintf 'source:%s\\n' \"$source_status\"\n"
        "greet() { printf 'hello\\n'; }\nif test \"$1\" = stop; then return 9; fi\n"
        "hello\nsource:9\n",
      exactSourceCase
        "explicit command dispatch bypasses a later sourced function"
        "a() { command echo -- -n; }\n. ./dependency.bash\na\n"
        "echo() { printf 'custom\\n'; }\n"
        "-- -n\n",
      exactScriptCase
        "local declaration right sides observe the pre-command scope"
        "x=outer; f() { local x=inner y=\"$x\"; printf '%s:%s\\n' \"$x\" \"$y\"; }; f"
        "inner:outer\n",
      rejectedScriptCase
        "an earlier case arm invalidates a numeric fact retained by the final arm"
        "x=1; case a in a) x=oops ;; b) true ;; esac; printf '%s\\n' \"$((x))\""
        (MkDiagnosticCode "monk.semantic.arithmetic-binding"),
      rejectedScriptCase
        "an unproven runtime exit status is rejected"
        "exit \"$MONK_EXIT_STATUS\""
        (MkDiagnosticCode "monk.semantic.status-argument")
    ]

rejectedSourceCase :: String -> Text -> Text -> DiagnosticCode -> TestTree
rejectedSourceCase name root dependency expectedCode = H.testCase name $
  withSources root [("dependency.bash", dependency)] $ \rootPath environment -> do
    result <- translateSourceGraphWithEnvironment strictConfig environment True rootPath
    case result of
      Left (MkSourceGraphFailure _ failure) -> do
        let diagnostic = NonEmpty.head (failureDiagnostics failure)
        diagnosticCode diagnostic H.@?= expectedCode
        diagnosticPhase diagnostic H.@?= PhaseTranslate
        H.assertBool "semantic rejection lost its source range" (isJust (diagnosticRange diagnostic))
      Right _ -> H.assertFailure "uncertain post-source dispatch produced an output graph"

rejectedScriptCase :: String -> Text -> DiagnosticCode -> TestTree
rejectedScriptCase name source expectedCode = H.testCase name $ do
  result <- translateBashScript strictConfig "planned-flow-facts.bash" source
  case result of
    Left failure -> do
      let diagnostic = NonEmpty.head (failureDiagnostics failure)
      diagnosticCode diagnostic H.@?= expectedCode
      diagnosticPhase diagnostic H.@?= PhaseTranslate
      H.assertBool "semantic rejection lost its source range" (isJust (diagnosticRange diagnostic))
    Right translation ->
      H.assertFailure
        ( "unsupported flow produced executable output:\n"
            <> toString (renderTranslation translation)
        )

exactSourceCase :: String -> Text -> Text -> Text -> TestTree
exactSourceCase name root dependency expectedOutput = H.testCaseSteps name $ \step -> do
  readiness <- shouldRunIntegration
  case readiness of
    Left reason -> step ("skipped: " <> reason)
    Right () ->
      withSources root [("dependency.bash", dependency)] $ \rootPath environment -> do
        graphResult <- translateSourceGraphWithEnvironment strictConfig environment True rootPath
        graph <- requireGraph graphResult
        generated <- renderGraph graph
        shellEnvironment <- prepareEnv
        let run command arguments =
              readCreateProcessWithExitCode
                (proc command arguments) {cwd = Just (sourceWorkingDirectory environment), env = Just shellEnvironment}
                ""
        bash <- run "bash" ["--noprofile", "--norc", rootPath]
        fish <- run "fish" ["--no-config", "-c", toString generated]
        H.assertEqual "independent Bash stdout" (ExitSuccess, toString expectedOutput, "") bash
        H.assertEqual "exit status, stdout, and stderr" bash fish

exactScriptCase :: String -> Text -> Text -> TestTree
exactScriptCase name source expectedOutput = H.testCaseSteps name $ \step -> do
  readiness <- shouldRunIntegration
  case readiness of
    Left reason -> step ("skipped: " <> reason)
    Right () -> do
      result <- translateBashScript strictConfig "planned-flow-facts.bash" source
      case result of
        Left failure -> H.assertFailure ("exact flow case rejected: " <> show failure)
        Right translation -> do
          shellEnvironment <- prepareEnv
          bash <- runShell ShellBash shellEnvironment source
          fish <- runShell ShellFish shellEnvironment (renderTranslation translation)
          H.assertEqual "independent Bash stdout" expectedOutput (rrStdout bash)
          H.assertEqual "exit status" (rrExit bash) (rrExit fish)
          H.assertEqual "stdout" (rrStdout bash) (rrStdout fish)
          H.assertEqual "stderr" (rrStderr bash) (rrStderr fish)

requireGraph :: Either SourceGraphFailure SourceGraph -> IO SourceGraph
requireGraph = \case
  Left failure -> H.assertFailure ("exact source graph rejected: " <> show failure) >> fail "unreachable"
  Right graph -> pure graph

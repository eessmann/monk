module Unit.PlannedSourceable (plannedSourceableTests) where

import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Monk.Translation
import Path (toFilePath)
import Path.IO qualified as PathIO
import ShellSupport (prepareEnv, shouldRunIntegration)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.Process (CreateProcess (env), proc, readCreateProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as H

plannedSourceableTests :: TestTree
plannedSourceableTests =
  testGroup
    "Planned sourceable execution"
    [ H.testCase "optional output guard rendering grows linearly with bindings" $ do
        sizes <- forM [4, 8] $ \count -> do
          let names = ["v" <> show index | index <- [1 .. count :: Int]]
              contract = emptyContract {callerVariables = M.fromList [(name, ScalarBinding OutputBinding GlobalBinding UnexportedBinding) | name <- names]}
              source = T.intercalate "; " [name <> "=value" | name <- names]
          result <- translateBashScript (config contract) "guard-growth.bash" source
          either (\failure -> H.assertFailure (show failure) >> pure 0) (pure . T.length . renderTranslation) result
        case sizes of
          [small, large] -> H.assertBool ("rendered guard sizes: " <> show sizes) (small > 0 && large < 3 * small)
          _ -> H.assertFailure "missing guard size sample",
      exact
        "visible caller local and unrelated global survive"
        visible
        "x=changed; printf '%s\\n' \"$x\""
        "x=global; unrelated=kept; caller() { local x=local; . \"$1\"; printf '%s\\n' \"$x\"; }; caller \"$1\"; printf '%s:%s\\n' \"$x\" \"$unrelated\""
        "set -g x global; set -g unrelated kept; function caller; set -l x local; source \"$argv[1]\"; builtin printf '%s\\n' \"$x\"; end; caller \"$argv[1]\"; builtin printf '%s:%s\\n' \"$x\" \"$unrelated\"",
      exact
        "unexported global output can be created"
        output
        "x=created"
        ". \"$1\"; printf '%s\\n' \"$x\""
        "source \"$argv[1]\"; builtin printf '%s\\n' \"$x\"; builtin set --query --global --unexport x",
      exact
        "early return preserves source status and arguments"
        emptyContract
        "printf '%s:%s\\n' \"$1\" \"$2\"; return 7; printf unreachable"
        ". \"$1\" alpha beta; printf 'status:%s\\n' \"$?\""
        "source \"$argv[1]\" alpha beta; builtin printf 'status:%s\\n' \"$status\"",
      exact
        "declared function remains callable after source returns"
        exported
        "greet() { printf '%s:%s\\n' \"$1\" \"$?\"; }; return 7"
        ". \"$1\"; printf 'source:%s\\n' \"$?\"; true; greet hello"
        "source \"$argv[1]\"; builtin printf 'source:%s\\n' \"$status\"; builtin true; greet hello",
      exact
        "installed arithmetic function owns helpers on later calls"
        exported
        "greet() { printf '%s\\n' \"$((5/2))\"; }"
        ". \"$1\"; greet; greet"
        "source \"$argv[1]\"; greet; greet",
      exact
        "source observes incoming caller status"
        emptyContract
        "printf '%s\\n' \"$?\""
        "false; . \"$1\""
        "builtin false; source \"$argv[1]\"",
      exact
        "declared host function updates caller state"
        imported
        "visit; printf '%s\\n' \"$x\""
        "x=original; visit() { x=host; }; . \"$1\"; printf '%s\\n' \"$x\""
        "set -g x original; function host_visit --no-scope-shadowing; set x host; end; source \"$argv[1]\"; builtin printf '%s\\n' \"$x\"",
      exact
        "repeat entry leaves no private helpers"
        emptyContract
        "printf '%s\\n' \"$((5/2))\""
        ". \"$1\"; . \"$1\""
        "source \"$argv[1]\"; source \"$argv[1]\"",
      rejected "unknown ambient effects do not authorize sourceable execution" emptyCallerContract "printf hi",
      rejected "undeclared caller for binding rejects" emptyContract "for x in value; do :; done",
      rejected "undeclared caller mutation rejects" emptyContract "x=changed",
      rejected "undeclared installed function rejects" emptyContract "greet() { true; }",
      rejected "caller shell option changes require persistent semantics" emptyContract "set -e",
      rejected "exported function option effects require persistent semantics" exported "greet() { set -e; }",
      guardCase "visible output cannot disappear into a temporary frame" visibleOutput "x=changed" "" "",
      guardCase "scalar cardinality guard runs before source effects" visible "printf effect; x=changed" "set -g x one two" "one two",
      guardCase "export mismatch runs before source effects" visible "printf effect; x=changed" "set -gx x original" "original"
    ]
  where
    emptyContract = emptyCallerContract {callerAmbientEffects = NoRelevantAmbientEffects}
    visible = emptyContract {callerVariables = M.singleton "x" (ScalarBinding InputOutputBinding VisibleBinding UnexportedBinding)}
    imported = visible {callerFunctions = M.singleton "visit" (MkFunctionContract "host_visit" (S.singleton "x") (S.singleton "x"))}
    visibleOutput = emptyContract {callerVariables = M.singleton "x" (ScalarBinding OutputBinding VisibleBinding UnexportedBinding)}
    output = emptyContract {callerVariables = M.singleton "x" (ScalarBinding OutputBinding GlobalBinding UnexportedBinding)}
    exported = emptyContract {callerExportedFunctions = S.singleton "greet"}

config :: CallerContract -> TranslateConfig
config contract = strictConfig {entryMode = Sourceable, callerContract = contract}

exact :: String -> CallerContract -> Text -> Text -> Text -> TestTree
exact name contract source bashCaller fishCaller = H.testCaseSteps name $ \step -> do
  readiness <- shouldRunIntegration
  case readiness of
    Left reason -> step ("skipped: " <> reason)
    Right () -> PathIO.withSystemTempDir "monk-sourceable" $ \temporary -> do
      let directory = toFilePath temporary
      result <- translateBashScript (config contract) "sourceable.bash" source
      translation <- either (\failure -> H.assertFailure (show failure) >> error "unreachable") pure result
      let bashPath = directory </> "input.bash"
          fishPath = directory </> "input.fish"
      TIO.writeFile bashPath source
      TIO.writeFile fishPath (renderTranslation translation)
      environment <- prepareEnv
      bash <- readCreateProcessWithExitCode ((proc "bash" ["--noprofile", "--norc", "-c", toString bashCaller, "caller.bash", bashPath]) {env = Just environment}) ""
      fish <- readCreateProcessWithExitCode ((proc "fish" ["--no-config", "-c", toString fishCaller, fishPath]) {env = Just environment}) ""
      H.assertEqual "stdout/stderr/status" bash fish

rejected :: String -> CallerContract -> Text -> TestTree
rejected name contract source = H.testCase name $ do
  result <- translateBashScript (config contract) "sourceable.bash" source
  case result of
    Left _ -> pure ()
    Right _ -> H.assertFailure "unsupported caller effects admitted"

guardCase :: String -> CallerContract -> Text -> Text -> Text -> TestTree
guardCase name contract source setup expected = H.testCaseSteps name $ \step -> do
  readiness <- shouldRunIntegration
  case readiness of
    Left reason -> step ("skipped: " <> reason)
    Right () -> PathIO.withSystemTempDir "monk-sourceable-guard" $ \temporary -> do
      let directory = toFilePath temporary
      result <- translateBashScript (config contract) "sourceable.bash" source
      translation <- either (\failure -> H.assertFailure (show failure) >> error "unreachable") pure result
      let path = directory </> "input.fish"
          caller = setup <> "\nfunction caller; source \"$argv[1]\"; builtin printf '%s:%s\\n' \"$status\" \"$x\"; end; caller \"$argv[1]\""
      TIO.writeFile path (renderTranslation translation)
      environment <- prepareEnv
      (status, output, errors) <- readCreateProcessWithExitCode ((proc "fish" ["--no-config", "-c", toString caller, path]) {env = Just environment}) ""
      H.assertEqual "caller continues" ExitSuccess status
      H.assertEqual "pre-effect rejection and preserved value" ("125:" <> toString expected <> "\n") output
      H.assertBool "missing contract diagnostic" (not (null errors))

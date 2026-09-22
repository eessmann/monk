module Unit.PlannedTraps (unitPlannedTrapTests) where

import Data.Text.IO qualified as TIO
import Monk.Translation
import ShellSupport (prepareEnv, shouldRunIntegration)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (cwd, env), proc, readCreateProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as H

unitPlannedTrapTests :: TestTree
unitPlannedTrapTests =
  testGroup
    "Deferred owned traps"
    [ exact "EXIT observes fallthrough status" "trap 'printf exit:%s \"$?\"' EXIT; false" "exit:1",
      exact "EXIT sees live variables after registration" "value=before; trap 'printf %s \"$value\"' EXIT; value=after" "after",
      exact "trap registration establishes status zero" "false; trap 'printf :exit' EXIT; printf '%s' \"$?\"" "0:exit",
      exact "EXIT replacement runs only latest body" "trap 'printf old' EXIT; trap 'printf new' EXIT" "new",
      exact "EXIT reset removes callback" "trap 'printf bad' EXIT; trap - EXIT; printf good" "good",
      exact "empty EXIT handler preserves failure" "trap '' EXIT; false" "",
      exact "EXIT handler failure preserves incoming success" "trap 'false' EXIT; true" "",
      exact "explicit handler exit overrides final status" "trap 'printf handler; exit 7' EXIT; true" "handler",
      exact "EXIT retains descriptors from exiting compound" "trap 'printf trap' EXIT; { exit; } >output" "",
      exact "compound ERR writes within its redirection" "trap 'printf trap' ERR; { false; } >output; printf outside; cat output" "outsidetrap",
      exact "simple ERR runs after redirection unwinds" "trap 'printf trap' ERR; false >output; printf outside; cat output" "trapoutside",
      exact "ERR executes before normal continuation" "trap 'printf err:%s: \"$?\"' ERR; false; printf after" "err:1:after",
      exact "ERR is suppressed in tested condition" "trap 'printf bad' ERR; if false; then printf bad; fi; printf good" "good",
      exact "ERR is suppressed on tested and-or operand" "trap 'printf bad' ERR; false && printf bad; printf good" "good",
      exact "ERR precedes EXIT under errexit" "trap 'printf err:' ERR; trap 'printf exit:%s \"$?\"' EXIT; set -e; false" "err:exit:1",
      exact "child does not inherit EXIT callback" "trap 'printf :exit' EXIT; (printf child); printf :parent" "child:parent:exit",
      exact "substitution does not inherit callbacks" "trap 'printf :exit' EXIT; value=\"$(printf child)\"; printf %s \"$value\"" "child:exit",
      exact "ERR is not inherited by function body" "trap 'printf bad' ERR; f() { false; printf good; }; f" "good",
      exact "function failure triggers caller ERR once" "trap 'printf err' ERR; f() { false; }; f" "err",
      exact "ERR handler writes are visible afterward" "value=before; trap 'value=after' ERR; false; printf %s \"$value\"" "after",
      exact "handler can replace EXIT while running ERR" "trap 'printf old' EXIT; trap \"trap 'printf new' EXIT\" ERR; false" "new",
      exact "EXIT sees an owned background PID assigned before callback" "false & pid=$!; trap 'if test -n \"$pid\"; then printf live; fi' EXIT; wait \"$pid\"" "live",
      rejected "unknown handler source stays rejected" "trap \"$1\" EXIT",
      rejected "signal traps outside EXIT and ERR stay rejected" "trap 'printf bad' INT",
      rejected "handler return stays rejected" "f() { trap 'return' EXIT; }; f",
      rejected "handler external lookup stays rejected" "trap 'rm temporary' EXIT",
      rejected "handler function lookup stays rejected" "f() { true; }; trap 'f' EXIT",
      rejected "ERR writes invalidate proven eval facts" "code='printf before'; trap 'code=unknown' ERR; false; eval \"$code\"",
      rejected "handler numeric reads cannot freeze registration facts" "n=1; trap 'printf %s \"$((n+1))\"' EXIT; n=unknown"
    ]

exact :: String -> Text -> Text -> TestTree
exact name source output = H.testCaseSteps name $ \step -> do
  translated <- translateBashScript strictConfig "trap.bash" source
  case translated of
    Left failure -> H.assertFailure (show failure)
    Right translation -> do
      readiness <- shouldRunIntegration
      case readiness of
        Left reason -> step ("skipped: " <> reason)
        Right () -> withSystemTempDirectory "monk-trap" $ \directory -> do
          let bashPath = directory </> "input.bash"
              fishPath = directory </> "input.fish"
          TIO.writeFile bashPath source
          TIO.writeFile fishPath (renderTranslation translation)
          environment <- prepareEnv
          bash <- readCreateProcessWithExitCode ((proc "bash" ["--noprofile", "--norc", bashPath]) {env = Just environment, cwd = Just directory}) ""
          fish <- readCreateProcessWithExitCode ((proc "fish" ["--no-config", fishPath]) {env = Just environment, cwd = Just directory}) ""
          let (_, actual, _) = bash
          H.assertEqual "independent expected stdout" (toString output) actual
          H.assertEqual "stdout/stderr/status" bash fish

rejected :: String -> Text -> TestTree
rejected name source = H.testCase name $ do
  result <- translateBashScript strictConfig "trap-rejected.bash" source
  H.assertBool "unproved trap was admitted" (isLeft result)

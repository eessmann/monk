module Unit.PlannedEval (unitPlannedEvalTests) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Monk.Translation
import ShellSupport (prepareEnv, shouldRunIntegration)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (env), proc, readCreateProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as H

unitPlannedEvalTests :: TestTree
unitPlannedEvalTests =
  testGroup
    "Compiled finite eval"
    [ exact "literal eval establishes later scalar facts" "eval 'x=ok'; printf '%s' \"$x\"" "ok",
      exact "proven scalar eval is compiled once" "program='x=one'; eval \"$program\"; printf '%s' \"$x\"" "one",
      exact "eval joins its arguments with spaces" "eval 'x=\"a' 'b\"'; printf '<%s>' \"$x\"" "<a b>",
      exact "empty eval resets incoming failure status" "false; eval ''; printf '%s' \"$?\"" "0",
      exact "operandless eval resets incoming failure status" "false; eval; printf '%s' \"$?\"" "0",
      exact "eval observes its incoming status" "false; eval 'printf \"%s\" \"$?\"'" "1",
      exact "explicit builtin eval remains compile-time" "builtin eval -- 'x=ok'; printf '%s' \"$x\"" "ok",
      exact "eval body sees live scalar values" "x=before; eval 'printf \"%s\" \"$x\"'; x=after; eval 'printf \"%s\" \"$x\"'" "beforeafter",
      exact "nested finite eval remains compiled" "eval \"eval 'x=ok'\"; printf '%s' \"$x\"" "ok",
      exact "eval returns from its owning function" "f() { eval 'return 7'; printf bad; }; f; printf '%s' \"$?\"" "7",
      exact "eval local belongs to its owning function" "x=outer; f() { eval 'local x=inner'; printf '%s:' \"$x\"; }; f; printf '%s' \"$x\"" "inner:outer",
      exact "eval compiled conditional preserves control and status" "eval 'if false; then printf bad; else printf good; fi'; printf ':%s' \"$?\"" "good:0",
      exact "eval defines a function for subsequent dispatch" "eval 'f() { printf defined; }'; f" "defined",
      exact "eval preserves successful constant arithmetic" "eval 'x=$((2+3))'; printf '%s' \"$x\"" "5",
      rejected "unknown eval source stays rejected" "eval \"$1\"",
      rejected "effectful eval operands stay rejected" "n=1; eval \"$((n++))\"",
      rejected "recursive proven eval stays rejected" "code='eval \"$code\"'; eval \"$code\"",
      rejected "eval redefinition invalidates earlier function dependency" "eval 'f() { printf one; }'; g() { f; }; eval 'f() { printf two; }'; g",
      rejected "invalid finite eval is rejected during translation" "eval 'if'",
      rejected "potential eval arithmetic error needs mapped diagnostics" "eval '((1/0))'",
      rejected "eval substitution warning needs mapped diagnostics" "eval 'value=$(echo -e \"a\\0b\")'",
      rejected "eval wait errors need mapped diagnostics" "eval 'wait 999999'"
    ]

exact :: String -> Text -> Text -> TestTree
exact name source output = H.testCaseSteps name $ \step -> do
  translated <- translateBashScript strictConfig "finite-eval.bash" source
  case translated of
    Left failure -> H.assertFailure (show failure)
    Right translation -> do
      H.assertBool "runtime eval survived compilation" (not (T.isInfixOf "builtin eval " (renderTranslation translation)))
      readiness <- shouldRunIntegration
      case readiness of
        Left reason -> step ("skipped: " <> reason)
        Right () -> withSystemTempDirectory "monk-finite-eval" $ \directory -> do
          let bashPath = directory </> "input.bash"
              fishPath = directory </> "input.fish"
          TIO.writeFile bashPath source
          TIO.writeFile fishPath (renderTranslation translation)
          environment <- prepareEnv
          bash <- readCreateProcessWithExitCode ((proc "bash" ["--noprofile", "--norc", bashPath]) {env = Just environment}) ""
          fish <- readCreateProcessWithExitCode ((proc "fish" ["--no-config", fishPath]) {env = Just environment}) ""
          let (_, actual, _) = bash
          H.assertEqual "independent expected stdout" (toString output) actual
          H.assertEqual "stdout/stderr/status" bash fish

rejected :: String -> Text -> TestTree
rejected name source = H.testCase name $ do
  result <- translateBashScript strictConfig "finite-eval-rejected.bash" source
  H.assertBool "unproved eval was admitted" (isLeft result)

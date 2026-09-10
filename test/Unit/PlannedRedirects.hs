module Unit.PlannedRedirects (unitPlannedRedirectTests) where

import Data.Text qualified as T
import Monk.Translation
import ShellSupport
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as H

unitPlannedRedirectTests :: TestTree
unitPlannedRedirectTests =
  testGroup
    "Planned ordered descriptors"
    [ exact "stdout duplicates the current stderr descriptor" "printf 'value\\n' >&2",
      exact "duplication before null output retains the original stdout" "sh -c 'printf out; printf err >&2' 2>&1 >/dev/null",
      exact "duplication after null output inherits null output" "sh -c 'printf out; printf err >&2' >/dev/null 2>&1",
      exact "null input reaches the external command" "cat </dev/null; printf 'done\\n'",
      exact "shared compound bindings survive null output" "x=before; { x=after; printf body; } >/dev/null; printf '%s\\n' \"$x\"",
      exact "function output redirect executes only when called" "f() { printf 'called\\n'; } >&2; printf 'before\\n'; f; printf 'after\\n'",
      exact "function redirect sees descriptors at invocation" "f() { sh -c 'printf err >&2'; } 2>&1; f >/dev/null; printf 'after\\n'",
      exact "uncalled function redirects do not affect definition status" "false; f() { printf body; } >/dev/null; printf 'status:%s\\n' \"$?\"",
      rejected "shared file opens require owned failure and descriptor semantics" "x=before; { x=after; } >out.txt",
      rejected "append requires the same file-open ownership" "printf value >>out.txt",
      rejected "computed file targets cannot bypass descriptor admission" "printf value >\"$target\"",
      rejected "nonstandard descriptors cannot collide with private child transport" "printf value 3>&1"
    ]

exact :: String -> Text -> TestTree
exact name source = H.testCaseSteps name $ \step -> do
  result <- translateBashScript strictConfig "ordered-descriptors.bash" source
  case result of
    Left failure -> H.assertFailure ("mandatory descriptor admission failed: " <> show failure)
    Right translated -> do
      ready <- shouldRunIntegration
      case ready of
        Left reason -> step ("skipped runtime: " <> reason)
        Right () -> do
          environment <- prepareEnv
          bash <- runShellWithMode ShellRunExec ShellBash environment source [] ""
          fish <- runShellWithMode ShellRunExec ShellFish environment (renderTranslation translated) [] ""
          let observation value = (rrExit value, rrStdout value, rrStderr value)
          H.assertEqual
            ((if null (translationDiagnostics translated) then "ZERO_DIAGNOSTIC_MISMATCH" else "DIAGNOSED_MISMATCH") <> "\n" <> toString source)
            (observation bash)
            (observation fish)

rejected :: String -> Text -> TestTree
rejected name source = H.testCase name $ do
  result <- translateBashScript strictConfig "ordered-descriptors.bash" source
  case result of
    Left failure -> H.assertBool "missing semantic rejection" (any (T.isPrefixOf "monk.semantic." . diagnosticCodeText . diagnosticCode) (failureDiagnostics failure))
    Right _ -> H.assertFailure "unsupported descriptor semantics produced executable output"

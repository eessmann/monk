module Unit.Pipefail (unitPipefailTests) where

import Monk.Translation
import ShellSupport
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as H

unitPipefailTests :: TestTree
unitPipefailTests =
  testGroup
    "Pipefail"
    [ H.testCaseSteps "pipefail toggles preserve single-command status and output" $ \step -> do
        let source = "set -o pipefail; printf 'one\\n'; false; printf 'status:%s\\n' \"$?\"; set +o pipefail; printf 'two\\n'"
        result <- translateBashScript strictConfig "pipefail.bash" source
        case result of
          Left failure -> H.assertFailure ("mandatory pipefail admission failed: " <> show failure)
          Right translated -> do
            ready <- shouldRunIntegration
            case ready of
              Left reason -> step ("skipped runtime: " <> reason)
              Right () -> do
                environment <- prepareEnv
                bash <- runShellWithMode ShellRunExec ShellBash environment source [] ""
                fish <- runShellWithMode ShellRunExec ShellFish environment (renderTranslation translated) [] ""
                let observations value = (rrExit value, rrStdout value, rrStderr value)
                H.assertEqual "ZERO_DIAGNOSTIC_MISMATCH" (observations bash) (observations fish)
    ]

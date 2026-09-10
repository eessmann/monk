module Unit.PlannedArithmetic
  ( unitPlannedArithmeticTests,
  )
where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Monk.Translation (renderTranslation, strictConfig, translateBashScript)
import ShellSupport (prepareEnv, shouldRunIntegration)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (env), proc, readCreateProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as H

unitPlannedArithmeticTests :: TestTree
unitPlannedArithmeticTests =
  testGroup
    "Planned integer arithmetic"
    [ exact "large numeric scalar crosses no executable argument boundary" ("n=" <> "1" <> T.replicate 200000 "0" <> "; printf '%s\\n' \"$((n+1))\"") "1\n",
      rejected "leading zero base prefix is not a Bash integer" "02#10",
      rejected "long leading zero base prefix is not a Bash integer" (T.replicate 5000 "0" <> "64#_"),
      exact "arithmetic assignment retains inherited export on unset local" "export x=outer; f() { local x; ((x=3)); printenv x; }; f; printf '%s\\n' \"$x\"" "3\nouter\n",
      exact "arithmetic update retains inherited export on unset local" "export x=outer; f() { local x; ((x++)); printenv x; }; f; printf '%s\\n' \"$x\"" "1\nouter\n",
      exact "compound arithmetic assignment retains inherited export on unset local" "export x=outer; f() { local x; ((x+=2)); printenv x; }; f; printf '%s\\n' \"$x\"" "2\nouter\n",
      exact "default assignment consumes the same unset local export marker" "export x=outer; f() { local x; printf '%s:' \"${x:=4}\"; printenv x; }; f; printf '%s\\n' \"$x\"" "4:4\nouter\n",
      exact "child arithmetic exports its local value without initializing the parent slot" "export x=outer; f() { local x; printf '<%s>:' \"$( ((x=3)); printenv x )\"; printenv x; printf 'parent:%s\\n' \"${x-unset}\"; }; f; printf '%s\\n' \"$x\"" "<3>:outer\nparent:unset\nouter\n",
      exact "division truncates at the intermediate operation" "printf '%s\\n' \"$((5/2*2))\"" "4\n",
      exact "division truncates toward zero" "printf '%s:%s\\n' \"$((-5/2))\" \"$((5/-2))\"" "-2:-2\n",
      exact "remainder has the dividend sign" "printf '%s:%s\\n' \"$((-5%2))\" \"$((5%-2))\"" "-1:1\n",
      exact "integer operations preserve values above double precision" "printf '%s\\n' \"$((9007199254740993+2))\"" "9007199254740995\n",
      exact "signed overflow wraps after each operation" "printf '%s:%s\\n' \"$((9223372036854775807+1))\" \"$(((9223372036854775807+1)/2))\"" "-9223372036854775808:-4611686018427387904\n",
      exact "minimum integer division overflow wraps with zero remainder" "printf '%s:%s\\n' \"$((-9223372036854775808/-1))\" \"$((-9223372036854775808%-1))\"" "-9223372036854775808:0\n",
      exact "large exponentiation computes bounded modular results" "printf '%s:%s\\n' \"$((2**9223372036854775807))\" \"$(((-1)**9223372036854775807))\"" "0:-1\n",
      exact "numeric bases use Bash digits" "printf '%s:%s:%s\\n' \"$((010+0xff))\" \"$((2#101))\" \"$((64#_))\"" "263:5:63\n",
      exact "bitwise operations use signed 64 bit values" "printf '%s:%s:%s\\n' \"$((~0))\" \"$((-8>>2))\" \"$((1<<64))\"" "-1:-2:1\n",
      exact "logical operators do not execute skipped updates" "n=0; printf '%s:%s:%s\\n' \"$((0 && n++))\" \"$((1 || n++))\" \"$n\"" "0:1:0\n",
      exact "conditional branches and comma preserve update order" "n=1; printf '%s:%s\\n' \"$((0 ? n++ : (n+=2, n++)))\" \"$n\"" "3:4\n",
      exact "compound assignment evaluates right side before writing" "n=3; printf '%s:%s\\n' \"$((n+=(n=5)))\" \"$n\"" "8:8\n",
      exact "skipped arithmetic errors are not evaluated" "printf '%s:%s:%s\\n' \"$((0 && 1/0))\" \"$((1 || 1/0))\" \"$((1 ? 9 : 1/0))\"" "0:1:9\n",
      differential "arithmetic command error stops later updates and preserves control status" "n=7; ((n=1/0,n=99)); printf 'after:%s:%s\\n' \"$?\" \"$n\"" Nothing,
      differential "arithmetic expansion error does not continue the command" "n=7; printf '%s\\n' \"$((n=1/0,n=99))\"; printf 'after:%s\\n' \"$n\"" Nothing,
      differential "first failing operation retains its own diagnostic suffix" "printf '%s\\n' \"$((1/(4/0),9/0))\"" Nothing,
      differential "negative exponent reports its actual parser cursor" "printf '%s\\n' \"$((2**(1-2),9))\"" Nothing,
      differential "arithmetic diagnostics retain original source line" "n=7\n# preserve this source line\n((n=1/0,n=99))\nprintf 'after:%s:%s\\n' \"$?\" \"$n\"" Nothing,
      differential "compound division error retains the later parser cursor" "n=7; ((n/=0,n=99)); printf 'after:%s:%s\\n' \"$?\" \"$n\"" Nothing,
      differential "negative exponent at expression end retains the last token" "printf '%s\\n' \"$((2**(1-2)))\"" Nothing,
      differential "multiline arithmetic command reports closing line" "((\n1/0\n)); printf 'after:%s\\n' \"$?\"" Nothing,
      differential "multiline expansion reports enclosing command start line" "printf '%s\\n' \"prefix\n$((1/0))\"" Nothing,
      differential "updates before arithmetic failure remain visible" "n=7; ((n++,1/0,n=99)); printf 'after:%s:%s\\n' \"$?\" \"$n\"" Nothing
    ]

exact :: String -> Text -> Text -> TestTree
exact name source output = differential name source (Just output)

differential :: String -> Text -> Maybe Text -> TestTree
differential name source expectedOutput = H.testCaseSteps name $ \step -> do
  readiness <- shouldRunIntegration
  case readiness of
    Left reason -> step ("skipped: " <> reason)
    Right () -> withSystemTempDirectory "monk-arithmetic" $ \directory -> do
      let bashPath = directory </> "source.bash"
          fishPath = directory </> "source.fish"
      TIO.writeFile bashPath source
      translated <- translateBashScript strictConfig bashPath source
      case translated of
        Left failure -> H.assertFailure ("strict arithmetic translation failed: " <> show failure)
        Right result -> do
          TIO.writeFile fishPath (renderTranslation result)
          environment <- prepareEnv
          (bashExit, bashOut, bashErr) <- readCreateProcessWithExitCode ((proc "bash" ["--noprofile", "--norc", bashPath]) {env = Just environment}) ""
          (fishExit, fishOut, fishErr) <- readCreateProcessWithExitCode ((proc "fish" ["--no-config", fishPath]) {env = Just environment}) ""
          for_ expectedOutput $ \output -> H.assertEqual "independent Bash expected output" (toString output) bashOut
          H.assertEqual "exit status" bashExit fishExit
          H.assertEqual "stdout" bashOut fishOut
          H.assertEqual "stderr" bashErr fishErr

rejected :: String -> Text -> TestTree
rejected name number = H.testCase name $ do
  result <- translateBashScript strictConfig "arithmetic-base.bash" ("printf '%s' \"$((" <> number <> "))\"")
  case result of
    Left _ -> pure ()
    Right _ -> H.assertFailure "invalid Bash base prefix was admitted"

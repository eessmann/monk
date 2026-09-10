module Unit.PlannedIsolation
  ( unitPlannedIsolationTests,
  )
where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Monk.Translation
import ShellSupport (prepareEnv, shouldRunIntegration)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (withBinaryFile)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (env, std_err, std_in, std_out), StdStream (UseHandle), proc, waitForProcess, withCreateProcess)
import System.Timeout qualified as Timeout
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as H

unitPlannedIsolationTests :: TestTree
unitPlannedIsolationTests =
  testGroup
    "Planned child isolation"
    [ sourceableExact
        "repeated source creates scalar output despite Fish PATH suffix"
        (emptyCallerContract {callerAmbientEffects = NoRelevantAmbientEffects, callerVariables = M.singleton "NEWPATH" (ScalarBinding OutputBinding GlobalBinding UnexportedBinding)})
        "NEWPATH='a:b'"
        ". \"$1\"; printf '%s:' \"$?\"; . \"$1\"; printf '%s:%s\\n' \"$?\" \"$NEWPATH\""
        "source \"$argv[1]\"; builtin printf '%s:' \"$status\"; source \"$argv[1]\"; builtin printf '%s:%s\\n' \"$status\" \"$NEWPATH\""
        "0:0:a:b\n",
      exact "child restores PATH suffix scalar without list conversion" "NEWPATH='a:b'; x=\"$(printf '%s' \"$NEWPATH\")\"; printf '%s\\n' \"$x\"" "a:b\n",
      sourceableExact
        "sourceable capture preserves caller local and return status"
        sourceableVisible
        "printf '%s:%s\\n' \"$(x=child; printf '%s' \"$x\")\" \"$x\"; return 7"
        "x=global; caller() { local x=local; . \"$1\"; printf '%s:%s\\n' \"$?\" \"$x\"; }; caller \"$1\"; printf '%s\\n' \"$x\""
        "set -g x global; function caller; set -l x local; source \"$argv[1]\"; builtin printf '%s:%s\\n' \"$status\" \"$x\"; end; caller \"$argv[1]\"; builtin printf '%s\\n' \"$x\""
        "child:local\n7:local\nglobal\n",
      sourceableExact
        "installed sourceable function retains its child closure"
        sourceableExported
        "greet() { printf '%s\\n' \"$(printf '%s' \"$1\")\"; }"
        ". \"$1\"; greet first; greet second"
        "source \"$argv[1]\"; greet first; greet second"
        "first\nsecond\n",
      rejected "nondraining pipeline rejects builtin SIGPIPE lifetime" ("{ printf '%s' '" <> largeValue <> "'; printf continued; } | head -c 1"),
      differentialWithInput "substitution preserves original stdin independently of metadata" "x=\"$(cat)\"; printf '<%s>\\n' \"$x\"" (Just "<original stdin>\n") "original stdin\n",
      exact "unset local slot shadows outer scalar in child snapshot" "v=outer; f() { local v; x=\"$(printf '%s' \"${v-fallback}\")\"; printf '%s:%s\\n' \"$x\" \"${v-fallback}\"; }; f; printf '%s\\n' \"$v\"" "fallback:fallback\nouter\n",
      exact "large exported scalar reaches child via framed state" ("v='" <> largeValue <> "'; export v; x=\"$(printf small)\"; printf '%s:%s' \"$x\" \"$v\"") ("small:" <> largeBytes),
      exact "exported scalar state remains visible to child external commands" "v=outer; export v; x=\"$(v=child; printenv v)\"; printf '%s:%s\\n' \"$x\" \"$v\"" "child:outer\n",
      exact "substitution writes stay in the child" "v=outer; x=\"$(v=child; printf '%s' \"$v\")\"; printf '%s:%s\\n' \"$v\" \"$x\"" "outer:child\n",
      exact "empty quoted output remains one field" "printf '<%s>\\n' \"$(true)\"" "<>\n",
      exact "capture removes trailing newlines but retains internal newlines" "x=\"$(printf 'a\\n\\nb\\n\\n')\"; printf '<%s>\\n' \"$x\"" "<a\n\nb>\n",
      exact "assignment takes the last substitution status" "x=\"$(false)$(true)\"; printf '%s:' \"$?\"; x=\"$(true)$(false)\"; printf '%s\\n' \"$?\"" "0:1\n",
      exact "one assignment command retains substitution status across assignments" "x=\"$(false)\" y=literal; printf '%s\\n' \"$?\"" "1\n",
      exact "separate assignment commands reset status" "x=\"$(false)\"; y=literal; printf '%s\\n' \"$?\"" "0\n",
      exact "local declaration masks substitution status" "f() { local x=\"$(false)\"; printf '%s\\n' \"$?\"; }; f" "0\n",
      exact "local declaration masks status after all operand expansions" "f() { local x=\"$(false)\" y=\"$?\"; printf '%s:%s\\n' \"$y\" \"$?\"; }; f" "1:0\n",
      exact "later expansion observes preceding substitution status" "true; x=\"$(false)$?\"; printf '%s:%s\\n' \"$x\" \"$?\"" "1:1\n",
      exact "substitution clears inherited errexit under the selected profile" "set -e; x=\"$(false; printf yes)\"; printf 'after:%s\\n' \"$x\"" "after:yes\n",
      exact "substitution exit does not exit the parent" "v=outer; x=\"$(v=child; exit 7)\"; printf '%s:%s:%s\\n' \"$?\" \"$v\" \"$x\"" "7:outer:\n",
      exact "nested child reads the enclosing child snapshot" "v=outer; x=\"$(v=inner; printf '%s' \"$(printf '%s' \"$v\")\")\"; printf '%s:%s\\n' \"$v\" \"$x\"" "outer:inner\n",
      exact "child owns a definite function closure" "v=outer; f() { printf '%s' \"$v\"; }; x=\"$(v=child; f)\"; printf '%s:%s\\n' \"$v\" \"$x\"" "outer:child\n",
      exact "child snapshots visible caller locals" "f() { local v=inner; x=\"$(v=child; printf '%s' \"$v\")\"; printf '%s:%s\\n' \"$v\" \"$x\"; }; f" "inner:child\n",
      exact "child argv changes stay isolated" "set -- a b; x=\"$(set -- c; printf '%s:%s' \"$#\" \"$1\")\"; printf '%s:%s:%s\\n' \"$#\" \"$1\" \"$x\"" "2:a:1:c\n",
      exact "subshell mutations and exit remain isolated" "v=outer; (v=child; printf '%s:' \"$v\"; exit 3); printf '%s:%s\\n' \"$?\" \"$v\"" "child:3:outer\n",
      exact "pipeline stage mutations remain isolated" "v=outer; printf x | { v=child; cat; }; printf ':%s:%s\\n' \"$?\" \"$v\"" "x:0:outer\n",
      exact "pipeline status follows the final stage by default" "false | true; printf '%s\\n' \"$?\"" "0\n",
      exact "pipefail selects the rightmost failed stage" "set -o pipefail; (exit 3) | (exit 7) | true; printf '%s\\n' \"$?\"" "7\n",
      exact "errexit applies inside compound pipeline stages" "set -e; { false; printf skipped; } | cat; printf 'after\\n'" "after\n",
      exact "conditional pipeline suppresses child errexit" "set -e; if { false; printf child; } | cat; then printf ':yes\\n'; fi" "child:yes\n",
      exact "capture preserves invalid UTF8 bytes" "x=\"$(printf '\\377\\n')\"; printf '%s\\n' \"$x\"" (BS.pack [255, 10]),
      exact "large scalar snapshot crosses no executable argument boundary" ("v='" <> largeValue <> "'; x=\"$(printf '%s' \"$v\")\"; printf '%s' \"$x\"") largeBytes,
      exact "large child script crosses no executable argument boundary" ("x=\"$(printf '%s' '" <> largeValue <> "')\"; printf '%s' \"$x\"") largeBytes,
      exact "large inherited argument crosses no executable argument boundary" ("set -- '" <> largeValue <> "'; x=\"$(printf '%s' \"$1\")\"; printf '%s' \"$x\"") largeBytes,
      exact "pipeline transports large snapshots and original stdin independently" ("v='" <> largeValue <> "'; printf '%s' \"$v\" | cat") largeBytes,
      differential "NUL output is removed with the original source warning" "x=\"$(printf '\\0a\\0')\"; printf '%s\\n' \"$x\"" Nothing,
      differential "arithmetic expansion failure terminates only its child" "x=\"$(printf '%s' \"$((1/0))\")\"; printf 'after:%s\\n' \"$?\"" Nothing,
      differential "failed substitution assignment triggers parent errexit" "set -e; x=\"$(false)\"; printf skipped" Nothing,
      differential "pipefail failure triggers parent errexit" "set -e; set -o pipefail; false | true; printf skipped" Nothing
    ]

sourceableVisible :: CallerContract
sourceableVisible = emptyCallerContract {callerAmbientEffects = NoRelevantAmbientEffects, callerVariables = M.singleton "x" (ScalarBinding InputOutputBinding VisibleBinding UnexportedBinding)}

sourceableExported :: CallerContract
sourceableExported = emptyCallerContract {callerAmbientEffects = NoRelevantAmbientEffects, callerExportedFunctions = S.singleton "greet"}

sourceableExact :: String -> CallerContract -> Text -> Text -> Text -> ByteString -> TestTree
sourceableExact name contract source bashCaller fishCaller expected = H.testCaseSteps name $ \step -> do
  readiness <- shouldRunIntegration
  case readiness of
    Left reason -> step ("skipped: " <> reason)
    Right () -> withSystemTempDirectory "monk-isolation-sourceable" $ \directory -> do
      let bashPath = directory </> "source.bash"
          fishPath = directory </> "source.fish"
          bashEntry = directory </> "caller.bash"
          fishEntry = directory </> "caller.fish"
          cfg = strictConfig {entryMode = Sourceable, callerContract = contract}
      BS.writeFile bashPath (encodeUtf8 source)
      BS.writeFile bashEntry (encodeUtf8 bashCaller)
      BS.writeFile fishEntry (encodeUtf8 fishCaller)
      translated <- translateBashScript cfg bashPath source >>= either (\failure -> H.assertFailure (show failure) >> error "unreachable") pure
      BS.writeFile fishPath (encodeUtf8 (renderTranslation translated))
      environment <- prepareEnv
      bash <- runBytes directory "bash" ["--noprofile", "--norc", bashEntry, bashPath] environment ""
      fish <- runBytes directory "fish" ["--no-config", fishEntry, fishPath] environment ""
      H.assertEqual "sourceable stdout/stderr/status" bash fish
      bytesEqual "independent sourceable stdout" expected (outBytes bash)

rejected :: String -> Text -> TestTree
rejected name source = H.testCase name $ do
  result <- translateBashScript strictConfig "planned-isolation.bash" source
  case result of
    Left failure -> H.assertBool "missing specific pipeline lifetime rejection" (any ((== "monk.semantic.pipeline-signal-lifetime") . diagnosticCodeText . diagnosticCode) (failureDiagnostics failure))
    Right _ -> H.assertFailure "nondraining builtin pipeline was admitted without an owned signal lifetime"

largeValue :: Text
largeValue = T.replicate 200000 "x"

largeBytes :: ByteString
largeBytes = BS.replicate 200000 120

exact :: String -> Text -> ByteString -> TestTree
exact name source output = differential name source (Just output)

differential :: String -> Text -> Maybe ByteString -> TestTree
differential name source expected = differentialWithInput name source expected ""

differentialWithInput :: String -> Text -> Maybe ByteString -> ByteString -> TestTree
differentialWithInput name source expected input = H.testCaseSteps name $ \step -> do
  readiness <- shouldRunIntegration
  case readiness of
    Left reason -> step ("skipped: " <> reason)
    Right () -> withSystemTempDirectory "monk-isolation" $ \directory -> do
      let bashPath = directory </> "planned-isolation.bash"
          fishPath = directory </> "planned-isolation.fish"
      BS.writeFile bashPath (encodeUtf8 source)
      result <- translateBashScript strictConfig bashPath source
      case result of
        Left failure -> H.assertFailure ("strict isolation translation failed: " <> show failure)
        Right translated -> do
          BS.writeFile fishPath (encodeUtf8 (renderTranslation translated))
          environment <- prepareEnv
          bash@(bashCode, bashOut, bashErr) <- runBytes directory "bash" ["--noprofile", "--norc", bashPath] environment input
          (fishCode, fishOut, fishErr) <- runBytes directory "fish" ["--no-config", fishPath] environment input
          forM_ expected $ \output -> bytesEqual "independent Bash stdout" output (outBytes bash)
          H.assertEqual "exit status" bashCode fishCode
          bytesEqual "stdout bytes" bashOut fishOut
          bytesEqual "stderr bytes" bashErr fishErr

bytesEqual :: String -> ByteString -> ByteString -> H.Assertion
bytesEqual label expected actual
  | BS.length expected + BS.length actual < 4096 = H.assertEqual label expected actual
  | otherwise = H.assertBool (label <> " differ; expected " <> show (BS.length expected) <> " bytes, got " <> show (BS.length actual)) (expected == actual)

outBytes :: (ExitCode, ByteString, ByteString) -> ByteString
outBytes (_, output, _) = output

runBytes :: FilePath -> String -> [String] -> [(String, String)] -> ByteString -> IO (ExitCode, ByteString, ByteString)
runBytes directory shell arguments environment input = do
  let outputPath = directory </> (shell <> ".stdout")
      errorPath = directory </> (shell <> ".stderr")
      inputPath = directory </> (shell <> ".stdin")
  BS.writeFile inputPath input
  code <- withBinaryFile outputPath WriteMode $ \output -> withBinaryFile errorPath WriteMode $ \errors -> withBinaryFile inputPath ReadMode $ \inputHandle -> do
    let process = (proc shell arguments) {env = Just environment, std_in = UseHandle inputHandle, std_out = UseHandle output, std_err = UseHandle errors}
    withCreateProcess process $ \_ _ _ handle -> do
      completed <- Timeout.timeout 30000000 (waitForProcess handle)
      maybe (H.assertFailure "shell byte comparison timed out" >> pure (ExitFailure 125)) pure completed
  (code,,) <$> BS.readFile outputPath <*> BS.readFile errorPath

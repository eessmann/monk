module Unit.PlannedInput (unitPlannedInputTests) where

import Data.ByteString qualified as BS
import Monk.Translation
import ShellSupport (prepareEnv, shouldRunIntegration)
import System.Directory (createDirectory, getPermissions, setOwnerExecutable, setPermissions)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (withBinaryFile)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (cwd, env, std_err, std_in, std_out), StdStream (UseHandle), proc, waitForProcess, withCreateProcess)
import System.Timeout qualified as Timeout
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as H

unitPlannedInputTests :: TestTree
unitPlannedInputTests =
  testGroup
    "Owned input and files"
    [ differentialWithInput "raw read preserves backslashes" "read -r value; printf '<%s>' \"$value\"" (Just "<a\\b>") "a\\b\n",
      differentialWithInput "read backslash continuation joins input lines" "read value; printf '<%s>' \"$value\"" (Just "<ab>") "a\\\nb\n",
      differentialWithInput "read scalar fields retain the final remainder" "read first rest; printf '<%s><%s>' \"$first\" \"$rest\"" (Just "<alpha><beta gamma>") "  alpha beta gamma  \n",
      differentialWithInput "REPLY preserves surrounding whitespace" "read; printf '<%s>' \"$REPLY\"" (Just "<  alpha  >") "  alpha  \n",
      differentialWithInput "custom delimiter stops before trailing input" "read -r -d , value; printf '<%s>:%s' \"$value\" \"$?\"" (Just "<one>:0") "one,two",
      differentialWithInput "read count leaves remaining bytes for next read" "read -r -n 3 first; read -r rest; printf '<%s><%s>' \"$first\" \"$rest\"" (Just "<abc><def>") "abcdef\n",
      differentialWithInput "zero read count consumes no input" "read -n 0 first; read rest; printf '<%s><%s>' \"$first\" \"$rest\"" (Just "<><abc>") "abc\n",
      differentialWithInput "EOF retains a partial line and failure status" "read value; printf '<%s>:%s' \"$value\" \"$?\"" (Just "<partial>:1") "partial",
      differentialWithInput "EOF clears an existing scalar" "value=before; read value; printf '<%s>:%s' \"$value\" \"$?\"" (Just "<>:1") "",
      differentialWithInput "read array creates a runtime dense vector" "read -a values; printf '%s:' \"${#values[@]}\"; printf '<%s>' \"${values[@]}\"" (Just "2:<one><two>") " one  two \n",
      differentialWithInput "empty read array owns zero fields" "read -a values; set -- \"${values[@]}\"; printf '%s' \"$#\"" (Just "0") "\n",
      differentialWithInput "read writes the nearest local binding" "value=outer; f() { local value=before; read value; printf '%s:' \"$value\"; }; f; printf '%s' \"$value\"" (Just "inner:outer") "inner\n",
      differentialWithInput "here-string supplies a trailing newline" "read -r value <<< 'a\\b'; printf '<%s>:%s' \"$value\" \"$?\"" (Just "<a\\b>:0") "",
      differentialWithInput "quoted heredoc retains parameter text" "value=expanded; read -r result <<'EOF'\n$value\nEOF\nprintf '<%s>' \"$result\"" (Just "<$value>") "",
      differentialWithInput "unquoted heredoc expands source parameters" "value=expanded; read -r result <<EOF\n$value\nEOF\nprintf '<%s>' \"$result\"" (Just "<expanded>") "",
      differentialWithInput "dashed heredoc strips source leading tabs" "value=expanded; cat <<-EOF\n\t$value\n\tend\nEOF\n" (Just "expanded\nend\n") "",
      differentialWithInput "file open and append retain byte order" "printf one > output; printf two >> output; cat < output" (Just "onetwo") "",
      differentialWithInput "shared redirected compound retains scalar effects" "value=before; { value=after; printf bytes; } > output; printf '%s:' \"$value\"; cat output" (Just "after:bytes") "",
      differentialWithInput "owned descriptor read uses the opened input" "printf 'input\\n' > input; { read -u 3 value; printf '%s' \"$value\"; } 3< input" (Just "input") "",
      differentialWithInput "ordered output duplication shares opened file" "{ printf out; printf err >&2; } > output 2>&1; cat output" (Just "outerr") "",
      differential "failed open retains source diagnostic and skips body" "cat < definitely-missing-file" Nothing,
      differentialWithInput "NUL delimiter reads bytes before NUL" "read -r -d '' value; printf '<%s>' \"$value\"" (Just "<one>") (BS.pack [111, 110, 101, 0, 116, 119, 111]),
      differentialWithInput "read array element zero remains a dense write" "read -a values; values[0]=new; printf '<%s>' \"${values[@]}\"" (Just "<new><two>") "one two\n",
      differential "ordinary assignment survives a successful file redirect" "value=before; value=after > output; printf '%s' \"$value\"" (Just "after"),
      rejected "file path effects remain excluded" "printf text >\"${name:=output}\"",
      rejected "redirected assignment effects remain excluded" "n=0; value=$((n++)) > output",
      H.testCase "sourceable input operations require session ownership" $ forM_ ["read value", "cat <<< text", "cat < input", "printf text 3>&1"] $ \source -> do
        result <- translateBashScript strictConfig {entryMode = Sourceable, callerContract = emptyCallerContract {callerAmbientEffects = NoRelevantAmbientEffects}} "input-sourceable.bash" source
        H.assertBool "sourceable input admitted" (isLeft result),
      differential "mixed expansion splits only unquoted segments" "IFS=:; value=a:b; printf '<%s>' pre${value}post" (Just "<prea><bpost>"),
      differential "empty quotes retain one field beside disappearing expansion" "value=; printf '<%s>' $value\"\" tail" (Just "<><tail>"),
      differential "dynamic wildcard expands matching files" "touch a.txt b.txt; pattern='*.txt'; printf '<%s>' $pattern" (Just "<a.txt><b.txt>"),
      differential "bracket wildcard expands matching files" "touch a1 a2 b1; printf '<%s>' a[12]" (Just "<a1><a2>"),
      differential "escaped wildcard stays literal during adjacent splitting" "value='a b'; printf '<%s>' \\*$value" (Just "<*a><b>"),
      differential "unmatched dynamic wildcard retains literal pattern" "pattern='no-match-*.xyz'; printf '<%s>' $pattern" (Just "<no-match-*.xyz>"),
      differential "prefix bindings are visible to the external process" "A=temporary sh -c 'printf \"%s\" \"$A\"'" (Just "temporary"),
      differential "prefix RHS values see preceding temporary assignments" "A=outer; A=inner B=\"$A\" sh -c 'printf \"%s:%s\" \"$A\" \"$B\"'; printf '/%s' \"$A\"" (Just "inner:inner/outer"),
      differential "command words expand before prefix bindings" "A=outer; A=inner printf '%s' \"$A\"" (Just "outer"),
      differential "prefix restores an initially unset binding" "A=temporary true; printf '<%s>' \"${A-unset}\"" (Just "<unset>"),
      differential "prefix restores an exported binding" "A=outer; export A; A=inner sh -c 'printf \"%s:\" \"$A\"'; printenv A" (Just "inner:outer\n"),
      differential "prefix RHS writes to other bindings survive invocation" "n=0; A=$((n++)) printf '%s:' \"$n\"; printf '%s' \"$n\"" (Just "0:1"),
      rejected "prefix RHS cannot write temporary destination names" "A=outer; A=$((A=2)) true",
      rejected "function prefix scope stays outside initial envelope" "f() { true; }; A=x f",
      differential "prefix RHS arithmetic precedes a failing file open" "n=0; A=$((n++)) true > missing-dir/output; printf '%s' \"$n\"" Nothing,
      differential "command words expand before a failing file open" "n=0; printf '%s' \"$((n++))\" > missing-dir/output; printf '%s' \"$n\"" Nothing,
      differential "read-write file descriptor shares its initial offset" "printf abc > input; { read -r -n 1 -u 3 value; printf '%s' \"$value\"; } 3<> input" (Just "a"),
      differential "dashed heredoc keeps tabs introduced by expansion" "value=$'\\tdata'; cat <<-EOF\n\t$value\nEOF\n" (Just "\tdata\n"),
      rejected "read cannot inherit an unowned nonstandard descriptor" "read -u 3 value",
      differential "process input path is consumed by cat" "cat <(printf input)" (Just "input"),
      differential "multiple process input paths are independently consumed" "cat <(printf one) <(printf two)" (Just "onetwo"),
      differential "process input becomes an owned read descriptor" "read -r value < <(printf \"input\\n\"); printf \"%s\" \"$value\"" (Just "input"),
      differential "temporary IFS read retains destination and restores binding" "IFS=:; IFS= read -r value <<< \" a b \"; printf \"<%s>:%s\" \"$value\" \"$IFS\"" (Just "< a b >::"),
      rejected "process path cannot escape in scalar storage" "value=<(printf x)",
      rejected "process path cannot escape through printable argv" "printf \"%s\" <(printf x)",
      rejected "process path cannot escape through arbitrary executable" "sh -c 'printf \"%s\" \"$1\"' sh <(printf x)",
      rejected "temporary read cannot write IFS itself" "IFS= read IFS",
      differential "process output endpoint drains into a file" "printf output > >(cat > output); wait \"$!\"; cat output" (Just "output"),
      differential "process output compound owns temporary IFS read" "printf \" a b \\n\" > >(while IFS= read -r line; do printf \"<%s>\" \"$line\"; done > output); wait \"$!\"; cat output" (Just "< a b >"),
      differential "unused underscore loop binding is a discard" "for _ in one two; do printf x; done" (Just "xx"),
      rejected "underscore loop binding cannot be observed" "for _ in one; do printf %s \"$_\"; done",
      differential "redirect filename sees prefix arithmetic side effects" "n=0; A=$((n++)) true > \"$n\"; test -f 1 && printf yes" (Just "yes"),
      differential "redirect filename sees restored temporary binding" "a=old; a=new true > \"$a\"; test -f old && printf yes" (Just "yes"),
      differential "runtime dense read array append retains its entire vector" "read -a values <<< \"one two\"; values+=(three four); printf \"%s:\" \"${#values[@]}\"; printf \"<%s>\" \"${values[@]}\"" (Just "4:<one><two><three><four>"),
      differential "runtime dense index zero preserves the unknown tail" "read -a values <<< \"one two three\"; values[0]=new; printf \"%s:\" \"${#values[@]}\"; printf \"<%s>\" \"${values[@]}\"" (Just "3:<new><two><three>"),
      rejected "runtime dense append cannot prove later indexed writes" "read -a values; values+=(one two); values[2]=bad",
      differential "read on write-only descriptor diagnoses IO failure without assignment" "{ value=before; read -r -u 3 value; code=$?; printf \"<%s>:%s\" \"$value\" \"$code\"; } 3> output" (Just "<before>:1"),
      differential "read on directory diagnoses IO failure without assignment" "{ value=before; read -r -u 3 value; code=$?; printf \"<%s>:%s\" \"$value\" \"$code\"; } 3< ." (Just "<before>:1"),
      differential "nested read with closed stderr has no inherited diagnostic fallback" "read x <<<'input'; (read -u 1 x) 2>&-; code=$?; printf '%s' \"$code\"" (Just "1"),
      differential "nested wait with closed stderr has no inherited diagnostic fallback" "read x <<<'input'; (wait 987654321) 2>&-; code=$?; printf '%s' \"$code\"" (Just "127"),
      differential "nested external with closed stderr avoids duplicate closure" "read x <<<'input'; (cat /dev/null) 2>&-; code=$?; printf '%s' \"$code\"" (Just "0"),
      differential "shared group read retains the original diagnostic stderr" "read x <<<'input'; { read -u 1 x; } 2>&-; code=$?; printf '%s' \"$code\"" (Just "1"),
      differentialWithInput "owned external function inherits its exec primitive" "copy() { cat; }; copy" (Just "stream\n") "stream\n",
      differentialWithInput "external pipeline preserves byte streams" "cat | tr a-z A-Z" (Just "STREAM\n") "stream\n",
      differential "background pipeline PID is the last external stage" "printf payload | sh -c 'IFS= read -r item; printf \"%s:%s\" \"$$\" \"$item\" > pidfile' & pid=$!; wait \"$pid\"; IFS=: read -r actual item < pidfile; test \"$actual\" = \"$pid\"; printf '%s:%s' \"$?\" \"$item\"" (Just "0:payload"),
      removedCwd,
      externalFailure "direct failed exec preserves permission status and source diagnostic" "chmod -x ./child; ./child; code=$?; printf '%s' \"$code\"" "126",
      externalFailure "supervised failed exec preserves permission status and source diagnostic" "true & wait; chmod -x ./child; ./child; code=$?; printf '%s' \"$code\"" "126",
      externalFailure "direct removed executable preserves missing-file diagnostic" "rm ./child; ./child; code=$?; printf '%s' \"$code\"" "127",
      externalFailure "supervised removed executable preserves missing-file diagnostic" "true & wait; rm ./child; ./child; code=$?; printf '%s' \"$code\"" "127",
      rejected "read timeout remains excluded" "read -t 1 value",
      rejected "read prompt remains excluded" "read -p prompt value",
      rejected "read exact-count flag remains excluded" "read -N 2 value",
      rejected "computed read flags remain excluded" "read \"$1\" value"
    ]

-- Keep fixture/output files outside the disposable execution directory.
removedCwd :: TestTree
removedCwd = H.testCaseSteps "removed cwd retains native invocation and relative-open semantics" $ \step -> do
  readiness <- shouldRunIntegration
  case readiness of
    Left reason -> step ("skipped: " <> reason)
    Right () -> withSystemTempDirectory "monk-removed-cwd" $ \directory -> do
      let working = directory </> "working"
          bashPath = directory </> "input.bash"
          fishPath = directory </> "input.fish"
          source = "true & wait; rmdir \"$1\"; cat /dev/null; printf before; printf ignored > relative; code=$?; printf ':%s' \"$code\""
      BS.writeFile bashPath (encodeUtf8 source)
      translated <- translateBashScript strictConfig bashPath source
      case translated of
        Left failure -> H.assertFailure (show failure)
        Right result -> do
          BS.writeFile fishPath (encodeUtf8 (renderTranslation result))
          environment <- prepareEnv
          createDirectory working
          bash <- runBytesAt directory working "bash" ["--noprofile", "--norc", bashPath, working] environment ""
          createDirectory working
          fish <- runBytesAt directory working "fish" ["--no-config", fishPath, working] environment ""
          bytesEqual "independent Bash stdout" "before:1" (outBytes bash)
          H.assertEqual "stdout/stderr/status after cwd removal" bash fish

externalFailure :: String -> Text -> ByteString -> TestTree
externalFailure label source expected = H.testCaseSteps label $ \step -> do
  readiness <- shouldRunIntegration
  case readiness of
    Left reason -> step ("skipped: " <> reason)
    Right () -> withSystemTempDirectory "monk-exec-failure" $ \directory -> do
      let bashPath = directory </> "input.bash"
          fishPath = directory </> "input.fish"
          executable = directory </> "child"
          prepare = do
            BS.writeFile executable "#!/bin/sh\nexit 0\n"
            permissions <- getPermissions executable
            setPermissions executable (setOwnerExecutable True permissions)
      BS.writeFile bashPath (encodeUtf8 source)
      translated <- translateBashScript strictConfig bashPath source
      case translated of
        Left failure -> H.assertFailure (show failure)
        Right result -> do
          BS.writeFile fishPath (encodeUtf8 (renderTranslation result))
          environment <- prepareEnv
          prepare
          bash <- runBytes directory "bash" ["--noprofile", "--norc", bashPath] environment ""
          prepare
          fish <- runBytes directory "fish" ["--no-config", fishPath] environment ""
          bytesEqual "independent Bash stdout" expected (outBytes bash)
          H.assertEqual "stdout/stderr/status after executable change" bash fish

rejected :: String -> Text -> TestTree
rejected name source = H.testCase name $ do
  result <- translateBashScript strictConfig "input-rejected.bash" source
  H.assertBool "unsupported input admitted" (isLeft result)

differential :: String -> Text -> Maybe ByteString -> TestTree
differential name source expected = differentialWithInput name source expected ""

differentialWithInput :: String -> Text -> Maybe ByteString -> ByteString -> TestTree
differentialWithInput name source expected input = H.testCaseSteps name $ \step -> do
  readiness <- shouldRunIntegration
  case readiness of
    Left reason -> step ("skipped: " <> reason)
    Right () -> withSystemTempDirectory "monk-input" $ \directory -> do
      let bashPath = directory </> "planned-input.bash"
          fishPath = directory </> "planned-input.fish"
      BS.writeFile bashPath (encodeUtf8 source)
      result <- translateBashScript strictConfig bashPath source
      case result of
        Left failure -> H.assertFailure ("strict input translation failed: " <> show failure)
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
runBytes directory = runBytesAt directory directory

runBytesAt :: FilePath -> FilePath -> String -> [String] -> [(String, String)] -> ByteString -> IO (ExitCode, ByteString, ByteString)
runBytesAt directory working shell arguments environment input = do
  let outputPath = directory </> (shell <> ".stdout")
      errorPath = directory </> (shell <> ".stderr")
      inputPath = directory </> (shell <> ".stdin")
  BS.writeFile inputPath input
  code <- withBinaryFile outputPath WriteMode $ \output -> withBinaryFile errorPath WriteMode $ \errors -> withBinaryFile inputPath ReadMode $ \inputHandle -> do
    let process = (proc shell arguments) {cwd = Just working, env = Just environment, std_in = UseHandle inputHandle, std_out = UseHandle output, std_err = UseHandle errors}
    withCreateProcess process $ \_ _ _ handle -> do
      completed <- Timeout.timeout 30000000 (waitForProcess handle)
      maybe (H.assertFailure "shell byte comparison timed out" >> pure (ExitFailure 125)) pure completed
  (code,,) <$> BS.readFile outputPath <*> BS.readFile errorPath

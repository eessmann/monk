module Unit.PlannedCommonCoverage
  ( unitPlannedCommonCoverageTests,
  )
where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Monk.Source
import Monk.Translation
import ShellSupport (prepareEnv, shouldRunIntegration)
import SourceTestSupport
import System.Directory (createFileLink)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (withBinaryFile)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (env, std_err, std_in, std_out), StdStream (NoStream, UseHandle), proc, waitForProcess, withCreateProcess)
import System.Timeout qualified as Timeout
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as H

unitPlannedCommonCoverageTests :: TestTree
unitPlannedCommonCoverageTests =
  testGroup
    "Common syntax native runtime coverage"
    [ exact "dollar bracket arithmetic" "value=$[1 + 2 * 3]; printf '%s\\n' \"$value\"" "7\n",
      exact "ANSI byte escapes" "value=$'line1\\nline2\\tend'; printf '%s\\n' \"$value\"" "line1\nline2\tend\n",
      exact "arithmetic for continue runs increment" "for ((i=0; i<3; i++)); do if [[ $i -eq 1 ]]; then continue; fi; printf '%s\\n' \"$i\"; done" "0\n2\n",
      exact "nested comma braces duplicate effects before expansion" "i=0; printf '%s\\n' {a,b}$((i++)); printf '%s\\n' \"$i\"" "a0\nb1\n2\n",
      exact "nested comma braces" "printf '%s\\n' pre{a,{b,c}}{1,2}" "prea1\nprea2\npreb1\npreb2\nprec1\nprec2\n",
      exact "lazy defaults skip and retain writes" "x=set; n=0; printf '%s:%s\\n' \"${x:-$((n++))}\" \"$n\"" "set:0\n",
      exact "lazy alternate uses selected operand" "x=set; n=0; printf '%s:%s\\n' \"${x:+$((n++))}\" \"$n\"" "0:1\n",
      exact "bounded parameter trimming" "x=v1.2.3-pre; y=${x#v}; printf '%s:%s\\n' \"${y%%-*}\" \"${y#*-}\"" "1.2.3:pre\n",
      exact "literal replacement" "x=--ascii; printf '%s\\n' \"${x/--}\"" "ascii\n",
      exact "scalar append reads old binding after RHS expansion" "x=old; x+=${x:=unused}; printf '%s\\n' \"$x\"" "oldold\n",
      exact "owned function shift" "f() { printf '%s\\n' \"$1\"; shift; printf '%s\\n' \"$1\"; }; f a b" "a\nb\n",
      exact "double bracket grouped lazy arithmetic" "n=0; if [[ x == x || $((n++)) -eq 0 ]]; then printf '%s\\n' \"$n\"; fi" "0\n",
      exact "ANSI invalid UTF8 remains one byte" "printf '%s' $'\\xff'" (BS.pack [255]),
      exact "ANSI NUL terminates the scalar" "printf '<%s>' $'a\\0ignored'" "<a>",
      exact "append observes arithmetic RHS write" "x=1; x+=$((x=2)); printf '%s\\n' \"$x\"" "22\n",
      exact "zero shift and excessive shift preserve arguments" "f() { shift 0; printf '%s:' \"$1\"; shift 8; printf '%s:%s\\n' \"$?\" \"$1\"; }; f a" "a:1:a\n",
      exact "nested loop continue belongs to inner loop" "for ((i=0;i<2;i++)); do for x in a b; do continue; done; printf '%s\\n' \"$i\"; done" "0\n1\n",
      exact "empty arithmetic for condition is true" "for ((i=0;;i++)); do printf '%s\\n' \"$i\"; if [[ $i -eq 1 ]]; then break; fi; done" "0\n1\n",
      exact "fixed unary test accepts option-looking value" "if test -n '-z'; then printf yes; fi" "yes",
      exact "fixed binary test accepts exclamation operand" "if test '!' = '!'; then printf yes; fi" "yes",
      exact "pure island wraps before signed division boundary" "n=9223372036854775807; printf '%s\\n' \"$(((n+1)*3 / -2))\"" "4611686018427387904\n",
      exact "pure island does not cross lazy mutation arm" "n=3; printf '%s:%s\\n' \"$((0 && (n=7) || (n+1)*2))\" \"$n\"" "1:3\n",
      exact "arithmetic for initialization failure skips body" "for ((i=1/0;i<2;i++)); do printf unreachable; done; printf 'status:%s\\n' \"$?\"" "status:1\n",
      exact "arithmetic for increment failure ends loop" "for ((i=0;i<2;i+=1/0)); do printf body; done; printf 'status:%s\\n' \"$?\"" "bodystatus:1\n",
      exact "arithmetic for condition failure ends loop" "for ((i=0;1/0;i++)); do printf unreachable; done; printf 'status:%s\\n' \"$?\"" "status:1\n",
      H.testCase "literal source shifts its owned argv and restores caller argv" $
        withSources ". ./child.bash a b; printf 'caller:%s\\n' \"$1\"" [("child.bash", "shift; printf 'child:%s\\n' \"$1\"")] $ \root environment -> do
          forM_ [Standalone, Sourceable] $ \mode -> do
            let cfg = strictConfig {entryMode = mode, callerContract = if mode == Sourceable then emptyCallerContract {callerAmbientEffects = NoRelevantAmbientEffects} else emptyCallerContract}
            graph <- requireGraph cfg environment root
            assertSourceEquivalent root environment graph ["root"],
      H.testCase "empty effective source argv cannot acquire shift ownership" $
        withSources ". ./child.bash \"$@\"" [("child.bash", "shift")] $ \root environment -> do
          result <- translateSourceGraphWithEnvironment strictConfig environment True root
          H.assertBool "unknown effective source argv admitted" (isLeft result),
      testGroup
        "source inherits argv only for zero effective fields"
        [ H.testCase (show mode <> " " <> toString operand) $
            withSources ("x=; . ./child.bash " <> operand <> "; printf 'caller:%s:%s\\n' \"$1\" \"$2\"") [("child.bash", "printf 'child:%s:<%s>:<%s>\\n' \"$#\" \"$1\" \"$2\"")] $ \root environment -> do
              let contract = emptyCallerContract {callerAmbientEffects = NoRelevantAmbientEffects, callerVariables = M.singleton "x" (ScalarBinding OutputBinding GlobalBinding UnexportedBinding)}
                  cfg = strictConfig {entryMode = mode, callerContract = if mode == Sourceable then contract else emptyCallerContract}
              graph <- requireGraph cfg environment root
              assertSourceEquivalent root environment graph ["alpha", "beta"]
        | mode <- [Standalone, Sourceable],
          operand <- ["$x", "\"$x\""]
        ],
      H.testCase "transitive source cannot write inherited argv" $
        withSources ". ./child.bash supplied" [("child.bash", ". ./grandchild.bash"), ("grandchild.bash", "shift")] $ \root environment -> do
          result <- translateSourceGraphWithEnvironment strictConfig environment True root
          H.assertBool "transitive inherited argv write admitted" (isLeft result),
      exact "fixed negated test treats unary-looking value literally" "if test ! -n; then printf unreachable; else printf no; fi" "no",
      exact "fixed negated binary test" "if test ! a = b; then printf yes; fi" "yes",
      exact "fixed numeric test with canonical data" "n=7; if [ \"$n\" -gt 3 ]; then printf yes; fi" "yes",
      rejected "fixed test rejects runtime cardinality changes" "x='! -n'; test $x",
      rejected "numeric test does not reinterpret octal as decimal" "test 010 -eq 8",
      exact "single quotes inside quoted alternate stay literal" "x=set; printf '<%s>' \"${x:+'a b'}\"" "<'a b'>",
      exact "single quotes inside unquoted assignment alternate quote the operand" "x=set; y=${x:+'a b'}; printf '<%s>' \"$y\"" "<a b>",
      exact "ANSI unknown Unicode beyond Bash range emits nothing" "printf '<%s>' $'\\Uffffffffz\\?'" "<z?>",
      exact "positional alternates distinguish empty and missing values" "f() { printf '<%s:%s>' \"${1+set}\" \"${1:+nonempty}\"; }; f ''; f" "<set:><:>",
      H.testCase "field fast path does not assume IFS after a function call" $ do
        let source = "f() { IFS=a; }; f; x=abc; printf '<%s>' $x"
        readiness <- shouldRunIntegration
        case readiness of
          Left _ -> pure ()
          Right () -> withSystemTempDirectory "monk-ifs-proof" $ \directory -> do
            let bashPath = directory </> "ifs.bash"
                fishPath = directory </> "ifs.fish"
            translated <- translateBashScript strictConfig bashPath source
            case translated of
              Left failure -> H.assertFailure (show failure)
              Right value -> do
                BS.writeFile bashPath (encodeUtf8 source)
                BS.writeFile fishPath (encodeUtf8 (renderTranslation value))
                environment <- prepareEnv
                bash <- runBytes directory "bash" ["--noprofile", "--norc", bashPath] environment
                fish <- runBytes directory "fish" ["--no-config", fishPath] environment
                H.assertEqual "IFS after call" bash fish,
      exact "arithmetic expansion assignment retains numeric evidence" "n=$[2+3]; printf '%s' \"$((n*2))\"" "10",
      exact "arithmetic for increment joins continue values" "x=1; for ((i=0;i<4;i+=x)); do printf '%s\\n' \"$i\"; if [[ $i -eq 0 ]]; then x=1; continue; fi; x=2; done; printf '%s\\n' \"$i\"" "0\n1\n3\n5\n",
      rejected "arithmetic for rejects expression string on continue edge" "x=1; for ((i=0;i<3;i+=x)); do if [[ $i -eq 0 ]]; then x='1/0'; continue; fi; x=2; done",
      rejected "single bracket rejects noncanonical decimal" "[ 010 -eq 10 ]",
      rejected "single bracket rejects hexadecimal data" "[ 0x10 -eq 16 ]",
      rejected "single bracket rejects pathname expansion" "[ abc = a* ]",
      exact "single bracket quoted pattern is literal" "if [ abc = 'a*' ]; then printf bad; else printf good; fi" "good",
      exact "break exit does not freeze fallthrough assignment" "x=1; for i in a b; do if test \"$i\" = a; then x=2; break; fi; x=1; done; printf '%s' \"$((x))\"" "2",
      rejected "break exit cannot restore numeric fact" "x=1; for i in a b; do if test \"$i\" = a; then x='1/0'; break; fi; x=1; done; echo \"$((x))\"",
      rejected "while continue cannot restore numeric fact" "x=1; while ((x<3)); do x='1/0'; continue; x=1; done",
      rejected "for in continue cannot restore numeric fact" "x=1; for i in a b; do echo \"$((x))\"; x='1/0'; continue; x=1; done",
      exact "sequential loops do not share continuation facts" "for i in a; do continue; done; x=1; for ((i=0;i<1;i+=x)); do continue; done; printf '%s' \"$i\"" "1",
      rejected "shift rejects hexadecimal count" "f() { shift 0x1; }; f a b",
      rejected "shift rejects overflowing decimal count" "f() { shift 18446744073709551616; }; f a b",
      exact "shift accepts leading-zero decimal count" "f() { shift 01; printf '%s' \"$1\"; }; f a b" "b",
      rejected "arithmetic command failure cannot establish numeric writes" "x='1/0'; ((1/0,x=1)); echo \"$((x))\"",
      rejected "arithmetic for initial failure cannot establish numeric writes" "x='1/0'; for ((1/0,x=1;0;0)); do echo unused; done; echo \"$((x))\"",
      rejected "arithmetic for increment failure cannot establish numeric writes" "x=1; for ((i=0;i<1;1/0,x=1)); do x='1/0'; done; echo \"$((x))\"",
      exact "arithmetic failure retains partial write without entry constant" "x=1; ((x=2,1/0,x=1)); printf '%s' \"$((x))\"" "2",
      exact "arithmetic for initial error retains partial write" "x=1; for ((x=2,1/0,x=1;0;0)); do printf unused; done; printf '%s' \"$((x))\"" "2",
      rejected "compound division failure cannot establish following numeric write" "x='1/0'; y=1; ((y/=0,x=1)); echo \"$((x))\"",
      exact "constant division establishes numeric loop initializer" "for ((i=6/3;i<4;i++)); do printf '%s' \"$i\"; done" "23",
      exact "successful closed division retains definite numeric write" "((x=6/3)); printf '%s' \"$((x+1))\"" "3",
      exact "successful closed power retains definite numeric write" "((x=2**3)); printf '%s' \"$((x+1))\"" "9",
      exact "single bracket negates empty scalar" "if [ ! \"\" ]; then printf yes; fi" "yes",
      exact "single bracket negates nonempty scalar" "if [ ! x ]; then printf bad; else printf yes; fi" "yes",
      testGroup "fixed test file size and symlink predicates" [differentialWithSetup mode filePredicateSource (Just "1010000011110000") | mode <- [Standalone, Sourceable]],
      rejected "array append remains excluded" "a=(); a+=(x)",
      rejected "computed parameter pattern remains excluded" "x=word; printf '%s' \"${x#$1}\""
    ]

exact :: String -> Text -> ByteString -> TestTree
exact name source output = testGroup name [differential mode source (Just output) | mode <- [Standalone, Sourceable]]

differential :: EntryMode -> Text -> Maybe ByteString -> TestTree
differential mode source = differentialWithSetup mode (const (pure source))

filePredicateSource :: FilePath -> IO Text
filePredicateSource directory = do
  let emptyFile = directory </> "empty"
      full = directory </> "nonempty"
      link = directory </> "link"
      dangling = directory </> "dangling"
      quote path = "'" <> T.replace "'" "'\\''" (toText path) <> "'"
      command bracket operator path = (if bracket then "[ " else "test ") <> operator <> " " <> quote path <> (if bracket then " ]" else "") <> "; printf '%s' \"$?\"; "
  BS.writeFile emptyFile ""
  BS.writeFile full "data"
  createFileLink full link
  createFileLink (directory </> "missing") dangling
  pure (mconcat [command bracket "-s" path | bracket <- [False, True], path <- [emptyFile, full]] <> mconcat [command bracket operator path | path <- [link, full, dangling], bracket <- [False, True], operator <- ["-L", "-h"]])

differentialWithSetup :: EntryMode -> (FilePath -> IO Text) -> Maybe ByteString -> TestTree
differentialWithSetup mode setup expected = H.testCaseSteps (show mode) $ \step -> do
  readiness <- shouldRunIntegration
  case readiness of
    Left reason -> step ("skipped: " <> reason)
    Right () -> withSystemTempDirectory "monk-common" $ \directory -> do
      source <- setup directory
      let contract = emptyCallerContract {callerAmbientEffects = NoRelevantAmbientEffects, callerVariables = M.fromList [(name, ScalarBinding OutputBinding GlobalBinding UnexportedBinding) | name <- ["x", "y", "n", "i", "value"]], callerExportedFunctions = S.singleton "f"}
          cfg = strictConfig {entryMode = mode, callerContract = if mode == Sourceable then contract else emptyCallerContract}
          bashPath = directory </> "common.bash"
          fishPath = directory </> "common.fish"
      result <- translateBashScript cfg bashPath source
      case result of
        Left failure -> H.assertFailure ("strict common translation failed: " <> show failure)
        Right translated -> do
          environment <- prepareEnv
          BS.writeFile bashPath (encodeUtf8 source)
          BS.writeFile fishPath (encodeUtf8 (renderTranslation translated))
          bash <- runBytes directory "bash" (["--noprofile", "--norc"] <> if mode == Standalone then [bashPath] else ["-c", ". \"$1\"", "common", bashPath]) environment
          fish <- runBytes directory "fish" (["--no-config"] <> if mode == Standalone then [fishPath] else ["-c", "source \"$argv[1]\"", fishPath]) environment
          forM_ expected $ \output -> H.assertEqual "independent Bash stdout" output (outBytes bash)
          H.assertEqual "exit status, stdout bytes, stderr bytes" bash fish

outBytes :: (ExitCode, ByteString, ByteString) -> ByteString
outBytes (_, output, _) = output

runBytes :: FilePath -> String -> [String] -> [(String, String)] -> IO (ExitCode, ByteString, ByteString)
runBytes directory shell arguments environment = do
  let outputPath = directory </> (shell <> ".stdout")
      errorPath = directory </> (shell <> ".stderr")
  code <- withBinaryFile outputPath WriteMode $ \output -> withBinaryFile errorPath WriteMode $ \errors -> do
    let process = (proc shell arguments) {env = Just environment, std_in = NoStream, std_out = UseHandle output, std_err = UseHandle errors}
    withCreateProcess process $ \_ _ _ handle -> do
      completed <- Timeout.timeout 30000000 (waitForProcess handle)
      maybe (H.assertFailure "shell byte comparison timed out" >> pure (ExitFailure 125)) pure completed
  (code,,) <$> BS.readFile outputPath <*> BS.readFile errorPath

rejected :: String -> Text -> TestTree
rejected name source = testGroup name [H.testCase (show mode) (check mode) | mode <- [Standalone, Sourceable]]
  where
    check mode = do
      result <- translateBashScript strictConfig {entryMode = mode} "planned-primitives.bash" source
      case result of
        Left _ -> pure ()
        Right _ -> H.assertFailure "unsupported syntax admitted"

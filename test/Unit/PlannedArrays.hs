module Unit.PlannedArrays (unitPlannedArrayTests) where

import Data.ByteString qualified as BS
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

unitPlannedArrayTests :: TestTree
unitPlannedArrayTests =
  testGroup
    "Dense indexed arrays"
    [ exact "array elements preserve empty and newline bytes" "a=(one '' $'two\\nthree'); printf '<%s>' \"${a[@]}\"" "<one><><two\nthree>",
      exact "empty array splice contributes zero fields" "a=(); set -- \"${a[@]}\"; printf '%s' \"$#\"" "0",
      exact "literal prefix and suffix force an empty array field" "a=(); set -- \"x${a[@]}y\"; printf '%s:%s' \"$#\" \"$1\"" "1:xy",
      exact "array prefix and suffix bind first and last fields" "a=(one two); printf '<%s>' \"x${a[@]}y\"" "<xone><twoy>",
      exact "array replacement discards prior elements" "a=(one two three); a=(next); printf '%s:<%s>' \"${#a[@]}\" \"${a[1]}\"" "1:<>",
      exact "array append retains empty elements" "a=(one); a+=('' two); printf '%s:' \"${#a[@]}\"; printf '<%s>' \"${a[@]}\"" "3:<one><><two>",
      exact "contiguous element write extends dense array" "a=(one); a[1]=two; a[0]=new; printf '<%s>' \"${a[@]}\"" "<new><two>",
      exact "bare array variable reads element zero" "a=(one two); printf '%s:%s' \"$a\" \"${a}\"" "one:one",
      exact "array elements preserve invalid UTF8" "a=($'\\xff' ''); printf '<%s>' \"${a[@]}\"" (BS.pack [60, 255, 62, 60, 62]),
      exact "array construction sequences arithmetic expansion" "n=0; a=(\"$((n++))\" \"$((n++))\"); printf '%s:' \"$n\"; printf '<%s>' \"${a[@]}\"" "2:<0><1>",
      exact "child snapshots arrays with empty fields" "a=(one '' two); x=\"$(printf '<%s>' \"${a[@]}\")\"; printf '%s' \"$x\"" "<one><><two>",
      exact "child array mutation stays isolated" "a=(parent ''); (a[0]=child; printf '<%s>' \"${a[@]}\"); printf '<%s>' \"${a[@]}\"" "<child><><parent><>",
      exact "function owns an array established in its body" "f() { a=(one ''); printf '<%s>' \"${a[@]}\"; }; f" "<one><>",
      exact "equal branch shapes retain a safe element bound" "a=(one two); if true; then a=(left right); else a=(no yes); fi; a[1]=new; printf '<%s>' \"${a[@]}\"" "<left><new>",
      exact "multiple array snapshots preserve argv and empty fields" "a=(one ''); b=('' two); set -- '' tail; x=\"$(printf '<%s>' \"${a[@]}\" \"${b[@]}\" \"$@\")\"; printf '%s' \"$x\"" "<one><><><two><><tail>",
      exact "scalar becomes an owned dense array" "a=scalar; a=(one two); printf '<%s>' \"${a[@]}\"" "<one><two>",
      exact "scalar assignment updates element zero without truncating array" "a=(one two); a=new; printf '<%s>' \"${a[@]}\"" "<new><two>",
      exact "zero length array survives a child snapshot" "a=(); x=\"$(printf '%s' \"${#a[@]}\")\"; printf '%s' \"$x\"" "0",
      H.testCaseSteps "ambient array-name binding fails the owned entry contract" $ \step -> do
        readiness <- shouldRunIntegration
        case readiness of
          Left reason -> step ("skipped: " <> reason)
          Right () -> withSystemTempDirectory "monk-array-contract" $ \directory -> do
            result <- translateBashScript strictConfig "array-contract.bash" "a=(one); printf unreachable"
            translation <- either (\failure -> H.assertFailure (show failure) >> error "unreachable") pure result
            let path = directory </> "array.fish"
            BS.writeFile path (encodeUtf8 (renderTranslation translation))
            environment <- prepareEnv
            (code, output, errors) <- runBytes directory "fish" ["--no-config", path] (("a", "ambient") : filter ((/= "a") . fst) environment) ""
            H.assertEqual "contract exit" (ExitFailure 125) code
            H.assertEqual "no effects before rejection" "" output
            H.assertBool "binding-specific contract diagnostic" ("preexisting array binding a" `BS.isInfixOf` errors),
      rejected "sparse element write is rejected" "a=(one); a[3]=gap",
      rejected "associative array declaration is rejected" "declare -A a=([key]=value)",
      rejected "overflowing array index write is rejected" "a=(one); a[999999999999999999999999999]=value",
      rejected "unknown array index write is rejected" "a=(one); a[$1]=value",
      rejected "array index-list expansion is rejected" "a=(one); printf '%s' \"${!a[@]}\"",
      rejected "unquoted array splice is rejected" "a=(one); printf '%s' ${a[@]}",
      rejected "array export is rejected" "a=(one); export a",
      rejected "unequal branch shapes are not assumed dense" "a=(one); if true; then a+=(two); fi; a[2]=three"
    ]

rejected :: String -> Text -> TestTree
rejected name source = H.testCase name $ do
  result <- translateBashScript strictConfig "array-rejected.bash" source
  H.assertBool "unsupported array admitted" (isLeft result)

exact :: String -> Text -> ByteString -> TestTree
exact name source output = differential name source (Just output)

differential :: String -> Text -> Maybe ByteString -> TestTree
differential name source expected = differentialWithInput name source expected ""

differentialWithInput :: String -> Text -> Maybe ByteString -> ByteString -> TestTree
differentialWithInput name source expected input = H.testCaseSteps name $ \step -> do
  readiness <- shouldRunIntegration
  case readiness of
    Left reason -> step ("skipped: " <> reason)
    Right () -> withSystemTempDirectory "monk-array" $ \directory -> do
      let bashPath = directory </> "planned-array.bash"
          fishPath = directory </> "planned-array.fish"
      BS.writeFile bashPath (encodeUtf8 source)
      result <- translateBashScript strictConfig bashPath source
      case result of
        Left failure -> H.assertFailure ("strict array translation failed: " <> show failure)
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

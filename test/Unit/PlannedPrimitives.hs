module Unit.PlannedPrimitives
  ( unitPlannedPrimitivesTests,
  )
where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Monk.Translation (TranslationStatistics (..), renderTranslation, strictConfig, translateBashScript, translationStatistics)
import ShellSupport (prepareEnv, shouldRunIntegration)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (withBinaryFile)
import System.IO.Temp (withSystemTempDirectory)
import System.Info (os)
import System.Process (CreateProcess (env, std_err, std_in, std_out), StdStream (NoStream, UseHandle), proc, waitForProcess, withCreateProcess)
import System.Timeout qualified as Timeout
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as H

unitPlannedPrimitivesTests :: TestTree
unitPlannedPrimitivesTests =
  testGroup
    "Planned word and builtin primitives"
    [ H.testCase "native scalar conditional uses only the exact output boundary" $ do
        result <- translateBashScript strictConfig "native-greeting.bash" "name=$1; if test -n \"$name\"; then printf 'Hello %s\\n' \"$name\"; else printf '%s\\n' Hello; fi"
        case result of
          Left failure -> H.assertFailure (show failure)
          Right translation -> do
            let output = renderTranslation translation
                stats = translationStatistics translation
            H.assertEqual "writer and signal termination sites" 2 (statisticsNativeCallSites stats)
            H.assertEqual "one output boundary" 1 (statisticsHelperDefinitions stats)
            H.assertBool "unnecessary field temporary" (not ("_field_" `T.isInfixOf` output))
            H.assertBool "unnecessary source-status slot" (not ("__monk_plan_0_status" `T.isInfixOf` output)),
      H.testCase "literal-only control needs no private namespace" $ do
        result <- translateBashScript strictConfig "native-literal.bash" "true"
        case result of
          Left failure -> H.assertFailure (show failure)
          Right translation -> H.assertBool "literal command carries private state" (not ("__monk_" `T.isInfixOf` renderTranslation translation)),
      H.testCase "mixed arithmetic and native control keep scalar operands direct" $ do
        result <- translateBashScript strictConfig "mixed-native-region.bash" mixedNativeRegion
        case result of
          Left failure -> H.assertFailure (show failure)
          Right translation -> H.assertBool "native suffix unnecessarily captures scalar fields" (not ("_field_" `T.isInfixOf` renderTranslation translation)),
      exact "mixed native region preserves final short-circuit status" mixedNativeRegion "<1>:ok:<0>",
      exact "native region reads incoming bounded-operation status" "n=0; ((n)); printf '<%s>' \"$?\"; false && printf bad; printf '<%s>' \"$?\"" "<1><1>",
      exact "native conditional has zero status when no branch runs" "if false; then printf bad; fi; printf '%s' \"$?\"" "0",
      exact "native short circuit preserves selected status" "false && printf bad; printf '%s:' \"$?\"; true || printf bad; printf '%s' \"$?\"" "1:0",
      exact "native scalar assignment retains incoming export" "export x=before; x=after; printenv x" "after\n",
      exact "native scalar reads retain spaces and empty arguments" "set -- '' 'a b'; x=$1; printf '<%s>' \"$x\" \"$2\" \"$3\"" "<><a b><>",
      H.testCase "standalone admission output grows linearly with bindings" $ do
        sizes <- forM [4, 8] $ \count -> do
          let source = T.intercalate "; " ["v" <> show index <> "=value" | index <- [1 .. count :: Int]]
          result <- translateBashScript strictConfig "guard-growth.bash" source
          either (\failure -> H.assertFailure (show failure) >> pure 0) (pure . T.length . renderTranslation) result
        case sizes of
          [small, large] -> H.assertBool ("rendered guard sizes: " <> show sizes) (small > 0 && large < 3 * small)
          _ -> H.assertFailure "missing guard size sample",
      exact "echo octal and hexadecimal escapes emit raw bytes" "echo -e '\\0377\\xff'" (BS.pack [255, 255, 10]),
      exact "echo Unicode escapes follow the selected C locale" "echo -e '\\uD800\\U00110000\\U7fffffff'" "\\uD800\\U00110000\\U7FFFFFFF\n",
      exact "echo out of range Unicode follows the pinned Nix Bash build" "echo -e 'a\\Uffffffffb'" (if os == "darwin" then "a\\UFFFFFFFFb\n" else "ab\n"),
      exact "echo stop escape suppresses following arguments and newline" "echo -e 'one\\cignored' two" "one",
      exact "concatenation freezes earlier scalar before later arithmetic update" "n=1; printf '%s\\n' \"$n$((n=2))$n\"" "122\n",
      exact "quoted argv prefix is evaluated before lazy suffix write" "set -- a b; unset x; printf '<%s>\\n' \"$x$@${x:=after}\"" "<a>\n<bafter>\n",
      exact "prefix assignment installation precedes later substitution" "x=before; x=after y=$(printf '%s' \"$x\") printenv x y; printf '<%s>' \"$x\"" "after\nafter\n<before>",
      exact "builtin dispatch bypasses a function of the same name" "printf() { echo shadow; }; builtin printf '%s\\n' exact" "exact\n",
      exact "command dispatch retains builtin printf" "printf() { echo shadow; }; command printf '%s\\n' exact" "exact\n",
      exact "printf canonical signed decimal values" "printf '%d:%d\\n' 42 -7" "42:-7\n",
      exact "large echo argument uses framed primitive input" ("x='" <> T.replicate 200000 "x" <> "'; echo -n \"$x\"") (BS.replicate 200000 120),
      exact "large split field uses framed primitive input" ("x='" <> T.replicate 200000 "x" <> "'; printf '%s' $x") (BS.replicate 200000 120),
      exact "large quoted argument uses framed primitive input" ("set -- '" <> T.replicate 200000 "x" <> "'; printf '%s' \"$@\"") (BS.replicate 200000 120),
      exact "for binding remains visible after the loop" "for x in a b; do :; done; printf '%s\\n' \"$x\"" "b\n",
      exact "for assignment consumes inherited export state" "export x=outer; f() { local x; for x in inner; do :; done; printenv x; printf '%s\\n' \"$x\"; }; f" "inner\ninner\n",
      exact "break establishes the loop result status" "for i in a b; do if test \"$i\" = b; then break; fi; false; done; printf '%s\\n' \"$?\"" "0\n",
      exact "continue establishes the loop result status" "for i in a b; do if test \"$i\" = b; then continue; fi; false; done; printf '%s\\n' \"$?\"" "0\n",
      rejected "printf variable assignment flag needs an owned mutation" "printf -v x '%s' hi",
      rejected "numeric printf does not interpret unknown scalar data" "printf '%d\\n' \"$value\"",
      rejected "numeric printf rejects noncanonical overflow or bases" "printf '%d\\n' 18446744073709551616",
      rejected "printf incomplete hex escape rejects" "printf '\\x'"
    ]

mixedNativeRegion :: Text
mixedNativeRegion = "n=0; ((n += 1)); if test \"$n\" = 1; then printf '<%s>' \"$n\"; else printf bad; fi; false || printf ':ok'; printf ':<%s>' \"$?\""

exact :: String -> Text -> ByteString -> TestTree
exact name source output = differential name source (Just output)

differential :: String -> Text -> Maybe ByteString -> TestTree
differential name source expected = H.testCaseSteps name $ \step -> do
  readiness <- shouldRunIntegration
  case readiness of
    Left reason -> step ("skipped: " <> reason)
    Right () -> do
      result <- translateBashScript strictConfig "planned-primitives.bash" source
      case result of
        Left failure -> H.assertFailure ("strict primitive translation failed: " <> show failure)
        Right translated -> withSystemTempDirectory "monk-primitives" $ \directory -> do
          environment <- prepareEnv
          let bashPath = directory </> "planned-primitives.bash"
              fishPath = directory </> "planned-primitives.fish"
          BS.writeFile bashPath (encodeUtf8 source)
          BS.writeFile fishPath (encodeUtf8 (renderTranslation translated))
          bash <- runBytes directory "bash" ["--noprofile", "--norc", bashPath] environment
          fish <- runBytes directory "fish" ["--no-config", fishPath] environment
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
rejected name source = H.testCase name $ do
  result <- translateBashScript strictConfig "planned-primitives.bash" source
  case result of
    Left _ -> pure ()
    Right _ -> H.assertFailure "unsupported printf admitted"

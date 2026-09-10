module Unit.PlannedPrimitives
  ( unitPlannedPrimitivesTests,
  )
where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Monk.Translation (renderTranslation, strictConfig, translateBashScript)
import ShellSupport (prepareEnv, shouldRunIntegration)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (withBinaryFile)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (env, std_err, std_in, std_out), StdStream (NoStream, UseHandle), proc, waitForProcess, withCreateProcess)
import System.Timeout qualified as Timeout
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as H

unitPlannedPrimitivesTests :: TestTree
unitPlannedPrimitivesTests =
  testGroup
    "Planned word and builtin primitives"
    [ H.testCase "standalone admission output grows linearly with bindings" $ do
        sizes <- forM [4, 8] $ \count -> do
          let source = T.intercalate "; " ["v" <> show index <> "=value" | index <- [1 .. count :: Int]]
          result <- translateBashScript strictConfig "guard-growth.bash" source
          either (\failure -> H.assertFailure (show failure) >> pure 0) (pure . T.length . renderTranslation) result
        case sizes of
          [small, large] -> H.assertBool ("rendered guard sizes: " <> show sizes) (small > 0 && large < 3 * small)
          _ -> H.assertFailure "missing guard size sample",
      exact "echo octal and hexadecimal escapes emit raw bytes" "echo -e '\\0377\\xff'" (BS.pack [255, 255, 10]),
      exact "echo Unicode escapes follow the selected C locale" "echo -e '\\uD800\\U00110000\\U7fffffff'" "\\uD800\\U00110000\\U7FFFFFFF\n",
      exact "echo out of range Unicode escape emits no bytes" "echo -e 'a\\Uffffffffb'" "ab\n",
      exact "echo stop escape suppresses following arguments and newline" "echo -e 'one\\cignored' two" "one",
      exact "concatenation freezes earlier scalar before later arithmetic update" "n=1; printf '%s\\n' \"$n$((n=2))$n\"" "122\n",
      exact "quoted argv prefix is evaluated before lazy suffix write" "set -- a b; unset x; printf '<%s>\\n' \"$x$@${x:=after}\"" "<a>\n<bafter>\n",
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

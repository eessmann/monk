module Unit.OutputBundle (unitOutputBundleTests) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as M
import Data.Text.IO qualified as TIO
import Language.Fish.DSL (renderScript)
import Monk.Output
import Monk.Source
import Monk.Translation
import ShellSupport (prepareEnv, readCreateProcessWithTimeout, shouldRunIntegration)
import SourceTestSupport
import System.Directory (copyFile, doesFileExist, findExecutable, getCurrentDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (CreateProcess (cwd, env), proc)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

unitOutputBundleTests :: TestTree
unitOutputBundleTests =
  testGroup
    "Owned output bundles"
    [ testCase "combined planning does not write output" $
        withSources "printf 'value\\n'\n" [] $ \root environment -> do
          graph <- requireGraph strictConfig environment root
          let target = sourceWorkingDirectory environment </> "out.fish"
          planned <- planCombinedOutputBundle (OutputPath target) graph
          assertBool "planner rejected" (isRight planned)
          doesFileExist target >>= (@?= False),
      testCase "managed planning captures runtime bytes before provider removal" $
        withSources "x=value; echo \"$x\"\n" [] $ \root environment -> do
          installed <- findExecutable "monk-runtime" >>= maybe (assertFailure "monk-runtime build tool missing" >> fail "runtime missing") pure
          let provider = sourceWorkingDirectory environment </> "provider"
              target = sourceWorkingDirectory environment </> "out.fish"
          copyFile installed provider
          graph <- requireGraph (strictConfig {translationRuntime = RuntimePath provider}) environment root
          bundle <- requireSeparate target graph
          image <- case bundleRuntimeArtifacts bundle of
            [artifact] -> do
              runtimeArtifactMode artifact @?= 0o700
              pure (runtimeArtifactImage artifact)
            _ -> assertFailure "expected one owned native image" >> fail "native artifact missing"
          bytes <- readFileBS provider
          nativeImageBytes image @?= bytes
          nativeImageABI image @?= 1
          nativeImageProfile image @?= Bash53Signed64Fish46
          removeFile provider
          void (publishRequired bundle)
          forM_ (bundleRuntimeArtifacts bundle) $ \artifact -> case runtimeArtifactTarget artifact of
            OutputPath path -> readFileBS path >>= (@?= bytes)
            OutputStdout -> assertFailure "native image was assigned to stdout",
      testCase "combined planning pins a relative destination immediately" $
        withSources "printf 'value\\n'\n" [] $ \root environment -> do
          graph <- requireGraph strictConfig environment root
          workingDirectory <- getCurrentDirectory
          planned <- planCombinedOutputBundle (OutputPath "relative-planned-output.fish") graph
          case planned of
            Left diagnostic -> assertFailure (show diagnostic)
            Right bundle -> do
              map generatedTarget (toList (bundleUserFiles bundle)) @?= [OutputPath (workingDirectory </> "relative-planned-output.fish")]
              doesFileExist (workingDirectory </> "relative-planned-output.fish") >>= (@?= False),
      testCase "publication writes exactly the planned combined artifact" $
        withSources ". ./child.bash\n" [("child.bash", "printf 'value\\n'\n")] $ \root environment -> do
          graph <- requireGraph strictConfig environment root
          let target = sourceWorkingDirectory environment </> "out.fish"
          planned <- planCombinedOutputBundle (OutputPath target) graph
          case planned of
            Left diagnostic -> assertFailure (show diagnostic)
            Right bundle -> do
              published <- publishOutputBundle bundle
              receipt <- case published of
                Left failure -> assertFailure (show (outputFailureDiagnostic failure)) >> fail "publication failed"
                Right value -> pure value
              outputReceiptDestination receipt @?= target
              outputReceiptGeneration receipt @?= Nothing
              outputReceiptWarnings receipt @?= []
              text <- TIO.readFile target
              renderOutputBundle bundle @?= [(OutputPath target, text)]
              assertSourceEquivalent root environment graph [],
      testCase "immutable graph output does not reread changed dependencies" $
        withSources ". ./child.bash\n" [("child.bash", "printf 'before\\n'\n")] $ \root environment -> do
          graph <- requireGraph strictConfig environment root
          before <- renderGraph graph
          writeFileText (sourceWorkingDirectory environment </> "child.bash") "printf 'after\\n'\n"
          after <- renderGraph graph
          after @?= before,
      testCase "managed planning has no filesystem effects" $
        withSources ". ./child.bash\n" [("child.bash", "printf 'value\\n'\n")] $ \root environment -> do
          graph <- requireGraph strictConfig environment root
          let target = sourceWorkingDirectory environment </> "out.fish"
          bundle <- requireSeparate target graph
          assertBool "missing planned entry" (not (null (renderOutputBundle bundle)))
          forM_ (bundleUserFiles bundle) $ \file ->
            statisticsRenderedFishBytes (generatedStatistics file) @?= BS.length (encodeUtf8 (renderScript (generatedScript file)))
          doesFileExist target >>= (@?= False),
      testCaseSteps "managed repeated sources execute without original inputs" $ \step ->
        withSources ". ./child.bash first; . ./child.bash second\n" [("child.bash", "printf '<%s>\\n' \"$1\"; return 7\n")] $ \root environment -> do
          graph <- requireGraph strictConfig environment root
          let target = sourceWorkingDirectory environment </> "out.fish"
          bundle <- requireSeparate target graph
          receipt <- publishRequired bundle
          assertBool "missing immutable generation" (isJust (outputReceiptGeneration receipt))
          ready <- shouldRunIntegration
          case ready of
            Left reason -> step ("skipped runtime: " <> reason)
            Right () -> do
              bash <- runAt environment "bash" ["--noprofile", "--norc", root]
              removeFile root
              removeFile (sourceWorkingDirectory environment </> "child.bash")
              fish <- runAt environment "fish" ["--no-config", target]
              assertEqual "managed source stdout/stderr/status" bash fish,
      testCaseSteps "one managed source occurrence can execute repeatedly in a loop" $ \step ->
        withSources "for value in first second; do . ./child.bash \"$value\"; done\n" [("child.bash", "printf '<%s>\\n' \"$1\"; return 7\n")] $ \root environment -> do
          graph <- requireGraph strictConfig environment root
          let target = sourceWorkingDirectory environment </> "out.fish"
          bundle <- requireSeparate target graph
          void (publishRequired bundle)
          ready <- shouldRunIntegration
          case ready of
            Left reason -> step ("skipped runtime: " <> reason)
            Right () -> do
              bash <- runAt environment "bash" ["--noprofile", "--norc", root]
              fish <- runAt environment "fish" ["--no-config", target]
              assertEqual "repeated owned module invocation" bash fish,
      testCaseSteps "an older loader remains pinned after replacement" $ \step ->
        withSources ". ./child.bash\n" [("child.bash", "printf 'old\\n'\n")] $ \root environment -> do
          firstGraph <- requireGraph strictConfig environment root
          let target = sourceWorkingDirectory environment </> "out.fish"
              oldEntry = sourceWorkingDirectory environment </> "saved-loader.fish"
          firstBundle <- requireSeparate target firstGraph
          firstReceipt <- publishRequired firstBundle
          readFileBS target >>= writeFileBS oldEntry
          writeFileText (sourceWorkingDirectory environment </> "child.bash") "printf 'new\\n'\n"
          secondGraph <- requireGraph strictConfig environment root
          secondBundle <- requireSeparate target secondGraph
          secondReceipt <- publishRequired secondBundle
          assertBool "changed content reused an immutable generation" (outputReceiptGeneration firstReceipt /= outputReceiptGeneration secondReceipt)
          ready <- shouldRunIntegration
          case ready of
            Left reason -> step ("skipped runtime: " <> reason)
            Right () -> do
              oldResult <- runAt environment "fish" ["--no-config", oldEntry]
              newResult <- runAt environment "fish" ["--no-config", target]
              oldResult @?= (ExitSuccess, "old\n", "")
              newResult @?= (ExitSuccess, "new\n", ""),
      testCaseSteps "managed sourceable loader preserves caller scope status and argv" $ \step ->
        withSources ". ./child.bash \"$@\"\n" [("child.bash", "printf '%s:%s\\n' \"$?\" \"$1\"; x=changed; return 7\n")] $ \root environment -> do
          let contract = emptyCallerContract {callerAmbientEffects = NoRelevantAmbientEffects, callerVariables = M.singleton "x" (ScalarBinding InputOutputBinding VisibleBinding UnexportedBinding)}
              cfg = strictConfig {entryMode = Sourceable, callerContract = contract}
              target = sourceWorkingDirectory environment </> "out.fish"
          graph <- requireGraph cfg environment root
          bundle <- requireSeparate target graph
          void (publishRequired bundle)
          ready <- shouldRunIntegration
          case ready of
            Left reason -> step ("skipped runtime: " <> reason)
            Right () -> do
              let bashCaller = "caller() { local x=original; false; . \"$1\" alpha; printf 'after:%s:%s\\n' \"$?\" \"$x\"; }; caller \"$1\""
                  fishCaller = "function caller; set -l x original; builtin false; source \"$argv[1]\" alpha; builtin printf 'after:%s:%s\\n' \"$status\" \"$x\"; end; caller \"$argv[1]\""
              bash <- runAt environment "bash" ["--noprofile", "--norc", "-c", bashCaller, "caller.bash", root]
              fish <- runAt environment "fish" ["--no-config", "-c", fishCaller, target]
              assertEqual "managed caller stdout/stderr/status" bash fish,
      testCase "stdout bundles cannot be published as filesystem artifacts" $
        withSources "true\n" [] $ \root environment -> do
          graph <- requireGraph strictConfig environment root
          planned <- planCombinedOutputBundle OutputStdout graph
          case planned of
            Left diagnostic -> diagnosticPhase diagnostic @?= PhaseOutput
            Right bundle -> do
              published <- publishOutputBundle bundle
              failure <- case published of
                Left value -> pure value
                Right _ -> assertFailure "stdout bundle was published" >> fail "publication unexpectedly succeeded"
              outputFailureKind failure @?= OutputNoFilesystemPublication
              diagnosticPhase (outputFailureDiagnostic failure) @?= PhaseOutput
              outputFailureObservedEntry failure @?= Nothing
    ]

requireSeparate :: FilePath -> SourceGraph -> IO OutputBundle
requireSeparate target graph = planSeparateOutputBundle target graph >>= either (\diagnostic -> assertFailure (show diagnostic) >> fail "managed planning rejected") pure

publishRequired :: OutputBundle -> IO OutputReceipt
publishRequired bundle = publishOutputBundle bundle >>= either (\failure -> assertFailure (show (outputFailureDiagnostic failure)) >> fail "publication failed") pure

runAt :: SourceEnvironment -> FilePath -> [String] -> IO (ExitCode, String, String)
runAt environment command arguments = do
  variables <- prepareEnv
  readCreateProcessWithTimeout 5000000 (proc command arguments) {cwd = Just (sourceWorkingDirectory environment), env = Just variables} ""

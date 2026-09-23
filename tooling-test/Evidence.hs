module Evidence (tests) where

import Control.Exception (bracket, try)
import Data.Aeson (Value (..), encode, object, toJSON, (.=))
import Data.Bits ((.&.))
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.ByteString.Lazy qualified as BL
import Data.List (isInfixOf, lookup)
import Data.Text qualified as T
import Monk.Tooling.Evidence.Babelfish (common16Timing)
import Monk.Tooling.Evidence.Common (Observation (..), arrayField, base64, compareEffects, compareObservations, copyTree, field, readJson, runObservation, snapshot, unbase64)
import Monk.Tooling.Evidence.Comparison (runComparison)
import Monk.Tooling.Evidence.Native (comparisonCorpusPath, nativeExecute, nativeSame, readComparisonCorpus, validateCommon16, validateHistoric95)
import Monk.Tooling.Evidence.Performance (aggregate, freezePerformance)
import Monk.Tooling.Evidence.Portable (freezePortable, insertFields, portableExecute, portableTranslate, strengthenedCases)
import Monk.Tooling.Evidence.Profile (runProfile)
import Monk.Tooling.Evidence.Trace (parseTrace, runTrace)
import Monk.Tooling.Evidence.Verification (productionIdentity, sourceIdentity)
import System.Directory (copyFile, copyFileWithMetadata, createDirectory, createDirectoryIfMissing, doesFileExist, findExecutable, getCurrentDirectory, getTemporaryDirectory, listDirectory, removeDirectoryRecursive, removeFile)
import System.Environment (getEnvironment, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.IO (hClose, openTempFile)
import System.Posix.Files (createSymbolicLink, fileMode, getFileStatus, setFileMode)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "evidence"
    [ testCase "base64 preserves NUL and invalid UTF-8" $ do
        let input = B.pack [0, 255, 10]
        base64 input @?= "AP8K"
        unbase64 "AP8K" @?= Right input,
      testCase "Rust sources and toolchain metadata enter production identity but target output does not" $ withScratch $ \root -> do
        forM_ ["runtime/src", ".cargo", "protocol", "target/debug"] $ \directory -> createDirectoryIfMissing True (root <> "/" <> directory)
        forM_ ["Cargo.toml", "Cargo.lock", "rust-toolchain.toml", "runtime/Cargo.toml", "runtime/src/lib.rs", ".cargo/config.toml", "protocol/abi2.tsv"] $ \path -> B.writeFile (root <> "/" <> path) "initial"
        initial <- sourceIdentity root
        initialProduction <- either fail pure (productionIdentity initial)
        rows <- either fail pure (arrayField "files" initialProduction)
        let paths = [path | row <- rows, Right (String path) <- [field "path" row]]
        forM_ ["Cargo.toml", "Cargo.lock", "rust-toolchain.toml", "runtime/Cargo.toml", "runtime/src/lib.rs", ".cargo/config.toml", "protocol/abi2.tsv"] $ \path ->
          assertBool ("missing Rust input from production identity: " <> path) (T.pack path `elem` paths)
        B.writeFile (root <> "/target/debug/monk-runtime") "generated"
        afterTarget <- sourceIdentity root
        field "sha256" afterTarget @?= field "sha256" initial
        B.writeFile (root <> "/runtime/src/lib.rs") "changed"
        afterSource <- sourceIdentity root
        assertBool "Rust source edit did not change evidence identity" (field "sha256" afterSource /= field "sha256" initial),
      testCase "nonzero matching exit is a match and timeout is unavailable" $ do
        let baseline = Observation "completed" 7 "" "error\n" 1
            timeout = baseline {status = "timeout"}
        field "status" (compareObservations baseline baseline) @?= Right (String "match")
        field "status" (compareObservations baseline timeout) @?= Right (String "unavailable"),
      testCase "filesystem modes can turn equal streams into a mismatch" $ do
        let observed = Observation "completed" 0 "" "" 1
        compareEffects observed observed (Just (object ["mode" .= (0o640 :: Int)])) (Just (object ["mode" .= (0o600 :: Int)]))
          @?= object ["status" .= ("mismatch" :: Text), "differences" .= (["filesystem"] :: [Text])],
      testCase "both translators may disagree with Bash despite agreeing with each other" $ do
        let baseline = Observation "completed" 0 "right\n" "" 1
            candidate = baseline {observedStdout = "wrong\n"}
        forM_ (["monk", "babelfish"] :: [Text]) $ \_ ->
          compareObservations baseline candidate @?= object ["status" .= ("mismatch" :: Text), "differences" .= (["stdout"] :: [Text])],
      testCase "snapshot keeps bytes, mode and link target" $ do
        temp <- getTemporaryDirectory
        (path, handle) <- openTempFile temp "monk-evidence-test-"
        hClose handle
        removeFile path
        createDirectory path
        B.writeFile (path <> "/data") (B.pack [0, 255, 10])
        setFileMode (path <> "/data") 0o640
        createSymbolicLink "data" (path <> "/link")
        tree <- snapshot path
        case field "data" tree of
          Right row -> do
            field "bytes_base64" row @?= Right (String "AP8K")
            field "mode" row @?= Right (Number 416)
          _ -> assertBool "missing file snapshot" False
        case field "link" tree of
          Right row -> field "target" row @?= Right (String "data")
          _ -> assertBool "missing link snapshot" False
        removeDirectoryRecursive path,
      testCase "strengthened Bash fixtures expose exact byte effects" $ withScratch $ \root -> do
        let expected =
              [ ("read-array-live-ifs", ("status:0\nfield:<alpha>\nfield:<beta>\nfield:<>\nfield:<gamma>\n", "")),
                ("read-eof-without-newline", ("status:1\nvalue:<partial value>\n", "")),
                ("read-nul-invalid-bytes", (B.pack [115, 116, 97, 116, 117, 115, 58, 48, 10, 118, 97, 108, 117, 101, 58, 60, 97, 255, 98, 62, 10], "")),
                ("dense-array-append-observed", ("<first item>\n<>\n<third>\n<last item>\n<*>\n", "")),
                ("visible-time-output", ("timed", "measured\n"))
              ]
        forM_ strengthenedCases $ \(name, source, input) -> do
          observed <- runObservation ["bash", "--noprofile", "--norc", "-c", source] input root [("LC_ALL", "C"), ("LANG", "C"), ("PATH", "/usr/bin:/bin")] 5
          case lookup name expected of
            Just (out, err) -> do
              exit observed @?= 0
              observedStdout observed @?= out
              observedStderr observed @?= err
            Nothing -> assertBool ("unexpected strengthened case " <> name) False,
      testCase "portable freeze preserves the historic denominator and source digests" $ withScratch $ \root -> do
        repo <- getCurrentDirectory
        summary <- freezePortable repo (root <> "/frozen") "stable"
        field "historic" summary @?= Right (Number 95)
        manifest <- readJson (root <> "/frozen/manifest.json")
        field "historic_denominator" manifest @?= Right (Number 95)
        rows <- either fail pure (field "fixtures" manifest)
        case rows of
          Array values -> do
            length values @?= 104
            length [() | row <- toList values, field "cohort" row == Right (String "strengthened")] @?= 5
          _ -> assertBool "missing fixture rows" False,
      testCase "frozen95 rejects changed identities, metadata, input and order" $ do
        repo <- getCurrentDirectory
        corpus <- readComparisonCorpus repo
        historical <- either fail pure (arrayField "historic95" corpus)
        common <- either fail pure (arrayField "common16" corpus)
        validateHistoric95 repo historical
        validateCommon16 repo common
        case historical of
          initialRow : nextRow : rest -> do
            let variants =
                  [ insertFields initialRow ["metadata" .= object ["fixtureMetaArgs" .= (["changed"] :: [Text])]] : nextRow : rest,
                    insertFields initialRow ["input_sha256" .= ("changed" :: Text)] : nextRow : rest,
                    insertFields initialRow ["stdin_base64" .= ("Y2hhbmdlZA==" :: Text)] : nextRow : rest,
                    nextRow : initialRow : rest,
                    initialRow : initialRow : rest,
                    nextRow : rest
                  ]
            forM_ variants $ \changed -> do
              outcome <- try (validateHistoric95 repo changed) :: IO (Either SomeException ())
              assertBool "changed historic corpus accepted" (either (const True) (const False) outcome)
          _ -> assertBool "missing historical rows" False
        forM_ [take 15 common, reverse common, take 1 common <> take 15 common] $ \changed -> do
          outcome <- try (validateCommon16 repo changed) :: IO (Either SomeException ())
          assertBool "changed common16 cohort accepted" (either (const True) (const False) outcome),
      testCase "portable freeze rejects altered canonical execution metadata" $ withScratch $ \root -> do
        repo <- getCurrentDirectory
        forM_ ["test/fixtures", "benchmark/fixtures", "test/evidence"] $ \folder -> do
          createDirectoryIfMissing True (root <> "/" <> takeWhile (/= '/') folder)
          copyTree (repo <> "/" <> folder) (root <> "/" <> folder)
        original <- readComparisonCorpus repo
        historical <- either fail pure (arrayField "historic95" original)
        case historical of
          initialRow : rest -> do
            let changed = insertFields initialRow ["metadata" .= object ["fixtureMetaArgs" .= (["changed"] :: [Text])]] : rest
                altered = insertFields original ["historic95" .= changed]
            BL.writeFile (root <> "/" <> comparisonCorpusPath) (encode altered)
          [] -> fail "missing historic fixtures"
        outcome <- try (freezePortable root (root <> "/frozen") "default") :: IO (Either SomeException Value)
        assertBool "portable freeze accepted changed canonical metadata" (either (const True) (const False) outcome)
        reportExists <- doesFileExist (root <> "/frozen/manifest.json")
        assertBool "altered cohort published a manifest" (not reportExists),
      testCase "comparison and performance freezes need no retired evidence reports" $ withScratch $ \root -> do
        repo <- getCurrentDirectory
        forM_ ["test/fixtures", "benchmark/fixtures", "test/evidence"] $ \folder -> do
          createDirectoryIfMissing True (root <> "/" <> takeWhile (/= '/') folder)
          copyTree (repo <> "/" <> folder) (root <> "/" <> folder)
        _ <- freezePortable root (root <> "/frozen") "default"
        manifest <- readJson (root <> "/frozen/manifest.json")
        field "schema" manifest @?= Right (Number 2)
        source <- B.readFile (root <> "/test/evidence/frozen95/background-jobs.bash")
        frozenSource <- B.readFile (root <> "/frozen/inputs/test/fixtures/integration/background-jobs.bash")
        frozenSource @?= source
        freezePerformance (root <> "/frozen") (root <> "/performance")
        performance <- readJson (root <> "/performance/manifest.json")
        fixtures <- either fail pure (arrayField "fixtures" performance)
        let arithmetic = [row | row <- fixtures, field "performance_cohort" row == Right (String "arithmetic3")]
        length arithmetic @?= 3
        forM_ arithmetic $ \row -> assertBool "missing arithmetic source accepted" (isRight (field "unavailable_reason" row)),
      testCase "Babelfish timing requires every common fixture and twenty numeric samples" $ do
        let common = [String (T.pack ("fixture-" <> show number)) | number <- [1 :: Int .. 16]]
            row name samples = object ["fixture" .= name, "modes" .= object ["default" .= object ["samples_ns" .= samples]]]
            complete = [row name (replicate 20 (100 :: Int)) | name <- common]
        case common16Timing common complete of
          Right timing -> do
            field "samples" timing @?= Right (Number 20)
            field "sample_totals_ns" timing @?= Right (toJSON (replicate 20 (1600 :: Int)))
          Left reason -> fail reason
        assertBool "missing common fixture accepted" (either (const True) (const False) (common16Timing common (take 15 complete)))
        case complete of
          initialRow : rest -> do
            let short = insertFields initialRow ["modes" .= object ["default" .= object ["samples_ns" .= replicate 19 (100 :: Int)]]] : rest
                malformed = insertFields initialRow ["modes" .= object ["default" .= object ["samples_ns" .= replicate 20 ("100" :: Text)]]] : rest
            assertBool "short timing row accepted" (either (const True) (const False) (common16Timing common short))
            assertBool "nonnumeric timing row accepted" (either (const True) (const False) (common16Timing common malformed))
          [] -> fail "missing timing fixtures",
      testCase "candidate launcher preserves empty and spaced script arguments" $ withScratch $ \root -> do
        let inputs = root <> "/inputs"
            launcher = root <> "/candidate-runtime"
            fixture = object ["cohort" .= ("historic95" :: Text), "fixture" .= ("script.fish" :: Text), "metadata" .= object ["fixtureMetaArgs" .= (["", "two words"] :: [Text])], "stdin_base64" .= ("" :: Text)]
        createDirectory inputs
        B.writeFile (inputs <> "/script.fish") ""
        B.writeFile launcher "#!/bin/sh\nprintf '<%s>' \"$@\"\n"
        setFileMode launcher 0o700
        (observed, command, _) <- portableExecute "/bin/sh" True (Just launcher) fixture (inputs <> "/script.fish") inputs (root <> "/cwd") [("PATH", "/usr/bin:/bin")] 5
        command @?= [launcher, "--abi", "2", "launch", inputs <> "/script.fish", "", "two words"]
        observedStdout observed @?= C.pack ("<--abi><2><launch><" <> inputs <> "/script.fish><><two words>"),
      testCase "sourceable caller observes Fish directly" $ withScratch $ \root -> do
        let inputs = root <> "/inputs"
            source = inputs <> "/script.fish"
            fixture = object ["cohort" .= ("caller" :: Text), "fixture" .= ("script.fish" :: Text), "metadata" .= object ["fixtureMetaArgs" .= ([] :: [Text])], "stdin_base64" .= ("" :: Text)]
        createDirectory inputs
        B.writeFile source "function f\n  echo function\nend\n"
        (observed, command, _) <- portableExecute "fish" True (Just (root <> "/unused-launcher")) fixture source inputs (root <> "/cwd") [("PATH", "/usr/bin:/bin"), ("LC_ALL", "C")] 5
        take 3 command @?= ["fish", "--no-config", "-c"]
        case drop 3 command of
          script : _ -> assertBool "caller source command missing" ("source \"$argv[1]\"" `isInfixOf` script)
          _ -> assertBool "caller command missing" False
        exit observed @?= 0
        observedStdout observed @?= "function\ncaller:caller:unchanged:0\n",
      testCase "stable directory flag applies to current and candidate translators only" $ withScratch $ \root -> do
        let inputs = root <> "/inputs"
            source = inputs <> "/script.bash"
            translator = root <> "/translator"
            fixture = object ["cohort" .= ("historic95" :: Text), "fixture" .= ("script.bash" :: Text), "metadata" .= object ["fixtureMetaRecursive" .= False]]
        createDirectory inputs
        B.writeFile source "pwd\n"
        B.writeFile translator "#!/bin/sh\nprintf '%s\\n' \"$@\"\n"
        setFileMode translator 0o700
        forM_ ["default", "stable"] $ \contract -> forM_ ["original", "current", "candidate", "babelfish"] $ \tool -> do
          (_, command) <- portableTranslate tool translator "/runtime" fixture inputs (root <> "/generated.fish") [("PATH", "/usr/bin:/bin")] 5 contract
          ("--directory-contract" `elem` command) @?= (contract == "stable" && tool `elem` ["current", "candidate"]),
      testCase "incomplete performance cohorts never pass" $ do
        let measured = object ["fixture" .= ("a" :: Text), "status" .= ("measured" :: Text), "samples_ns" .= object ["baseline" .= replicate 20 (100 :: Int), "candidate" .= replicate 20 (1 :: Int)]]
            missing = object ["fixture" .= ("b" :: Text), "status" .= ("unsupported-native-platform" :: Text)]
        field "status" (aggregate [measured, missing]) @?= Right (String "unverified")
        field "status" (aggregate []) @?= Right (String "unverified")
        forM_ (["missing-frozen-input", "sample-mismatch", "translation-unavailable"] :: [Text]) $ \failure ->
          field "status" (aggregate [object ["fixture" .= ("a" :: Text), "status" .= failure]]) @?= Right (String "unverified"),
      testCase "twenty-sample regression gate rejects 11 percent" $ do
        let measured = object ["fixture" .= ("a" :: Text), "status" .= ("measured" :: Text), "samples_ns" .= object ["baseline" .= replicate 20 (100 :: Int), "candidate" .= replicate 20 (111 :: Int)]]
        field "at_most_10_percent_regression" (aggregate [measured]) @?= Right (Bool False),
      testCase "nonnumeric performance samples remain unverified" $ do
        let malformed = object ["fixture" .= ("a" :: Text), "status" .= ("measured" :: Text), "samples_ns" .= object ["baseline" .= replicate 20 ("100" :: Text), "candidate" .= replicate 20 (1 :: Int)]]
        field "status" (aggregate [malformed]) @?= Right (String "unverified"),
      testCase "bakeoff comparison reads a top-level fixture array" $ do
        temp <- getTemporaryDirectory
        (path, handle) <- openTempFile temp "monk-bakeoff-list-"
        hClose handle
        removeFile path
        createDirectory path
        let fixture = path <> "/example.bash"
            provider = path <> "/version-tool"
            report =
              [ object
                  [ "fixtureReportPath" .= fixture,
                    "fixtureReportRelativePath" .= ("example.bash" :: Text),
                    "fixtureReportGroup" .= ("example" :: Text),
                    "fixtureReportMetadata" .= object ["fixtureMetaMode" .= ("ShellRunExec" :: Text)],
                    "fixtureReportMonkTranslation" .= Null,
                    "fixtureReportBabelfishTranslation" .= Null
                  ]
              ]
        B.writeFile fixture "echo test\n"
        B.writeFile provider "#!/bin/sh\necho provider 1\n"
        setFileMode provider 0o700
        BL.writeFile (path <> "/report.json") (encode report)
        BL.writeFile (path <> "/meta.json") (encode (object ["metaCwd" .= path]))
        totals <- runComparison path provider provider 5
        (field "monk" totals >>= field "MissingReport") @?= Right (Number 1)
        removeDirectoryRecursive path,
      testCase "native measurement keeps argv, stdin, raw streams and status" $ do
        temp <- getTemporaryDirectory
        let environment = [("PATH", "/usr/bin:/bin")]
        result <- nativeExecute ["/bin/sh", "-c", "printf '<%s>' \"$1\"; cat; printf '\\377\\000' >&2; exit 7", "fixture", "a b"] temp (B.pack [0, 255]) environment
        exit result @?= 7
        observedStdout result @?= B.pack [60, 97, 32, 98, 62, 0, 255]
        observedStderr result @?= B.pack [255, 0]
        assertBool "elapsed time missing" (elapsedNs result > 0)
        assertBool "elapsed time changed stream equivalence" (nativeSame result result {elapsedNs = elapsedNs result + 1})
        forM_ [result {observedStdout = "different"}, result {observedStderr = "different"}, result {exit = 0}] $ \changed ->
          assertBool "changed stream or status was treated as same" (not (nativeSame result changed)),
      testCase "reference profile includes OS release, executable hashes and byte probes" $ do
        profile <- runProfile "bash" "fish"
        case field "platform" profile of
          Right (String platform) -> assertBool "missing OS release" (length (T.words platform) >= 3)
          _ -> assertBool "missing platform" False
        executables <- either fail pure (field "executables" profile)
        bash <- either fail pure (field "bash" executables)
        assertBool "missing Bash digest" (field "sha256" bash /= Left "missing JSON key: sha256")
        probes <- either fail pure (field "bash_probes" profile)
        case field "a\\uD800b" probes of
          Right probe -> assertBool "missing probe byte stream" (field "stdout_base64" probe /= Left "missing JSON key: stdout_base64")
          _ -> assertBool "missing Unicode probe" False,
      testCase "verification detects test and document changes without changing product identity" $ do
        forM_ [("test/Spec.hs", "test"), ("docs/input.md", "document")] $ \(name, label) -> withScratch $ \root -> do
          createDirectoryIfMissing True (root <> "/" <> takeWhile (/= '/') name)
          B.writeFile (root <> "/" <> name) "before"
          (result, receipt) <- collectReceipt root [] ("printf after > " <> name)
          result @?= ExitFailure 3
          field "build_inputs_unchanged" receipt @?= Right (Bool False)
          field "production_inputs_unchanged" receipt @?= Right (Bool True)
          field "successful_stable_command" receipt @?= Right (Bool False)
          assertBool (label <> " command exit not recorded") (field "exit" receipt == Right (Number 0)),
      testCase "verification detects production changes" $ withScratch $ \root -> do
        createDirectory (root <> "/src")
        B.writeFile (root <> "/src/Core.hs") "before"
        (result, receipt) <- collectReceipt root [] "printf after > src/Core.hs"
        result @?= ExitFailure 3
        field "production_inputs_unchanged" receipt @?= Right (Bool False),
      testCase "verification detects added and removed build inputs" $ withScratch $ \root -> do
        createDirectory (root <> "/test")
        B.writeFile (root <> "/test/old.stdin") "old"
        (result, receipt) <- collectReceipt root [] "rm test/old.stdin; printf new > test/new.stdin"
        result @?= ExitFailure 3
        field "build_inputs_unchanged" receipt @?= Right (Bool False),
      testCase "verification ignores generated caches" $ withScratch $ \root -> do
        (result, receipt) <- collectReceipt root [] "mkdir -p scripts/__pycache__ test/dist-local; printf cache > scripts/__pycache__/module.pyc; printf interface > test/dist-local/Core.hi"
        result @?= ExitSuccess
        field "successful_stable_command" receipt @?= Right (Bool True),
      testCase "verification records failed command and missing binary" $ withScratch $ \root -> do
        (result, receipt) <- collectReceipt root [root <> "/missing"] "exit 7"
        result @?= ExitFailure 7
        field "exit" receipt @?= Right (Number 7)
        field "requested_binaries_present" receipt @?= Right (Bool False)
        case field "binaries" receipt of
          Right (Array values) -> case toList values of
            firstBinary : _ -> field "missing" firstBinary @?= Right (Bool True)
            _ -> assertBool "missing binary receipt" False
          _ -> assertBool "missing binaries" False,
      testCase "trace counts successful execs and child processes only" $ do
        let traceOutput =
              T.unlines
                [ "10 execve(\"/fish\", [\"/fish\"], 0x0) = 0",
                  "10 clone(child_stack=NULL, flags=CLONE_VM|CLONE_VFORK|SIGCHLD <unfinished ...>",
                  "11 execve(\"/missing\", [], 0x0) = -1 ENOENT (No such file or directory)",
                  "10 <... clone resumed>) = 11",
                  "11 execve(\"/runtime\", [\"/runtime\"], 0x0 <unfinished ...>",
                  "11 <... execve resumed>) = 0",
                  "11 clone3({flags=CLONE_VM|CLONE_THREAD}, 88) = 12"
                ]
            parsed = parseTrace traceOutput
        field "successful_exec_count_including_entry_shell" parsed @?= Right (Number 2)
        field "child_process_creation_count_excluding_threads" parsed @?= Right (Number 1),
      testCase "failed trace command cannot publish partial counts" $ withScratch $ \root -> do
        let binDirectory = root <> "/bin"
            traceExecutable = binDirectory <> "/strace"
            generatedDirectory = root <> "/baseline"
            generated = generatedDirectory <> "/0.fish"
            cohort =
              object
                [ "cwd" .= root,
                  "fixtures" .= [object ["fixture" .= ("example.bash" :: Text), "index" .= (0 :: Int), "metadata" .= object ["fixtureMetaArgs" .= ([] :: [Text])], "stdin_base64" .= ("" :: Text)]],
                  "common16" .= (["example.bash"] :: [Text])
                ]
        createDirectory binDirectory
        createDirectory generatedDirectory
        B.writeFile generated "exit 7\n"
        B.writeFile traceExecutable "#!/bin/sh\nwhile [ \"$1\" != -o ]; do shift; done\nshift; log=$1; shift\nprintf '1 execve(\"%s\", [], 0x0) = 0\\n' \"$1\" > \"$log\"\nexit 23\n"
        setFileMode traceExecutable 0o700
        BL.writeFile (root <> "/baseline-cohort.json") (encode cohort)
        fish <- findExecutable "fish" >>= maybe (fail "fish executable missing") pure
        oldPath <- lookupEnv "PATH"
        let restore = maybe (unsetEnv "PATH") (setEnv "PATH") oldPath
        bracket (setEnv "PATH" (binDirectory <> ":" <> fromMaybe "" oldPath)) (const restore) $ \_ -> do
          outcome <- try (runTrace root fish "baseline" Nothing) :: IO (Either SomeException Value)
          assertBool "nonzero trace succeeded" (either (const True) (const False) outcome)
        reportExists <- doesFileExist (root <> "/process-traces-baseline/report.json")
        assertBool "failed trace published report" (not reportExists),
      testCase "trace accepts a valid child exit of seven" $ withScratch $ \root -> do
        let binDirectory = root <> "/bin"
            traceExecutable = binDirectory <> "/strace"
            generatedDirectory = root <> "/baseline"
            cohort =
              object
                [ "cwd" .= root,
                  "fixtures" .= [object ["fixture" .= ("example.bash" :: Text), "index" .= (0 :: Int), "metadata" .= object ["fixtureMetaArgs" .= ([] :: [Text])], "stdin_base64" .= ("" :: Text)]],
                  "common16" .= (["example.bash"] :: [Text])
                ]
        createDirectory binDirectory
        createDirectory generatedDirectory
        B.writeFile (generatedDirectory <> "/0.fish") "exit 7\n"
        B.writeFile traceExecutable "#!/bin/sh\nwhile [ \"$1\" != -o ]; do shift; done\nshift; log=$1; shift\nprintf '1 execve(\"%s\", [], 0x0) = 0\\n' \"$1\" > \"$log\"\nexec \"$@\"\n"
        setFileMode traceExecutable 0o700
        BL.writeFile (root <> "/baseline-cohort.json") (encode cohort)
        fish <- findExecutable "fish" >>= maybe (fail "fish executable missing") pure
        oldPath <- lookupEnv "PATH"
        let restore = maybe (unsetEnv "PATH") (setEnv "PATH") oldPath
        bracket (setEnv "PATH" (binDirectory <> ":" <> fromMaybe "" oldPath)) (const restore) $ \_ -> do
          result <- runTrace root fish "baseline" Nothing
          field "fixtures" result @?= Right (Number 1)
        reportExists <- doesFileExist (root <> "/process-traces-baseline/report.json")
        assertBool "valid nonzero trace omitted report" reportExists,
      testCase "portable directory contract typos are rejected before freeze" $ withScratch $ \root -> do
        let output = root <> "/frozen"
        (result, _, _) <- readCreateProcessWithExitCode (proc "monk-tool" ["evidence", "portable-comparison", "freeze", "--repo", ".", "--output", output, "--directory-contract", "stabel"]) ""
        assertBool "invalid contract was admitted" (result /= ExitSuccess)
        exists <- doesFileExist (output <> "/manifest.json")
        assertBool "invalid contract wrote a manifest" (not exists),
      publicationTests
    ]

withScratch :: (FilePath -> IO a) -> IO a
withScratch action = do
  temporary <- getTemporaryDirectory
  (path, handle) <- openTempFile temporary "monk-evidence-scratch-"
  hClose handle
  removeFile path
  bracket (createDirectory path >> pure path) removeDirectoryRecursive action

collectReceipt :: FilePath -> [FilePath] -> String -> IO (ExitCode, Value)
collectReceipt root binaries shellCommand = do
  let binaryOptions = concatMap (\path -> ["--binary", path]) binaries
      arguments = ["evidence", "verification", "--output", root <> "/receipt"] <> binaryOptions <> ["--", "/bin/sh", "-c", shellCommand]
  (status, _, errors) <- readCreateProcessWithExitCode (proc "monk-tool" arguments) {cwd = Just root} ""
  receipt <- readJson (root <> "/receipt/receipt.json")
  unless (null errors) $ assertBool errors True
  pure (status, receipt)

publicationTests :: TestTree
publicationTests =
  testGroup
    "native publication"
    [ testCase "literal output declares exact writer and executes" $ withNative $ \root monk runtime _fish env -> do
        target <- translateNative root monk runtime env "entry" "echo hello\n" []
        generated <- B.readFile target
        assertBool "generated Python dependency" (not ("python" `B.isInfixOf` generated))
        assertBool "missing exact writer" ("write-builtin" `B.isInfixOf` generated)
        observed <- runObservation [runtime, "--abi", "2", "launch", target] "" root env 20
        streams observed 0 "hello\n" "",
      testCase "silent literal assignment has no body helper" $ withNative $ \root monk runtime _fish env -> do
        target <- translateNative root monk runtime env "entry" "literal_control=hello\n" []
        generated <- B.readFile target
        assertBool "body helper generated" (not ("function __monk_" `B.isInfixOf` generated))
        assertBool "writer generated" (not ("write-builtin" `B.isInfixOf` generated))
        observed <- runObservation [runtime, "--abi", "2", "launch", target] "" root env 20
        streams observed 0 "" "",
      testCase "managed runtime runs with isolated PATH and executable mode" $ withNative $ \root monk runtime fish env -> do
        target <- translateNative root monk runtime env "entry" "x=21; echo \"$x\"\n" ["--managed"]
        managed <- managedRuntime root "entry"
        mode <- fileMode <$> getFileStatus managed
        mode .&. 0o7777 @?= 0o700
        createDirectory (root <> "/path")
        createSymbolicLink fish (root <> "/path/fish")
        let isolated = ("PATH", root <> "/path") : filter ((/= "PATH") . fst) env
        observed <- runObservation [managed, "--abi", "2", "launch", target] "" root isolated 20
        streams observed 0 "21\n" "",
      testCase "missing installed runtime prevents body effects" $ withNative $ \root monk _runtime fish env -> do
        target <- translateNative root monk (root <> "/missing-runtime") env "entry" "x=value; printf side-effect; echo \"$x\"\n" []
        observed <- runObservation [fish, "--no-config", target] "" root env 20
        exit observed @?= 125
        observedStdout observed @?= ""
        assertBool "missing runtime diagnostic" ("monk.runtime:" `B.isInfixOf` observedStderr observed),
      testCase "GHCRTS cannot change native operation streams" $ withNative $ \root _monk runtime _fish env -> do
        let flagged = ("GHCRTS", "-s") : filter ((/= "GHCRTS") . fst) env
        observed <- runObservation [runtime, "--abi", "2", "echo"] "hello\0" root flagged 20
        streams observed 0 "hello\n" "",
      testCase "runtime provider path preserves terminal newline" $ withNative $ \root monk runtime _fish env -> do
        let provider = root <> "/provider\n"
        copyFileWithMetadata runtime provider
        target <- translateNative root monk provider env "entry" "x=value; echo \"$x\"\n" []
        observed <- runObservation [provider, "--abi", "2", "launch", target] "" root env 20
        streams observed 0 "value\n" "",
      testCase "incompatible installed runtime prevents body effects" $ withNative $ \root monk _runtime fish env -> do
        let provider = root <> "/incompatible-runtime"
        B.writeFile provider "#!/bin/sh\nprintf 'monk-runtime 2 bash53-i64-linux64\\necho\\n'\n"
        setFileMode provider 0o700
        target <- translateNative root monk provider env "entry" "printf side-effect; x=value; echo \"$x\"\n" []
        observed <- runObservation [fish, "--no-config", target] "" root env 20
        exit observed @?= 125
        observedStdout observed @?= ""
        assertBool "incompatible runtime diagnostic" ("monk.runtime:" `B.isInfixOf` observedStderr observed),
      testCase "reuse rejects changed bytes without executing them" $ withNative $ \root monk runtime _fish env -> do
        target <- translateNative root monk runtime env "entry" "x=value; echo \"$x\"\n" ["--managed"]
        original <- B.readFile target
        managed <- managedRuntime root "entry"
        let marker = root <> "/unexpected-execution"
        B.writeFile managed (C.pack ("#!/bin/sh\nprintf reached > '" <> marker <> "'\nprintf 'monk-runtime 1 bash53-i64-linux64\\necho\\n'\n"))
        failed <- runObservation [monk, root <> "/entry.bash", "--strict", "--managed", "--runtime", runtime, "-o", target] "" root env 20
        assertBool "corrupt generation reused" (exit failed /= 0)
        B.readFile target >>= (@?= original)
        exists <- doesFileExist marker
        assertBool "corrupt member executed" (not exists),
      testCase "reuse rejects executable-mode corruption" $ withNative $ \root monk runtime _fish env -> do
        target <- translateNative root monk runtime env "entry" "x=21; echo \"$x\"\n" ["--managed"]
        original <- B.readFile target
        managed <- managedRuntime root "entry"
        setFileMode managed 0o600
        failed <- runObservation [monk, root <> "/entry.bash", "--strict", "--managed", "--runtime", runtime, "-o", target] "" root env 20
        assertBool "mode corruption reused" (exit failed /= 0)
        assertBool "missing mode diagnostic" ("mode differs" `B.isInfixOf` observedStderr failed)
        B.readFile target >>= (@?= original),
      testCase "exported function retains its runtime generation" $ withNative $ \root monk runtime fish env -> do
        let contract = root <> "/caller.json"
            options = ["--managed", "--entry", "sourceable", "--caller-contract", contract]
        B.writeFile contract "{\"version\":1,\"exportedFunctions\":[\"f\"],\"ambientEffects\":\"none\"}"
        firstTarget <- translateNative root monk runtime env "entry" "f() { local n=21; echo \"$n\"; }\n" options
        copyFile firstTarget (root <> "/saved.fish")
        secondTarget <- translateNative root monk runtime env "entry" "f() { local n=33; echo \"$n\"; }\n" options
        let driver = root <> "/driver.fish"
        B.writeFile driver (C.pack ("source '" <> root <> "/saved.fish'\nfunctions --copy f old_f\nsource '" <> secondTarget <> "'\nbuiltin cd /\nbuiltin set --global PATH\nold_f\nf\n"))
        observed <- runObservation [fish, "--no-config", driver] "" root env 20
        streams observed 0 "21\n33\n" ""
    ]

withNative :: (FilePath -> FilePath -> FilePath -> FilePath -> [(String, String)] -> IO a) -> IO a
withNative action = withScratch $ \root -> do
  monk <- findExecutable "monk" >>= maybe (fail "monk executable missing from test PATH") pure
  runtime <- findExecutable "monk-runtime" >>= maybe (fail "monk-runtime executable missing from test PATH") pure
  fish <- findExecutable "fish" >>= maybe (fail "fish executable missing from test PATH") pure
  inherited <- getEnvironment
  let env = [("LC_ALL", "C"), ("LANG", "C")] <> filter (\(key, _) -> key `notElem` ["LC_ALL", "LANG"]) inherited
  action root monk runtime fish env

translateNative :: FilePath -> FilePath -> FilePath -> [(String, String)] -> String -> B.ByteString -> [String] -> IO FilePath
translateNative root monk runtime env name source options = do
  let input = root <> "/" <> name <> ".bash"
      target = root <> "/" <> name <> ".fish"
  B.writeFile input source
  observed <- runObservation ([monk, input, "--strict", "--runtime", runtime, "-o", target] <> options) "" root env 20
  assertBool ("translator rejected source: " <> C.unpack (observedStderr observed)) (status observed == "completed" && exit observed == 0)
  pure target

managedRuntime :: FilePath -> String -> IO FilePath
managedRuntime root name = do
  let generations = root <> "/." <> name <> ".fish.monk/generations"
  names <- listDirectory generations
  case names of
    [generation] -> pure (generations <> "/" <> generation <> "/bin/monk-runtime")
    _ -> fail ("expected one managed runtime generation, found " <> show names)

streams :: Observation -> Int -> B.ByteString -> B.ByteString -> IO ()
streams observed expectedExit expectedOut expectedErr = do
  exit observed @?= expectedExit
  observedStdout observed @?= expectedOut
  observedStderr observed @?= expectedErr

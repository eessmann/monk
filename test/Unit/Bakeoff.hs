{-# LANGUAGE OverloadedStrings #-}

module Unit.Bakeoff
  ( unitBakeoffTests,
  )
where

import Bakeoff.Artifacts (FixtureArtifacts (faMonkFish), fixtureArtifacts)
import Bakeoff.Benchmark (makeBenchmarkPlan, runtimeEntryHasValidSyntax)
import Bakeoff.Execution.Runtime (runRuntimeBenchmarkEntry)
import Bakeoff.Fixture (FixtureMetadata (..))
import Bakeoff.Report (renderSummaryMarkdown)
import Bakeoff.Selection
  ( fixtureArtifactDir,
    makeFixtureSelectionReport,
    resolveFixtureSelection,
    summarizeFixtureMetadata,
  )
import Bakeoff.Shell (ShellRunMode (..))
import Bakeoff.Tools
  ( ToolPreflightFailure (..),
    ToolPreflightIssue (..),
    renderToolPreflightFailure,
    toolPreflightWarnings,
  )
import Bakeoff.Types
import Control.Exception (bracket)
import Data.Aeson (eitherDecode, encode)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Monk.Translation (DirectoryContract (..), RuntimeSelection (..), TranslateConfig (..))
import Path
  ( Abs,
    Dir,
    File,
    Path,
    Rel,
    parseAbsDir,
    parseRelDir,
    parseRelFile,
    toFilePath,
    (</>),
  )
import Path.IO qualified as PathIO
import System.Directory
  ( createDirectory,
    createDirectoryIfMissing,
    doesDirectoryExist,
    findExecutable,
    removeDirectoryRecursive,
    removeFile,
  )
import System.Environment (getEnvironment)
import System.IO qualified as IO
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as H

unitBakeoffTests :: TestTree
unitBakeoffTests =
  testGroup
    "Bakeoff"
    [ H.testCase "benchmark plan serializes explicit runtime and stable directory contract" $ do
        path <- PathIO.resolveFile' "/tmp/pinned-runtime"
        let tools = MkResolvedTools path path path Nothing (MkToolVersion "unknown") (MkToolVersion "unknown") Nothing
            settings = MkBakeoffTranslationSettings (RuntimePath "/tmp/pinned-runtime") StableDirectoryContract
            cfg = (sampleBakeoffConfig (unsafeAbsDir "/tmp/") True) {bakeoffTranslationSettings = settings}
            plan = makeBenchmarkPlan cfg [] tools
        benchmarkCwd plan @?= bakeoffCwd cfg
        translationRuntime (bakeoffTranslateConfig settings) @?= RuntimePath "/tmp/pinned-runtime"
        directoryContract (bakeoffTranslateConfig settings) @?= StableDirectoryContract
        benchmarkTranslationSettings plan @?= settings
        (eitherDecode (encode plan) :: Either String BenchmarkPlan) @?= Right plan,
      H.testCase "benchmark settings preserve on-path and generation-owned runtimes" $
        forM_ [RuntimeOnPath, RuntimeGeneration "runtime/monk-runtime"] $ \runtime -> do
          let settings = MkBakeoffTranslationSettings runtime NoDirectoryContract
          (eitherDecode (encode settings) :: Either String BakeoffTranslationSettings) @?= Right settings
          translationRuntime (bakeoffTranslateConfig settings) @?= runtime,
      H.testCase "fixtureArtifactDir mirrors the fixture path under fixtures/" $ do
        relPath <- parseRelFile "test/fixtures/integration/source-recursive.bash"
        artifactDir <- fixtureArtifactDir relPath
        toFilePath artifactDir @?= "fixtures/test/fixtures/integration/source-recursive/",
      H.testCase "resolveFixtureSelection preserves explicit file selectors" $ do
        cwd <- PathIO.getCurrentDir
        fixturePath <- repoFile "test/fixtures/integration/source-recursive.bash"
        fixtures <- resolveFixtureSelection cwd [] [fixturePath] [] []
        case fixtures of
          [fixture] -> do
            specGroup fixture @?= FixtureGroupIntegration
            specSelectionSources fixture @?= [SelectionFile fixturePath]
            fmMode (specMetadata fixture) @?= ShellRunExec
          other -> H.assertFailure ("expected one selected fixture, got " <> show (length other)),
      H.testCase "resolveFixtureSelection resolves selector entries relative to the list file" $ do
        withTempDir "monk-bakeoff-selection" $ \tmpDir -> do
          fixtureDirRel <- parseRelDir "fixtures/"
          fixtureRel <- parseRelFile "fixtures/custom-script.bash"
          listDirRel <- parseRelDir "lists/nested/"
          listRel <- parseRelFile "lists/nested/fixtures.txt"
          let fixtureDir = tmpDir </> fixtureDirRel
              fixturePath = tmpDir </> fixtureRel
              listDir = tmpDir </> listDirRel
              listPath = tmpDir </> listRel
              selectorContents = "../../fixtures/custom-script.bash\n"
          createDirectoryIfMissing True (toFilePath fixtureDir)
          createDirectoryIfMissing True (toFilePath listDir)
          TIO.writeFile (toFilePath fixturePath) "#!/usr/bin/env bash\n"
          TIO.writeFile (toFilePath listPath) selectorContents
          fixtures <- resolveFixtureSelection tmpDir [] [] [listPath] []
          case fixtures of
            [fixture] -> do
              specPath fixture @?= fixturePath
              specSelectionSources fixture @?= [SelectionFileList listPath]
            other -> H.assertFailure ("expected one selected fixture, got " <> show (length other)),
      H.testCase "resolveFixtureSelection loads the real bakeoff compatible list" $ do
        cwd <- PathIO.getCurrentDir
        compatibleList <- repoFile "scripts/bakeoff-compatible.txt"
        helloRel <- parseRelFile "test/fixtures/realworld/hello-world.bash"
        fixtures <- resolveFixtureSelection cwd [] [] [] [compatibleList]
        H.assertBool "expected checked-in compatible fixtures" (length fixtures >= 10)
        case filter ((== helloRel) . specRelativePath) fixtures of
          [fixture] -> specSelectionSources fixture @?= [SelectionCompatible compatibleList]
          other -> H.assertFailure ("expected hello-world compatible fixture once, got " <> show (length other)),
      H.testCase "makeBenchmarkPlan excludes skipped and failed translations from runtime measurements" $ do
        cwd <- PathIO.getCurrentDir
        benchmarkRel <- parseRelFile "benchmark/fixtures/small.bash"
        integrationRel <- parseRelFile "test/fixtures/integration/source-recursive.bash"
        benchmarkArtifactDir <- fixtureArtifactDir benchmarkRel
        integrationArtifactDir <- fixtureArtifactDir integrationRel
        let benchmarkPath = cwd </> benchmarkRel
            integrationPath = cwd </> integrationRel
            metadata = MkFixtureMetadata [] ShellRunSource Nothing "" [] False
            benchmarkFixture =
              MkFixtureSpec
                { specPath = benchmarkPath,
                  specRelativePath = benchmarkRel,
                  specGroup = FixtureGroupBenchmark,
                  specMetadata = metadata,
                  specSelectionSources = [SelectionDefault FixtureGroupBenchmark],
                  specArtifactDir = benchmarkArtifactDir,
                  specSkipReason = Nothing
                }
            skippedFixture =
              benchmarkFixture
                { specPath = integrationPath,
                  specRelativePath = integrationRel,
                  specGroup = FixtureGroupIntegration,
                  specSelectionSources = [SelectionDefault FixtureGroupIntegration],
                  specArtifactDir = integrationArtifactDir,
                  specSkipReason = Just (SkipMissingPrereqs ["missing-tool"])
                }
            failedFixture =
              skippedFixture
                { specSkipReason = Nothing
                }
            tools =
              MkResolvedTools
                { toolsMonkExecutable = integrationPath,
                  toolsBabelfishPath = integrationPath,
                  toolsFishPath = integrationPath,
                  toolsHyperfinePath = Nothing,
                  toolsBabelfishVersion = MkToolVersion "unknown",
                  toolsFishVersion = MkToolVersion "unknown",
                  toolsHyperfineVersion = Nothing
                }
        benchmarkArtifacts <- fixtureArtifacts (sampleBakeoffConfig cwd True) benchmarkFixture
        skippedArtifacts <- fixtureArtifacts (sampleBakeoffConfig cwd True) skippedFixture
        failedArtifacts <- fixtureArtifacts (sampleBakeoffConfig cwd True) failedFixture
        let plan =
              makeBenchmarkPlan
                (sampleBakeoffConfig cwd True)
                [ (benchmarkFixture, benchmarkArtifacts, sampleTranslationReport CommandSucceeded),
                  (skippedFixture, skippedArtifacts, sampleTranslationReport CommandSkipped),
                  (failedFixture, failedArtifacts, sampleTranslationReport CommandFailed)
                ]
                tools
        benchmarkAllFixtures plan @?= [benchmarkPath, integrationPath]
        benchmarkFixtures plan @?= [benchmarkPath]
        benchmarkAllRuntime plan
          @?= [ MkRuntimeBenchmarkEntry
                  { runtimeBenchmarkBashPath = benchmarkPath,
                    runtimeBenchmarkFishPath = faMonkFish benchmarkArtifacts,
                    runtimeBenchmarkArgs = [],
                    runtimeBenchmarkMode = ShellRunSource,
                    runtimeBenchmarkStdin = ""
                  }
              ]
        benchmarkRuntimeFixtures plan @?= benchmarkAllRuntime plan,
      H.testCase "runtime benchmark accepts intentional nonzero exits but rejects a missing generated script" $ do
        cwd <- PathIO.getCurrentDir
        bashPath <- repoFile "test/fixtures/integration/stdout-stderr-exit.bash"
        fishPath <- repoFile "app/Main.hs"
        processEnv <- getEnvironment
        let missingFish = cwd </> unsafeRelFile "missing-generated.fish"
            entry =
              MkRuntimeBenchmarkEntry
                { runtimeBenchmarkBashPath = bashPath,
                  runtimeBenchmarkFishPath = missingFish,
                  runtimeBenchmarkArgs = [],
                  runtimeBenchmarkMode = ShellRunExec,
                  runtimeBenchmarkStdin = ""
                }
        bashCompleted <- runRuntimeBenchmarkEntry RuntimeBash fishPath 5 processEnv entry
        fishCompleted <- runRuntimeBenchmarkEntry RuntimeFish fishPath 5 processEnv entry
        H.assertBool "intentional Bash nonzero exit was treated as an infrastructure failure" bashCompleted
        H.assertBool "missing generated Fish script was accepted" (not fishCompleted),
      H.testCase "runtime benchmark syntax preflight rejects malformed generated Fish" $ do
        withTempDir "monk-runtime-syntax" $ \tmpDir -> do
          bashRel <- parseRelFile "valid.bash"
          fishRel <- parseRelFile "invalid.fish"
          let bashPath = tmpDir </> bashRel
              fishScriptPath = tmpDir </> fishRel
          TIO.writeFile (toFilePath bashPath) "exit 7\n"
          TIO.writeFile (toFilePath fishScriptPath) "if true\n"
          fishExecutable <-
            findExecutable "fish" >>= \case
              Nothing -> H.assertFailure "fish not found" >> unreachable
              Just path -> PathIO.resolveFile' path
          let entry =
                MkRuntimeBenchmarkEntry
                  { runtimeBenchmarkBashPath = bashPath,
                    runtimeBenchmarkFishPath = fishScriptPath,
                    runtimeBenchmarkArgs = [],
                    runtimeBenchmarkMode = ShellRunExec,
                    runtimeBenchmarkStdin = ""
                  }
          syntaxValid <- runtimeEntryHasValidSyntax fishExecutable entry
          H.assertBool "malformed generated Fish entered the runtime benchmark" (not syntaxValid),
      H.testCase "renderSummaryMarkdown reports aggregated statuses and mismatches" $ do
        cwd <- PathIO.getCurrentDir
        fixturePath <- repoFile "test/fixtures/integration/source-recursive.bash"
        relPath <- parseRelFile "test/fixtures/integration/source-recursive.bash"
        artifactDir <- fixtureArtifactDir relPath
        reports <- resolveFixtureSelection cwd [] [fixturePath] [] []
        fixture <-
          case reports of
            [selected] -> pure selected
            _ -> H.assertFailure "expected single selected fixture" >> unreachable
        monkBin <- repoFile "app/Main.hs"
        outputRel <- parseRelDir "tmp-bakeoff/"
        let meta =
              MkMetaReport
                { metaTimestamp = UTCTime (fromGregorian 2026 4 16) (secondsToDiffTime 0),
                  metaCwd = cwd,
                  metaOutputDir = cwd </> outputRel,
                  metaGit = MkGitMetadata {gitSha = Just "deadbeef", gitDirty = True},
                  metaHostOs = "darwin",
                  metaHostArch = "aarch64",
                  metaTools =
                    MkResolvedTools
                      { toolsMonkExecutable = monkBin,
                        toolsBabelfishPath = monkBin,
                        toolsFishPath = monkBin,
                        toolsHyperfinePath = Nothing,
                        toolsBabelfishVersion = MkToolVersion "unknown",
                        toolsFishVersion = MkToolVersion "unknown",
                        toolsHyperfineVersion = Nothing
                      },
                  metaConfig =
                    MkConfigReport
                      { configTranslationSettings = defaultBakeoffTranslationSettings,
                        configTranslationTimeoutSeconds = 30,
                        configRuntimeTimeoutSeconds = 30,
                        configBenchmarksEnabled = False,
                        configHyperfineRuns = 10,
                        configHyperfineWarmup = 1,
                        configJobs = Nothing,
                        configGroups = [FixtureGroupIntegration],
                        configFiles = [fixturePath],
                        configFileLists = [],
                        configCompatibleFileLists = []
                      },
                  metaFixtures = [makeFixtureSelectionReport fixture]
                }
            monkTranslation =
              MkTranslationReport
                { translationTool = ToolMonk,
                  translationStatus = CommandSucceeded,
                  translationExitCode = Just 0,
                  translationErrorCount = 0,
                  translationWarningCount = 0,
                  translationNotesCount = 0,
                  translationReviewRisk = Just "clean",
                  translationInputBytes = Just 10,
                  translationOutputBytes = Just 12,
                  translationExpansionRatio = Just 1.2,
                  translationStatistics = Nothing,
                  translationHelperBytes = Just 0,
                  translationHelperInvocations = 0,
                  translationExternalRequirements = [],
                  translationOutputPath = Nothing,
                  translationStderrPath = Nothing,
                  translationErrorMessage = Nothing
                }
            babelfishTranslation =
              MkTranslationReport
                { translationTool = ToolBabelfish,
                  translationStatus = CommandFailed,
                  translationExitCode = Just 1,
                  translationErrorCount = 1,
                  translationWarningCount = 0,
                  translationNotesCount = 0,
                  translationReviewRisk = Just "unsafe",
                  translationInputBytes = Just 10,
                  translationOutputBytes = Nothing,
                  translationExpansionRatio = Nothing,
                  translationStatistics = Nothing,
                  translationHelperBytes = Nothing,
                  translationHelperInvocations = 0,
                  translationExternalRequirements = [],
                  translationOutputPath = Nothing,
                  translationStderrPath = Nothing,
                  translationErrorMessage = Just "parse error"
                }
            report =
              MkFixtureReport
                { fixtureReportPath = fixturePath,
                  fixtureReportRelativePath = relPath,
                  fixtureReportArtifactDir = artifactDir,
                  fixtureReportGroup = FixtureGroupIntegration,
                  fixtureReportSelectionSources = [SelectionFile fixturePath],
                  fixtureReportMetadata = summarizeFixtureMetadata (specMetadata fixture),
                  fixtureReportSkipReason = Nothing,
                  fixtureReportMonkTranslation = Just monkTranslation,
                  fixtureReportBabelfishTranslation = Just babelfishTranslation,
                  fixtureReportMonkRuntime = Nothing,
                  fixtureReportBabelfishRuntime = Nothing,
                  fixtureReportDiff = Nothing
                }
            runtimeBenchmark =
              MkHyperfineSummary
                { hyperfineTitle = "Runtime All Fixtures",
                  hyperfineJsonPath = fixturePath,
                  hyperfineMarkdownPath = fixturePath,
                  hyperfineResults =
                    [ MkHyperfineResult
                        { hyperfineCommand = "generated-fish",
                          hyperfineMean = 0.012,
                          hyperfineMedian = 0.01,
                          hyperfineStddev = 0.002
                        }
                    ]
                }
            summary = renderSummaryMarkdown meta [report] [runtimeBenchmark]
        assertContains summary "- Babelfish translation: succeeded=0, failed=1, timed_out=0, skipped=0"
        assertContains summary "- Fixtures with any runtime diff: 1"
        assertContains summary "- `test/fixtures/integration/source-recursive.bash`: babelfish translation failed"
        assertContains summary "### Runtime All Fixtures"
        assertContains summary "generated-fish: median=0.010s, mean=0.012s, stddev=0.002s",
      H.testCase "renderToolPreflightFailure gives actionable missing-tool guidance" $ do
        let failure =
              MkToolPreflightFailure
                [ MkToolPreflightIssue
                    { preflightToolName = "babelfish",
                      preflightMessage = "not found on PATH.",
                      preflightAction = "Install it or pass `--babelfish` /path/to/babelfish."
                    }
                ]
            rendered = renderToolPreflightFailure failure
        assertContains rendered "Bake-off tool preflight failed."
        assertContains rendered "--babelfish"
        assertContains rendered "not found on PATH.",
      H.testCase "toolPreflightWarnings explains skipped benchmarks when hyperfine is missing" $ do
        cwd <- PathIO.getCurrentDir
        let cfg = sampleBakeoffConfig cwd True
            tools =
              MkResolvedTools
                { toolsMonkExecutable = cwd </> unsafeRelFile "app/Main.hs",
                  toolsBabelfishPath = cwd </> unsafeRelFile "app/Main.hs",
                  toolsFishPath = cwd </> unsafeRelFile "app/Main.hs",
                  toolsHyperfinePath = Nothing,
                  toolsBabelfishVersion = MkToolVersion "unknown",
                  toolsFishVersion = MkToolVersion "unknown",
                  toolsHyperfineVersion = Nothing
                }
            warnings = toolPreflightWarnings cfg tools
        warnings @?= ["hyperfine was not found, so benchmark runs will be skipped. Install hyperfine or rerun with --no-benchmark."]
    ]

repoFile :: FilePath -> IO (Path Abs File)
repoFile rel = do
  cwd <- PathIO.getCurrentDir
  relPath <- parseRelFile rel
  pure (cwd </> relPath)

withTempDir :: String -> (Path Abs Dir -> IO a) -> IO a
withTempDir prefix action = do
  tmpDir <- PathIO.getTempDir
  let create = do
        (path, handle) <- IO.openTempFile (toFilePath tmpDir) prefix
        IO.hClose handle
        removeFile path
        createDirectory path
        pure path
  bracket create cleanup (PathIO.resolveDir' >=> action)
  where
    cleanup path = do
      exists <- doesDirectoryExist path
      when exists (removeDirectoryRecursive path)

assertContains :: Text -> Text -> H.Assertion
assertContains haystack needle =
  H.assertBool
    ("expected substring " <> show needle <> " in:\n" <> toString haystack)
    (T.isInfixOf needle haystack)

unreachable :: IO a
unreachable = error "unreachable"

sampleBakeoffConfig :: Path Abs Dir -> Bool -> BakeoffConfig
sampleBakeoffConfig cwd benchmarksEnabled =
  MkBakeoffConfig
    { bakeoffTranslationSettings = defaultBakeoffTranslationSettings,
      bakeoffCwd = cwd,
      bakeoffOutputDir = unsafeAbsDir "/tmp/monk-bakeoff-tests/",
      bakeoffForce = False,
      bakeoffGroups = [],
      bakeoffFiles = [],
      bakeoffFileLists = [],
      bakeoffCompatibleFileLists = [],
      bakeoffJobs = Nothing,
      bakeoffTranslationTimeoutSeconds = 30,
      bakeoffRuntimeTimeoutSeconds = 30,
      bakeoffBenchmarksEnabled = benchmarksEnabled,
      bakeoffHyperfineRuns = 10,
      bakeoffHyperfineWarmup = 1,
      bakeoffBabelfishPathHint = Nothing,
      bakeoffFishPathHint = Nothing,
      bakeoffHyperfinePathHint = Nothing,
      bakeoffBabelfishVersionOverride = Nothing
    }

sampleTranslationReport :: CommandStatus -> TranslationReport
sampleTranslationReport status =
  MkTranslationReport
    { translationTool = ToolMonk,
      translationStatus = status,
      translationExitCode = Nothing,
      translationErrorCount = 0,
      translationWarningCount = 0,
      translationNotesCount = 0,
      translationReviewRisk = Nothing,
      translationInputBytes = Nothing,
      translationOutputBytes = Nothing,
      translationExpansionRatio = Nothing,
      translationStatistics = Nothing,
      translationHelperBytes = Nothing,
      translationHelperInvocations = 0,
      translationExternalRequirements = [],
      translationOutputPath = Nothing,
      translationStderrPath = Nothing,
      translationErrorMessage = Nothing
    }

unsafeAbsDir :: FilePath -> Path Abs Dir
unsafeAbsDir =
  either (error . show) id . parseAbsDir

unsafeRelFile :: FilePath -> Path Rel File
unsafeRelFile =
  either (error . show) id . parseRelFile

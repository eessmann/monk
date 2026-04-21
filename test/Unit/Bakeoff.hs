{-# LANGUAGE OverloadedStrings #-}

module Unit.Bakeoff
  ( unitBakeoffTests,
  )
where

import Control.Exception (bracket)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Bakeoff.Benchmark (makeBenchmarkPlan)
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
    removeDirectoryRecursive,
    removeFile,
  )
import System.IO qualified as IO
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as H

unitBakeoffTests :: TestTree
unitBakeoffTests =
  testGroup
    "Bakeoff"
    [ H.testCase "fixtureArtifactDir mirrors the fixture path under fixtures/" $ do
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
      H.testCase "makeBenchmarkPlan excludes skipped fixtures" $ do
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
            plan = makeBenchmarkPlan [benchmarkFixture, skippedFixture] tools
        benchmarkAllFixtures plan @?= [benchmarkPath]
        benchmarkFixtures plan @?= [benchmarkPath],
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
                      { configTranslationTimeoutSeconds = 30,
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
                  translationWarningCount = 0,
                  translationNotesCount = 0,
                  translationHighWarnings = 0,
                  translationMediumWarnings = 0,
                  translationLowWarnings = 0,
                  translationConfidenceScore = Just 100,
                  translationOutputPath = Nothing,
                  translationStderrPath = Nothing,
                  translationErrorMessage = Nothing
                }
            babelfishTranslation =
              MkTranslationReport
                { translationTool = ToolBabelfish,
                  translationStatus = CommandFailed,
                  translationExitCode = Just 1,
                  translationWarningCount = 0,
                  translationNotesCount = 0,
                  translationHighWarnings = 0,
                  translationMediumWarnings = 0,
                  translationLowWarnings = 0,
                  translationConfidenceScore = Nothing,
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
            summary = renderSummaryMarkdown meta [report] []
        assertContains summary "- Babelfish translation: succeeded=0, failed=1, timed_out=0, skipped=0"
        assertContains summary "- Fixtures with any runtime diff: 1"
        assertContains summary "- `test/fixtures/integration/source-recursive.bash`: babelfish translation failed",
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
    { bakeoffCwd = cwd,
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

unsafeAbsDir :: FilePath -> Path Abs Dir
unsafeAbsDir =
  either (error . show) id . parseAbsDir

unsafeRelFile :: FilePath -> Path Rel File
unsafeRelFile =
  either (error . show) id . parseRelFile

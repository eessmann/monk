{-# LANGUAGE OverloadedStrings #-}

module Unit.Bakeoff
  ( unitBakeoffTests,
  )
where

import Data.Text qualified as T
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
import Bakeoff.Types
import Path
  ( Abs,
    File,
    Path,
    parseRelDir,
    parseRelFile,
    toFilePath,
    (</>),
  )
import Path.IO qualified as PathIO
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
            fsGroup fixture @?= FixtureGroupIntegration
            fsSelectionSources fixture @?= [SelectionFile fixturePath]
          other -> H.assertFailure ("expected one selected fixture, got " <> show (length other)),
      H.testCase "makeBenchmarkPlan excludes skipped fixtures" $ do
        cwd <- PathIO.getCurrentDir
        benchmarkRel <- parseRelFile "benchmark/fixtures/small.bash"
        integrationRel <- parseRelFile "test/fixtures/integration/source-recursive.bash"
        benchmarkArtifactDir <- fixtureArtifactDir benchmarkRel
        integrationArtifactDir <- fixtureArtifactDir integrationRel
        let benchmarkPath = cwd </> benchmarkRel
            integrationPath = cwd </> integrationRel
            metadata = FixtureMetadata [] ShellRunSource Nothing "" [] False
            benchmarkFixture =
              FixtureSpec
                { fsPath = benchmarkPath,
                  fsRelativePath = benchmarkRel,
                  fsGroup = FixtureGroupBenchmark,
                  fsMetadata = metadata,
                  fsSelectionSources = [SelectionDefault FixtureGroupBenchmark],
                  fsArtifactDir = benchmarkArtifactDir,
                  fsSkipReason = Nothing
                }
            skippedFixture =
              benchmarkFixture
                { fsPath = integrationPath,
                  fsRelativePath = integrationRel,
                  fsGroup = FixtureGroupIntegration,
                  fsSelectionSources = [SelectionDefault FixtureGroupIntegration],
                  fsArtifactDir = integrationArtifactDir,
                  fsSkipReason = Just (SkipMissingPrereqs ["missing-tool"])
                }
            tools =
              ResolvedTools
                { rtMonkExecutable = integrationPath,
                  rtBabelfishPath = integrationPath,
                  rtFishPath = integrationPath,
                  rtHyperfinePath = Nothing,
                  rtBabelfishVersion = "unknown",
                  rtFishVersion = "unknown",
                  rtHyperfineVersion = Nothing
                }
            plan = makeBenchmarkPlan [benchmarkFixture, skippedFixture] tools
        bpAllFixtures plan @?= [benchmarkPath]
        bpBenchmarkFixtures plan @?= [benchmarkPath],
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
              MetaReport
                { mrTimestamp = UTCTime (fromGregorian 2026 4 16) (secondsToDiffTime 0),
                  mrCwd = cwd,
                  mrOutputDir = cwd </> outputRel,
                  mrGit = GitMetadata {gmSha = Just "deadbeef", gmDirty = True},
                  mrHostOs = "darwin",
                  mrHostArch = "aarch64",
                  mrTools =
                    ResolvedTools
                      { rtMonkExecutable = monkBin,
                        rtBabelfishPath = monkBin,
                        rtFishPath = monkBin,
                        rtHyperfinePath = Nothing,
                        rtBabelfishVersion = "unknown",
                        rtFishVersion = "unknown",
                        rtHyperfineVersion = Nothing
                      },
                  mrConfig =
                    ConfigReport
                      { crTranslationTimeoutSeconds = 30,
                        crRuntimeTimeoutSeconds = 30,
                        crBenchmarksEnabled = False,
                        crHyperfineRuns = 10,
                        crHyperfineWarmup = 1,
                        crJobs = Nothing,
                        crGroups = [FixtureGroupIntegration],
                        crFiles = [fixturePath],
                        crFileLists = [],
                        crCompatibleFileLists = []
                      },
                  mrFixtures = [makeFixtureSelectionReport fixture]
                }
            monkTranslation =
              TranslationReport
                { trTool = ToolMonk,
                  trStatus = CommandSucceeded,
                  trExitCode = Just 0,
                  trWarnings = 0,
                  trNotes = 0,
                  trWarningHigh = 0,
                  trWarningMedium = 0,
                  trWarningLow = 0,
                  trConfidenceScore = Just 100,
                  trOutputPath = Nothing,
                  trStderrPath = Nothing,
                  trErrorMessage = Nothing
                }
            babelfishTranslation =
              TranslationReport
                { trTool = ToolBabelfish,
                  trStatus = CommandFailed,
                  trExitCode = Just 1,
                  trWarnings = 0,
                  trNotes = 0,
                  trWarningHigh = 0,
                  trWarningMedium = 0,
                  trWarningLow = 0,
                  trConfidenceScore = Nothing,
                  trOutputPath = Nothing,
                  trStderrPath = Nothing,
                  trErrorMessage = Just "parse error"
                }
            report =
              FixtureReport
                { frPath = fixturePath,
                  frRelativePath = relPath,
                  frArtifactDir = artifactDir,
                  frGroup = FixtureGroupIntegration,
                  frSelectionSources = [SelectionFile fixturePath],
                  frMetadata = summarizeFixtureMetadata (fsMetadata fixture),
                  frSkipReason = Nothing,
                  frMonkTranslation = Just monkTranslation,
                  frBabelfishTranslation = Just babelfishTranslation,
                  frMonkRuntime = Nothing,
                  frBabelfishRuntime = Nothing,
                  frDiff = Nothing
                }
            summary = renderSummaryMarkdown meta [report] []
        assertContains summary "- Babelfish translation: succeeded=0, failed=1, timed_out=0, skipped=0"
        assertContains summary "- Fixtures with any runtime diff: 1"
        assertContains summary "- `test/fixtures/integration/source-recursive.bash`: babelfish translation failed"
    ]

repoFile :: FilePath -> IO (Path Abs File)
repoFile rel = do
  cwd <- PathIO.getCurrentDir
  relPath <- parseRelFile rel
  pure (cwd </> relPath)

assertContains :: Text -> Text -> H.Assertion
assertContains haystack needle =
  H.assertBool
    ("expected substring " <> show needle <> " in:\n" <> toString haystack)
    (T.isInfixOf needle haystack)

unreachable :: IO a
unreachable = error "unreachable"

module Bakeoff.Execution
  ( defineFixtureRules,
    defineBenchmarkRules,
    runWorkerFixture,
  )
where

import Bakeoff.Artifacts
  ( BakeoffOutputs (..),
    FixtureArtifacts (..),
    ensureParentDirectory,
  )
import Bakeoff.Benchmark (runHyperfineRuntimeSuite, runHyperfineSuite)
import Bakeoff.Execution.Diff (buildDiffReport)
import Bakeoff.Execution.Runtime (buildRuntimeReport)
import Bakeoff.Execution.Translation
  ( buildBabelfishTranslationReport,
    buildMonkTranslationReport,
    runWorkerFixture,
  )
import Bakeoff.Process
  ( readJsonFile,
    writeJsonFile,
  )
import Bakeoff.Selection (summarizeFixtureMetadata)
import Bakeoff.Types
import Development.Shake
import Path
  ( toFilePath,
  )
import Path.IO qualified as PathIO

defineFixtureRules :: BakeoffConfig -> ResolvedTools -> [(String, String)] -> FixtureSpec -> FixtureArtifacts -> Rules ()
defineFixtureRules cfg tools processEnv fixture artifacts = do
  toFilePath (faMonkTranslateJson artifacts) %> \_ -> do
    need [toFilePath (specPath fixture)]
    liftIO (PathIO.ensureDir (faDir artifacts))
    report <- liftIO $ buildMonkTranslationReport cfg fixture artifacts
    liftIO $ do
      ensureParentDirectory (faMonkTranslateJson artifacts)
      writeJsonFile (faMonkTranslateJson artifacts) report

  toFilePath (faBabelfishTranslateJson artifacts) %> \_ -> do
    need [toFilePath (specPath fixture)]
    liftIO (PathIO.ensureDir (faDir artifacts))
    report <- liftIO $ buildBabelfishTranslationReport cfg tools fixture artifacts processEnv
    liftIO $ do
      ensureParentDirectory (faBabelfishTranslateJson artifacts)
      writeJsonFile (faBabelfishTranslateJson artifacts) report

  toFilePath (faMonkRuntimeJson artifacts) %> \_ -> do
    need [toFilePath (faMonkTranslateJson artifacts)]
    liftIO (PathIO.ensureDir (faDir artifacts))
    translationReport <- liftIO $ readJsonFile (faMonkTranslateJson artifacts)
    report <- liftIO $ buildRuntimeReport cfg tools fixture artifacts ToolMonk translationReport processEnv
    liftIO $ do
      ensureParentDirectory (faMonkRuntimeJson artifacts)
      writeJsonFile (faMonkRuntimeJson artifacts) report

  toFilePath (faBabelfishRuntimeJson artifacts) %> \_ -> do
    need [toFilePath (faBabelfishTranslateJson artifacts)]
    liftIO (PathIO.ensureDir (faDir artifacts))
    translationReport <- liftIO $ readJsonFile (faBabelfishTranslateJson artifacts)
    report <- liftIO $ buildRuntimeReport cfg tools fixture artifacts ToolBabelfish translationReport processEnv
    liftIO $ do
      ensureParentDirectory (faBabelfishRuntimeJson artifacts)
      writeJsonFile (faBabelfishRuntimeJson artifacts) report

  toFilePath (faDiffJson artifacts) %> \_ -> do
    need [toFilePath (faMonkRuntimeJson artifacts), toFilePath (faBabelfishRuntimeJson artifacts)]
    liftIO (PathIO.ensureDir (faDir artifacts))
    monkRuntime <- liftIO $ readJsonFile (faMonkRuntimeJson artifacts)
    babelfishRuntime <- liftIO $ readJsonFile (faBabelfishRuntimeJson artifacts)
    report <- liftIO $ buildDiffReport cfg artifacts monkRuntime babelfishRuntime
    liftIO $ do
      ensureParentDirectory (faDiffJson artifacts)
      writeJsonFile (faDiffJson artifacts) report

  toFilePath (faResultJson artifacts) %> \_ -> do
    need
      [ toFilePath (faMonkTranslateJson artifacts),
        toFilePath (faBabelfishTranslateJson artifacts),
        toFilePath (faMonkRuntimeJson artifacts),
        toFilePath (faBabelfishRuntimeJson artifacts),
        toFilePath (faDiffJson artifacts)
      ]
    monkTranslation <- liftIO $ readJsonFile (faMonkTranslateJson artifacts)
    babelfishTranslation <- liftIO $ readJsonFile (faBabelfishTranslateJson artifacts)
    monkRuntime <- liftIO $ readJsonFile (faMonkRuntimeJson artifacts)
    babelfishRuntime <- liftIO $ readJsonFile (faBabelfishRuntimeJson artifacts)
    diffReport <- liftIO $ readJsonFile (faDiffJson artifacts)
    let fixtureReport =
          MkFixtureReport
            { fixtureReportPath = specPath fixture,
              fixtureReportRelativePath = specRelativePath fixture,
              fixtureReportArtifactDir = specArtifactDir fixture,
              fixtureReportGroup = specGroup fixture,
              fixtureReportSelectionSources = specSelectionSources fixture,
              fixtureReportMetadata = summarizeFixtureMetadata (specMetadata fixture),
              fixtureReportSkipReason = specSkipReason fixture,
              fixtureReportMonkTranslation = Just monkTranslation,
              fixtureReportBabelfishTranslation = Just babelfishTranslation,
              fixtureReportMonkRuntime = Just monkRuntime,
              fixtureReportBabelfishRuntime = Just babelfishRuntime,
              fixtureReportDiff = Just diffReport
            }
    liftIO $ do
      ensureParentDirectory (faResultJson artifacts)
      writeJsonFile (faResultJson artifacts) fixtureReport

defineBenchmarkRules :: BakeoffConfig -> ResolvedTools -> BakeoffOutputs -> [(FixtureSpec, FixtureArtifacts)] -> Rules ()
defineBenchmarkRules cfg tools outputs fixtures =
  case toolsHyperfinePath tools of
    Nothing -> pure ()
    Just hyperfinePath -> do
      toFilePath (boHyperfineAllJsonPath outputs) %> \_ -> do
        need [toFilePath (boBenchmarkPlanPath outputs)]
        liftIO $
          runHyperfineSuite
            hyperfinePath
            cfg
            outputs
            BenchmarkSuiteAll
            (boHyperfineAllJsonPath outputs)
            (boHyperfineAllMarkdownPath outputs)

      toFilePath (boHyperfineBenchmarkJsonPath outputs) %> \_ -> do
        need [toFilePath (boBenchmarkPlanPath outputs)]
        liftIO $
          runHyperfineSuite
            hyperfinePath
            cfg
            outputs
            BenchmarkSuiteBenchmark
            (boHyperfineBenchmarkJsonPath outputs)
            (boHyperfineBenchmarkMarkdownPath outputs)

      toFilePath (boHyperfineRuntimeAllJsonPath outputs) %> \_ -> do
        need (toFilePath (boBenchmarkPlanPath outputs) : runtimeDependencies (const True))
        liftIO $
          runHyperfineRuntimeSuite
            hyperfinePath
            cfg
            outputs
            BenchmarkSuiteAll
            (boHyperfineRuntimeAllJsonPath outputs)
            (boHyperfineRuntimeAllMarkdownPath outputs)

      toFilePath (boHyperfineRuntimeBenchmarkJsonPath outputs) %> \_ -> do
        need (toFilePath (boBenchmarkPlanPath outputs) : runtimeDependencies ((== FixtureGroupBenchmark) . specGroup))
        liftIO $
          runHyperfineRuntimeSuite
            hyperfinePath
            cfg
            outputs
            BenchmarkSuiteBenchmark
            (boHyperfineRuntimeBenchmarkJsonPath outputs)
            (boHyperfineRuntimeBenchmarkMarkdownPath outputs)
  where
    runtimeDependencies includeFixture =
      [ toFilePath (faMonkTranslateJson artifacts)
      | (fixture, artifacts) <- fixtures,
        isNothing (specSkipReason fixture),
        includeFixture fixture
      ]

module Bakeoff.Runner
  ( runBakeoff,
    runBenchmarkWorker,
  )
where

import Data.Time (getCurrentTime)
import Bakeoff.Artifacts
  ( BakeoffOutputs (..),
    bakeoffOutputs,
    ensureParentDirectory,
    FixtureArtifacts (..),
    fixtureArtifacts,
    prepareOutputDirectory,
    writeTextFile,
  )
import Bakeoff.Benchmark
  ( loadBenchmarkSummaries,
    makeBenchmarkPlan,
  )
import Bakeoff.Execution
  ( defineBenchmarkRules,
    defineFixtureRules,
    runWorkerFixture,
  )
import Development.Shake
import Bakeoff.Process
  ( readJsonFile,
    writeJsonFile,
  )
import Bakeoff.Report (renderSummaryMarkdown)
import Bakeoff.Selection
  ( makeFixtureSelectionReport,
    resolveFixtureSelection,
  )
import Bakeoff.Shell (prepareEnv)
import Bakeoff.Tools
  ( collectGitMetadata,
    configReport,
    resolveTools,
  )
import Bakeoff.Types
import Path
  ( Abs,
    File,
    Path,
    toFilePath,
  )
import Path.IO qualified as PathIO
import System.Environment (getExecutablePath)
import System.Info qualified as SysInfo

runBakeoff :: BakeoffConfig -> IO ()
runBakeoff cfg = do
  prepareOutputDirectory (bakeoffOutputDir cfg) (bakeoffForce cfg)
  monkExecutable <- PathIO.resolveFile' =<< getExecutablePath
  tools <- resolveTools monkExecutable cfg
  processEnv <- prepareEnv
  fixtures <- resolveFixtureSelection (bakeoffCwd cfg) (bakeoffGroups cfg) (bakeoffFiles cfg) (bakeoffFileLists cfg) (bakeoffCompatibleFileLists cfg)
  artifacts <- traverse (\fixture -> (fixture,) <$> fixtureArtifacts cfg fixture) fixtures
  outputs <- bakeoffOutputs (bakeoffOutputDir cfg)
  git <- collectGitMetadata (bakeoffCwd cfg)
  timestamp <- getCurrentTime
  let meta =
        MkMetaReport
          { metaTimestamp = timestamp,
            metaCwd = bakeoffCwd cfg,
            metaOutputDir = bakeoffOutputDir cfg,
            metaGit = git,
            metaHostOs = toText SysInfo.os,
            metaHostArch = toText SysInfo.arch,
            metaTools = tools,
            metaConfig = configReport cfg,
            metaFixtures = map (makeFixtureSelectionReport . fst) artifacts
          }
      benchmarkPlan = makeBenchmarkPlan (map fst artifacts) tools
      benchmarkTargets =
        case (bakeoffBenchmarksEnabled cfg, toolsHyperfinePath tools) of
          (True, Just _) -> [boHyperfineAllJsonPath outputs, boHyperfineBenchmarkJsonPath outputs]
          _ -> []
  shake
    ( shakeOptions
        { shakeFiles = toFilePath (boShakeDir outputs),
          shakeThreads = fromMaybe 0 (bakeoffJobs cfg),
          shakeVerbosity = Quiet
        }
    )
    $ do
      want [toFilePath (boSummaryPath outputs)]

      toFilePath (boMetaPath outputs) %> \_ ->
        liftIO $ do
          ensureParentDirectory (boMetaPath outputs)
          writeJsonFile (boMetaPath outputs) meta

      toFilePath (boBenchmarkPlanPath outputs) %> \_ ->
        liftIO $ do
          ensureParentDirectory (boBenchmarkPlanPath outputs)
          writeJsonFile (boBenchmarkPlanPath outputs) benchmarkPlan

      forM_ artifacts $ \(fixture, fixtureArtifactPaths) ->
        defineFixtureRules cfg tools processEnv fixture fixtureArtifactPaths

      toFilePath (boReportPath outputs) %> \_ -> do
        need (map (toFilePath . faResultJson . snd) artifacts)
        reports <- liftIO $ traverse (readJsonFile . faResultJson . snd) artifacts
        liftIO $ do
          ensureParentDirectory (boReportPath outputs)
          writeJsonFile (boReportPath outputs) (reports :: [FixtureReport])

      when (not (null benchmarkTargets)) $
        defineBenchmarkRules cfg tools outputs

      toFilePath (boSummaryPath outputs) %> \_ -> do
        need (map toFilePath ([boMetaPath outputs, boReportPath outputs] <> benchmarkTargets))
        metaReport <- liftIO $ readJsonFile (boMetaPath outputs)
        reports <- liftIO $ readJsonFile (boReportPath outputs)
        benchmarks <- liftIO $ loadBenchmarkSummaries outputs benchmarkTargets
        liftIO $ do
          ensureParentDirectory (boSummaryPath outputs)
          writeTextFile (boSummaryPath outputs) (renderSummaryMarkdown metaReport reports benchmarks)

runBenchmarkWorker :: ToolName -> Path Abs File -> BenchmarkSuite -> IO Int
runBenchmarkWorker tool planPath suite = do
  MkBenchmarkPlan {..} <- readJsonFile planPath
  let fixtures =
        case suite of
          BenchmarkSuiteAll -> benchmarkAllFixtures
          BenchmarkSuiteBenchmark -> benchmarkFixtures
  failures <- catMaybes <$> traverse (runWorkerFixture tool benchmarkBabelfishPath) fixtures
  pure $
    if null failures
      then 0
      else 1

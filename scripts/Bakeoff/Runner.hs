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
  prepareOutputDirectory (bcOutputDir cfg) (bcForce cfg)
  monkExecutable <- PathIO.resolveFile' =<< getExecutablePath
  tools <- resolveTools monkExecutable cfg
  processEnv <- prepareEnv
  fixtures <- resolveFixtureSelection (bcCwd cfg) (bcGroups cfg) (bcFiles cfg) (bcFileLists cfg) (bcCompatibleFileLists cfg)
  artifacts <- traverse (\fixture -> (fixture,) <$> fixtureArtifacts cfg fixture) fixtures
  outputs <- bakeoffOutputs (bcOutputDir cfg)
  git <- collectGitMetadata (bcCwd cfg)
  timestamp <- getCurrentTime
  let meta =
        MetaReport
          { mrTimestamp = timestamp,
            mrCwd = bcCwd cfg,
            mrOutputDir = bcOutputDir cfg,
            mrGit = git,
            mrHostOs = toText SysInfo.os,
            mrHostArch = toText SysInfo.arch,
            mrTools = tools,
            mrConfig = configReport cfg,
            mrFixtures = map (makeFixtureSelectionReport . fst) artifacts
          }
      benchmarkPlan = makeBenchmarkPlan (map fst artifacts) tools
      benchmarkTargets =
        case (bcBenchmarksEnabled cfg, rtHyperfinePath tools) of
          (True, Just _) -> [boHyperfineAllJsonPath outputs, boHyperfineBenchmarkJsonPath outputs]
          _ -> []
  shake
    ( shakeOptions
        { shakeFiles = toFilePath (boShakeDir outputs),
          shakeThreads = fromMaybe 0 (bcJobs cfg),
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
  BenchmarkPlan {..} <- readJsonFile planPath
  let fixtures =
        case suite of
          BenchmarkSuiteAll -> bpAllFixtures
          BenchmarkSuiteBenchmark -> bpBenchmarkFixtures
  failures <- catMaybes <$> traverse (runWorkerFixture tool bpBabelfishPath) fixtures
  pure $
    if null failures
      then 0
      else 1

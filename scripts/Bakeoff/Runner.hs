module Bakeoff.Runner
  ( runBakeoff,
    runBenchmarkWorker,
  )
where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
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
    renderToolPreflightFailure,
    resolveTools,
    toolPreflightWarnings,
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
import System.Exit qualified as Exit
import System.Info qualified as SysInfo

runBakeoff :: BakeoffConfig -> IO ()
runBakeoff cfg = do
  prepareOutputDirectory (bakeoffOutputDir cfg) (bakeoffForce cfg)
  monkExecutable <- PathIO.resolveFile' =<< getExecutablePath
  toolsResult <- resolveTools monkExecutable cfg
  tools <-
    case toolsResult of
      Left preflightFailure ->
        Exit.die (toString (renderToolPreflightFailure preflightFailure))
      Right resolved -> pure resolved
  let warnings = toolPreflightWarnings cfg tools
  unless (null warnings) $
    TIO.hPutStrLn stderr $
      T.unlines
        ( "Bake-off tool preflight notes:"
            : map ("- " <>) warnings
        )
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

      forM_ artifacts $
        uncurry (defineFixtureRules cfg tools processEnv)

      toFilePath (boReportPath outputs) %> \_ -> do
        need (map (toFilePath . faResultJson . snd) artifacts)
        reports <- liftIO $ traverse (readJsonFile . faResultJson . snd) artifacts
        liftIO $ do
          ensureParentDirectory (boReportPath outputs)
          writeJsonFile (boReportPath outputs) (reports :: [FixtureReport])

      unless (null benchmarkTargets) $
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

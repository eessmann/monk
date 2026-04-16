module Bakeoff.Benchmark
  ( makeBenchmarkPlan,
    runHyperfineSuite,
    loadBenchmarkSummaries,
  )
where

import Data.Text qualified as T
import Bakeoff.Artifacts (BakeoffOutputs (..), ensureParentDirectory)
import Bakeoff.Process (runProcessText)
import Bakeoff.Report (readHyperfineSummary)
import Bakeoff.Types
import Path (Abs, File, Path, toFilePath)
import Path.IO qualified as PathIO
import System.Environment (getExecutablePath)
import System.Process (proc)

makeBenchmarkPlan :: [FixtureSpec] -> ResolvedTools -> BenchmarkPlan
makeBenchmarkPlan fixtures tools =
  MkBenchmarkPlan
    { benchmarkAllFixtures = [specPath fixture | fixture <- fixtures, isNothing (specSkipReason fixture)],
      benchmarkFixtures = [specPath fixture | fixture <- fixtures, specGroup fixture == FixtureGroupBenchmark, isNothing (specSkipReason fixture)],
      benchmarkBabelfishPath = toolsBabelfishPath tools
    }

runHyperfineSuite ::
  Path Abs File ->
  BakeoffConfig ->
  BakeoffOutputs ->
  BenchmarkSuite ->
  Path Abs File ->
  Path Abs File ->
  IO ()
runHyperfineSuite hyperfinePath cfg outputs suite jsonPath markdownPath = do
  ensureParentDirectory jsonPath
  currentExe <- PathIO.resolveFile' =<< getExecutablePath
  let suiteName =
        case suite of
          BenchmarkSuiteAll -> "all"
          BenchmarkSuiteBenchmark -> "benchmark"
      workerCommand tool =
        T.unpack $
          T.intercalate
            " "
            [ quoteArg (toText (toFilePath currentExe)),
              quoteArg "_benchmark-worker",
              quoteArg "--tool",
              quoteArg (renderToolArgument tool),
              quoteArg "--plan",
              quoteArg (toText (toFilePath (boBenchmarkPlanPath outputs))),
              quoteArg "--suite",
              quoteArg suiteName
            ]
      process =
        proc
          (toFilePath hyperfinePath)
          [ "--warmup",
            show (bakeoffHyperfineWarmup cfg),
            "--runs",
            show (bakeoffHyperfineRuns cfg),
            "--ignore-failure",
            "--export-json",
            toFilePath jsonPath,
            "--export-markdown",
            toFilePath markdownPath,
            "--command-name",
            "monk",
            workerCommand ToolMonk,
            "--command-name",
            "babelfish",
            workerCommand ToolBabelfish
          ]
  _ <- runProcessText Nothing process ""
  pure ()

loadBenchmarkSummaries :: BakeoffOutputs -> [Path Abs File] -> IO [HyperfineSummary]
loadBenchmarkSummaries outputs targets = do
  allSummary <-
    if boHyperfineAllJsonPath outputs `elem` targets
      then readHyperfineSummary "All Fixtures" (boHyperfineAllJsonPath outputs) (boHyperfineAllMarkdownPath outputs)
      else pure Nothing
  benchmarkSummary <-
    if boHyperfineBenchmarkJsonPath outputs `elem` targets
      then readHyperfineSummary "Benchmark Fixtures" (boHyperfineBenchmarkJsonPath outputs) (boHyperfineBenchmarkMarkdownPath outputs)
      else pure Nothing
  pure (catMaybes [allSummary, benchmarkSummary])

renderToolArgument :: ToolName -> Text
renderToolArgument = \case
  ToolMonk -> "monk"
  ToolBabelfish -> "babelfish"

quoteArg :: Text -> Text
quoteArg txt =
  "'" <> T.replace "'" "'\\''" txt <> "'"

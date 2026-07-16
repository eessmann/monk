module Bakeoff.Benchmark
  ( makeBenchmarkPlan,
    runHyperfineSuite,
    runHyperfineRuntimeSuite,
    runtimeEntryHasValidSyntax,
    loadBenchmarkSummaries,
  )
where

import Bakeoff.Artifacts (BakeoffOutputs (..), FixtureArtifacts (faMonkFish), ensureParentDirectory, writeTextFile)
import Bakeoff.Fixture (FixtureMetadata (..))
import Bakeoff.Process (ProcessOutput (poExitCode), readJsonFile, runProcessText, writeJsonFile)
import Bakeoff.Report (readHyperfineSummary)
import Bakeoff.Types
import Control.Exception (IOException, try)
import Data.Aeson (Value, object, (.=))
import Data.Text qualified as T
import Path (Abs, File, Path, toFilePath)
import Path.IO qualified as PathIO
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (ExitSuccess))
import System.Process (proc)

makeBenchmarkPlan :: BakeoffConfig -> [(FixtureSpec, FixtureArtifacts, TranslationReport)] -> ResolvedTools -> BenchmarkPlan
makeBenchmarkPlan cfg fixtures tools =
  MkBenchmarkPlan
    { benchmarkAllFixtures = map (specPath . first3) available,
      benchmarkFixtures = map (specPath . first3) benchmarkOnly,
      benchmarkAllRuntime = map runtimeEntry runtimeAvailable,
      benchmarkRuntimeFixtures = map runtimeEntry runtimeBenchmarkOnly,
      benchmarkBabelfishPath = toolsBabelfishPath tools,
      benchmarkFishPath = toolsFishPath tools,
      benchmarkRuntimeTimeoutSeconds = bakeoffRuntimeTimeoutSeconds cfg
    }
  where
    available = filter (isNothing . specSkipReason . first3) fixtures
    benchmarkOnly = filter ((== FixtureGroupBenchmark) . specGroup . first3) available
    runtimeAvailable = filter ((== CommandSucceeded) . translationStatus . third3) available
    runtimeBenchmarkOnly = filter ((== FixtureGroupBenchmark) . specGroup . first3) runtimeAvailable
    runtimeEntry (fixture, artifacts, _) =
      let metadata = specMetadata fixture
       in MkRuntimeBenchmarkEntry
            { runtimeBenchmarkBashPath = specPath fixture,
              runtimeBenchmarkFishPath = faMonkFish artifacts,
              runtimeBenchmarkArgs = fmArgs metadata,
              runtimeBenchmarkMode = fmMode metadata,
              runtimeBenchmarkStdin = fmStdin metadata
            }

    first3 (value, _, _) = value
    third3 (_, _, value) = value

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

runHyperfineRuntimeSuite ::
  Path Abs File ->
  BakeoffConfig ->
  BakeoffOutputs ->
  BenchmarkSuite ->
  Path Abs File ->
  Path Abs File ->
  IO ()
runHyperfineRuntimeSuite hyperfinePath cfg outputs suite jsonPath markdownPath = do
  ensureParentDirectory jsonPath
  plan@MkBenchmarkPlan {..} <- readJsonFile (boBenchmarkPlanPath outputs)
  let entries =
        case suite of
          BenchmarkSuiteAll -> benchmarkAllRuntime
          BenchmarkSuiteBenchmark -> benchmarkRuntimeFixtures
  validEntries <- filterM (runtimeEntryHasValidSyntax benchmarkFishPath) entries
  if null validEntries
    then do
      writeJsonFile jsonPath (object ["results" .= ([] :: [Value])])
      writeTextFile markdownPath ""
    else do
      currentExe <- PathIO.resolveFile' =<< getExecutablePath
      suitePlanPath <- PathIO.resolveFile' (toFilePath jsonPath <> ".plan.json")
      writeJsonFile suitePlanPath (withRuntimeEntries suite validEntries plan)
      let suiteName = renderSuiteArgument suite
          workerCommand runtimeShell =
            T.unpack $
              T.intercalate
                " "
                [ quoteArg (toText (toFilePath currentExe)),
                  quoteArg "_runtime-benchmark-worker",
                  quoteArg "--shell",
                  quoteArg (renderRuntimeShellArgument runtimeShell),
                  quoteArg "--plan",
                  quoteArg (toText (toFilePath suitePlanPath)),
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
                "bash",
                workerCommand RuntimeBash,
                "--command-name",
                "generated-fish",
                workerCommand RuntimeFish
              ]
      _ <- runProcessText Nothing process ""
      pure ()
  where
    withRuntimeEntries selectedSuite entries plan =
      case selectedSuite of
        BenchmarkSuiteAll -> plan {benchmarkAllRuntime = entries}
        BenchmarkSuiteBenchmark -> plan {benchmarkRuntimeFixtures = entries}

runtimeEntryHasValidSyntax :: Path Abs File -> RuntimeBenchmarkEntry -> IO Bool
runtimeEntryHasValidSyntax fishPath entry = do
  bashValid <- syntaxCheck (proc "bash" ["-n", toFilePath (runtimeBenchmarkBashPath entry)])
  fishValid <- syntaxCheck (proc (toFilePath fishPath) ["--no-config", "--no-execute", toFilePath (runtimeBenchmarkFishPath entry)])
  pure (bashValid && fishValid)
  where
    syntaxCheck process = do
      attempted <- try (runProcessText Nothing process "") :: IO (Either IOException (Maybe ProcessOutput))
      pure $
        case attempted of
          Right (Just output) -> poExitCode output == ExitSuccess
          _ -> False

loadBenchmarkSummaries :: BakeoffOutputs -> [Path Abs File] -> IO [HyperfineSummary]
loadBenchmarkSummaries outputs targets = do
  allSummary <-
    if boHyperfineAllJsonPath outputs `elem` targets
      then readHyperfineSummary "Translation All Fixtures" (boHyperfineAllJsonPath outputs) (boHyperfineAllMarkdownPath outputs)
      else pure Nothing
  benchmarkSummary <-
    if boHyperfineBenchmarkJsonPath outputs `elem` targets
      then readHyperfineSummary "Translation Benchmark Fixtures" (boHyperfineBenchmarkJsonPath outputs) (boHyperfineBenchmarkMarkdownPath outputs)
      else pure Nothing
  runtimeAllSummary <-
    if boHyperfineRuntimeAllJsonPath outputs `elem` targets
      then readHyperfineSummary "Runtime All Fixtures" (boHyperfineRuntimeAllJsonPath outputs) (boHyperfineRuntimeAllMarkdownPath outputs)
      else pure Nothing
  runtimeBenchmarkSummary <-
    if boHyperfineRuntimeBenchmarkJsonPath outputs `elem` targets
      then readHyperfineSummary "Runtime Benchmark Fixtures" (boHyperfineRuntimeBenchmarkJsonPath outputs) (boHyperfineRuntimeBenchmarkMarkdownPath outputs)
      else pure Nothing
  pure (catMaybes [allSummary, benchmarkSummary, runtimeAllSummary, runtimeBenchmarkSummary])

renderToolArgument :: ToolName -> Text
renderToolArgument = \case
  ToolMonk -> "monk"
  ToolBabelfish -> "babelfish"

renderRuntimeShellArgument :: RuntimeShell -> Text
renderRuntimeShellArgument = \case
  RuntimeBash -> "bash"
  RuntimeFish -> "fish"

renderSuiteArgument :: BenchmarkSuite -> Text
renderSuiteArgument = \case
  BenchmarkSuiteAll -> "all"
  BenchmarkSuiteBenchmark -> "benchmark"

quoteArg :: Text -> Text
quoteArg txt =
  "'" <> T.replace "'" "'\\''" txt <> "'"

{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.Time (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Bakeoff.Runner (runBakeoff, runBenchmarkWorker)
import Bakeoff.Types (BakeoffConfig (..), BenchmarkSuite (..), FixtureGroup (..), ToolName (..))
import Options.Applicative
import Path (Abs, Dir, Path, parseRelDir, parseRelFile, (</>))
import Path.IO qualified as PathIO
import System.Exit (ExitCode (..))
import System.Exit qualified as Exit

data Command
  = Run RawOptions
  | BenchmarkWorker ToolName FilePath BenchmarkSuite

data RawOptions = RawOptions
  { roOutDir :: Maybe FilePath,
    roGroups :: [FixtureGroup],
    roFiles :: [FilePath],
    roFileLists :: [FilePath],
    roCompatible :: Bool,
    roJobs :: Maybe Int,
    roTranslationTimeoutSeconds :: Int,
    roRuntimeTimeoutSeconds :: Int,
    roNoBenchmark :: Bool,
    roHyperfineRuns :: Int,
    roHyperfineWarmup :: Int,
    roBabelfishPath :: Maybe FilePath,
    roFishPath :: Maybe FilePath,
    roHyperfinePath :: Maybe FilePath,
    roBabelfishVersion :: Maybe Text,
    roForce :: Bool
  }

main :: IO ()
main = do
  parsedCommand <- execParser (info (commandParser <**> helper) (fullDesc <> progDesc "Run Monk vs babelfish bake-offs"))
  case parsedCommand of
    Run raw -> do
      cwd <- PathIO.getCurrentDir
      outputDir <-
        case roOutDir raw of
          Just outDir -> PathIO.resolveDir' outDir
          Nothing -> defaultOutputDirectory
      files <- traverse PathIO.resolveFile' (roFiles raw)
      fileLists <- traverse PathIO.resolveFile' (roFileLists raw)
      compatibleSelector <- parseRelFile "scripts/bakeoff-compatible.txt"
      babelfishPathHint <- traverse PathIO.resolveFile' (roBabelfishPath raw)
      fishPathHint <- traverse PathIO.resolveFile' (roFishPath raw)
      hyperfinePathHint <- traverse PathIO.resolveFile' (roHyperfinePath raw)
      let cfg =
            BakeoffConfig
              { bcCwd = cwd,
                bcOutputDir = outputDir,
                bcForce = roForce raw,
                bcGroups = roGroups raw,
                bcFiles = files,
                bcFileLists = fileLists,
                bcCompatibleFileLists = [cwd </> compatibleSelector | roCompatible raw],
                bcJobs = roJobs raw,
                bcTranslationTimeoutSeconds = roTranslationTimeoutSeconds raw,
                bcRuntimeTimeoutSeconds = roRuntimeTimeoutSeconds raw,
                bcBenchmarksEnabled = not (roNoBenchmark raw),
                bcHyperfineRuns = roHyperfineRuns raw,
                bcHyperfineWarmup = roHyperfineWarmup raw,
                bcBabelfishPathHint = babelfishPathHint,
                bcFishPathHint = fishPathHint,
                bcHyperfinePathHint = hyperfinePathHint,
                bcBabelfishVersionOverride = roBabelfishVersion raw
              }
      runBakeoff cfg
    BenchmarkWorker tool planPath suite -> do
      resolvedPlanPath <- PathIO.resolveFile' planPath
      exitCode <- runBenchmarkWorker tool resolvedPlanPath suite
      Exit.exitWith $
        if exitCode == 0
          then ExitSuccess
          else ExitFailure exitCode

commandParser :: Parser Command
commandParser =
  benchmarkWorkerParser
    <|> (Run <$> rawOptionsParser)

rawOptionsParser :: Parser RawOptions
rawOptionsParser =
  RawOptions
    <$> optional (strOption (long "out-dir" <> metavar "DIR" <> help "Output directory"))
    <*> fmap concat (many groupOptionParser)
    <*> many (strOption (long "file" <> metavar "PATH" <> help "Bake off a specific fixture"))
    <*> many (strOption (long "file-list" <> metavar "PATH" <> help "Read fixture paths from a file"))
    <*> switch (long "compatible" <> help "Use scripts/bakeoff-compatible.txt as an additional selector")
    <*> optional (option auto (long "jobs" <> metavar "N" <> help "Parallel jobs for Shake"))
    <*> option auto (long "translation-timeout-seconds" <> metavar "N" <> value 30 <> showDefault <> help "Per-fixture translation timeout")
    <*> option auto (long "runtime-timeout-seconds" <> metavar "N" <> value 30 <> showDefault <> help "Per-fixture runtime timeout")
    <*> switch (long "no-benchmark" <> help "Skip hyperfine benchmark runs")
    <*> option auto (long "hyperfine-runs" <> metavar "N" <> value 10 <> showDefault <> help "Hyperfine runs per benchmark")
    <*> option auto (long "hyperfine-warmup" <> metavar "N" <> value 1 <> showDefault <> help "Hyperfine warmup runs")
    <*> optional (strOption (long "babelfish" <> metavar "PATH" <> help "Path to babelfish"))
    <*> optional (strOption (long "fish" <> metavar "PATH" <> help "Path to fish"))
    <*> optional (strOption (long "hyperfine" <> metavar "PATH" <> help "Path to hyperfine"))
    <*> optional (strOption (long "babelfish-version" <> metavar "TEXT" <> help "Override babelfish version in metadata"))
    <*> switch (long "force" <> help "Overwrite an existing non-empty output directory")

groupOptionParser :: Parser [FixtureGroup]
groupOptionParser =
  option
    (eitherReader parseFixtureGroup)
    (long "group" <> metavar "GROUP" <> help "Select a fixture group (all|corpus|benchmark|integration|golden|realworld)")

parseFixtureGroup :: String -> Either String [FixtureGroup]
parseFixtureGroup = \case
  "all" -> Right [FixtureGroupCorpus, FixtureGroupBenchmark, FixtureGroupIntegration, FixtureGroupGolden, FixtureGroupRealWorld]
  "corpus" -> Right [FixtureGroupCorpus]
  "benchmark" -> Right [FixtureGroupBenchmark]
  "integration" -> Right [FixtureGroupIntegration]
  "golden" -> Right [FixtureGroupGolden]
  "realworld" -> Right [FixtureGroupRealWorld]
  other -> Left ("invalid group: " <> other)

benchmarkWorkerParser :: Parser Command
benchmarkWorkerParser =
  hsubparser
    ( command
        "_benchmark-worker"
        ( info
            (BenchmarkWorker <$> toolParser <*> planParser <*> suiteParser)
            (progDesc "Internal hyperfine worker")
        )
        <> internal
    )
  where
    toolParser =
      option
        (eitherReader parseToolName)
        (long "tool" <> metavar "TOOL")
    planParser =
      strOption
        (long "plan" <> metavar "PATH")
    suiteParser =
      option
        (eitherReader parseBenchmarkSuite)
        (long "suite" <> metavar "SUITE")

parseToolName :: String -> Either String ToolName
parseToolName = \case
  "monk" -> Right ToolMonk
  "babelfish" -> Right ToolBabelfish
  other -> Left ("invalid tool: " <> other)

parseBenchmarkSuite :: String -> Either String BenchmarkSuite
parseBenchmarkSuite = \case
  "all" -> Right BenchmarkSuiteAll
  "benchmark" -> Right BenchmarkSuiteBenchmark
  other -> Left ("invalid suite: " <> other)

defaultOutputDirectory :: IO (Path Abs Dir)
defaultOutputDirectory = do
  now <- getCurrentTime
  tmpDir <- PathIO.getTempDir
  relDir <- parseRelDir ("monk-babelfish-" <> formatTime defaultTimeLocale "%Y%m%d%H%M%S" now <> "/")
  pure (tmpDir </> relDir)

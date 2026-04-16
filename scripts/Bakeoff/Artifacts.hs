module Bakeoff.Artifacts
  ( BakeoffOutputs (..),
    FixtureArtifacts (..),
    prepareOutputDirectory,
    bakeoffOutputs,
    fixtureArtifacts,
    ensureParentDirectory,
    writeTextFile,
  )
where

import Bakeoff.Types
import Path
  ( Abs,
    Dir,
    File,
    Path,
    filename,
    parent,
    parseRelDir,
    parseRelFile,
    splitExtension,
    toFilePath,
    (</>),
  )
import Path.IO qualified as PathIO

data BakeoffOutputs = BakeoffOutputs
  { boShakeDir :: Path Abs Dir,
    boMetaPath :: Path Abs File,
    boBenchmarkPlanPath :: Path Abs File,
    boReportPath :: Path Abs File,
    boSummaryPath :: Path Abs File,
    boHyperfineAllJsonPath :: Path Abs File,
    boHyperfineAllMarkdownPath :: Path Abs File,
    boHyperfineBenchmarkJsonPath :: Path Abs File,
    boHyperfineBenchmarkMarkdownPath :: Path Abs File
  }

data FixtureArtifacts = FixtureArtifacts
  { faDir :: Path Abs Dir,
    faBase :: String,
    faMonkFish :: Path Abs File,
    faMonkTranslateJson :: Path Abs File,
    faMonkTranslateStderr :: Path Abs File,
    faBabelfishFish :: Path Abs File,
    faBabelfishTranslateJson :: Path Abs File,
    faBabelfishTranslateStderr :: Path Abs File,
    faMonkStdout :: Path Abs File,
    faMonkRuntimeStderr :: Path Abs File,
    faMonkRuntimeJson :: Path Abs File,
    faMonkExitCode :: Path Abs File,
    faBabelfishStdout :: Path Abs File,
    faBabelfishRuntimeStderr :: Path Abs File,
    faBabelfishRuntimeJson :: Path Abs File,
    faBabelfishExitCode :: Path Abs File,
    faMonkRuntimeStderrNorm :: Path Abs File,
    faBabelfishRuntimeStderrNorm :: Path Abs File,
    faStdoutDiff :: Path Abs File,
    faStderrDiff :: Path Abs File,
    faExitCodeDiff :: Path Abs File,
    faDiffJson :: Path Abs File,
    faResultJson :: Path Abs File
  }

prepareOutputDirectory :: Path Abs Dir -> Bool -> IO ()
prepareOutputDirectory outputDir overwrite = do
  exists <- PathIO.doesDirExist outputDir
  if not exists
    then PathIO.ensureDir outputDir
    else do
      (_, files) <- PathIO.listDir outputDir
      if null files
        then pure ()
        else
          if overwrite
            then do
              PathIO.removeDirRecur outputDir
              PathIO.ensureDir outputDir
            else
              fail ("output directory exists and is not empty: " <> toFilePath outputDir)

bakeoffOutputs :: Path Abs Dir -> IO BakeoffOutputs
bakeoffOutputs outputDir = do
  shakeDir <- (outputDir </>) <$> parseRelDir ".shake/"
  metaPath <- (outputDir </>) <$> parseRelFile "meta.json"
  benchmarkPlanPath <- (outputDir </>) <$> parseRelFile "benchmark-plan.json"
  reportPath <- (outputDir </>) <$> parseRelFile "report.json"
  summaryPath <- (outputDir </>) <$> parseRelFile "summary.md"
  hyperfineAllJsonPath <- (outputDir </>) <$> parseRelFile "hyperfine-all.json"
  hyperfineAllMarkdownPath <- (outputDir </>) <$> parseRelFile "hyperfine-all.md"
  hyperfineBenchmarkJsonPath <- (outputDir </>) <$> parseRelFile "hyperfine-benchmark.json"
  hyperfineBenchmarkMarkdownPath <- (outputDir </>) <$> parseRelFile "hyperfine-benchmark.md"
  pure
    BakeoffOutputs
      { boShakeDir = shakeDir,
        boMetaPath = metaPath,
        boBenchmarkPlanPath = benchmarkPlanPath,
        boReportPath = reportPath,
        boSummaryPath = summaryPath,
        boHyperfineAllJsonPath = hyperfineAllJsonPath,
        boHyperfineAllMarkdownPath = hyperfineAllMarkdownPath,
        boHyperfineBenchmarkJsonPath = hyperfineBenchmarkJsonPath,
        boHyperfineBenchmarkMarkdownPath = hyperfineBenchmarkMarkdownPath
      }

fixtureArtifacts :: BakeoffConfig -> FixtureSpec -> IO FixtureArtifacts
fixtureArtifacts cfg fixture = do
  let dir = bcOutputDir cfg </> fsArtifactDir fixture
  base <- fixtureBaseStem (fsPath fixture)
  monkFish <- artifactFile dir (base <> ".monk.fish")
  monkTranslateJson <- artifactFile dir "monk.translate.json"
  monkTranslateStderr <- artifactFile dir (base <> ".monk.err")
  babelfishFish <- artifactFile dir (base <> ".babelfish.fish")
  babelfishTranslateJson <- artifactFile dir "babelfish.translate.json"
  babelfishTranslateStderr <- artifactFile dir (base <> ".babelfish.err")
  monkStdout <- artifactFile dir (base <> ".monk.out")
  monkRuntimeStderr <- artifactFile dir (base <> ".monk.runerr")
  monkRuntimeJson <- artifactFile dir "monk.runtime.json"
  monkExitCode <- artifactFile dir (base <> ".monk.rc")
  babelfishStdout <- artifactFile dir (base <> ".babelfish.out")
  babelfishRuntimeStderr <- artifactFile dir (base <> ".babelfish.runerr")
  babelfishRuntimeJson <- artifactFile dir "babelfish.runtime.json"
  babelfishExitCode <- artifactFile dir (base <> ".babelfish.rc")
  monkRuntimeStderrNorm <- artifactFile dir (base <> ".monk.runerr.norm")
  babelfishRuntimeStderrNorm <- artifactFile dir (base <> ".babelfish.runerr.norm")
  stdoutDiff <- artifactFile dir "stdout.diff"
  stderrDiff <- artifactFile dir "stderr.diff"
  exitCodeDiff <- artifactFile dir "exit.diff"
  diffJson <- artifactFile dir "diff.json"
  resultJson <- artifactFile dir "result.json"
  pure
    FixtureArtifacts
      { faDir = dir,
        faBase = base,
        faMonkFish = monkFish,
        faMonkTranslateJson = monkTranslateJson,
        faMonkTranslateStderr = monkTranslateStderr,
        faBabelfishFish = babelfishFish,
        faBabelfishTranslateJson = babelfishTranslateJson,
        faBabelfishTranslateStderr = babelfishTranslateStderr,
        faMonkStdout = monkStdout,
        faMonkRuntimeStderr = monkRuntimeStderr,
        faMonkRuntimeJson = monkRuntimeJson,
        faMonkExitCode = monkExitCode,
        faBabelfishStdout = babelfishStdout,
        faBabelfishRuntimeStderr = babelfishRuntimeStderr,
        faBabelfishRuntimeJson = babelfishRuntimeJson,
        faBabelfishExitCode = babelfishExitCode,
        faMonkRuntimeStderrNorm = monkRuntimeStderrNorm,
        faBabelfishRuntimeStderrNorm = babelfishRuntimeStderrNorm,
        faStdoutDiff = stdoutDiff,
        faStderrDiff = stderrDiff,
        faExitCodeDiff = exitCodeDiff,
        faDiffJson = diffJson,
        faResultJson = resultJson
      }

fixtureBaseStem :: Path Abs File -> IO String
fixtureBaseStem path = do
  (stem, _) <- splitExtension (filename path)
  pure (toFilePath stem)

artifactFile :: Path Abs Dir -> FilePath -> IO (Path Abs File)
artifactFile dir name =
  (dir </>) <$> parseRelFile name

ensureParentDirectory :: Path b File -> IO ()
ensureParentDirectory =
  PathIO.ensureDir . parent

writeTextFile :: Path b File -> Text -> IO ()
writeTextFile path =
  writeFileText (toFilePath path)

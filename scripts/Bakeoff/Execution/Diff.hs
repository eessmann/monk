module Bakeoff.Execution.Diff
  ( buildDiffReport,
  )
where

import Bakeoff.Artifacts
  ( FixtureArtifacts (..),
    writeTextFile,
  )
import Bakeoff.Process
  ( normalizeRuntimeStderr,
    writeComparisonFile,
  )
import Bakeoff.Types
import Data.Text.IO qualified as TIO
import Path
  ( Abs,
    File,
    Path,
    toFilePath,
  )

buildDiffReport :: BakeoffConfig -> FixtureArtifacts -> RuntimeReport -> RuntimeReport -> IO DiffReport
buildDiffReport cfg artifacts monkRuntime babelfishRuntime = do
  stdoutDiff <- comparePlainArtifacts (runtimeStdoutPath babelfishRuntime) (runtimeStdoutPath monkRuntime) (faStdoutDiff artifacts) "babelfish stdout" "monk stdout"
  stderrDiff <-
    compareNormalizedArtifacts
      (runtimeStderrPath babelfishRuntime)
      (runtimeStderrPath monkRuntime)
      (faBabelfishRuntimeStderrNorm artifacts)
      (faMonkRuntimeStderrNorm artifacts)
      (faStderrDiff artifacts)
      (normalizeRuntimeStderr (bakeoffOutputDir cfg))
      "babelfish stderr"
      "monk stderr"
  exitDiff <-
    compareExitCodes
      (runtimeExitCode babelfishRuntime)
      (runtimeExitCode monkRuntime)
      (faExitCodeDiff artifacts)
  pure
    MkDiffReport
      { diffStdout = stdoutDiff,
        diffStderr = stderrDiff,
        diffExitCode = exitDiff
      }

comparePlainArtifacts ::
  Maybe (Path Abs File) ->
  Maybe (Path Abs File) ->
  Path Abs File ->
  Text ->
  Text ->
  IO DiffArtifact
comparePlainArtifacts leftPath rightPath diffPath leftLabel rightLabel =
  case (leftPath, rightPath) of
    (Just left, Just right) -> do
      leftText <- TIO.readFile (toFilePath left)
      rightText <- TIO.readFile (toFilePath right)
      if leftText == rightText
        then pure (MkDiffArtifact DiffNone Nothing)
        else do
          writeComparisonFile diffPath leftLabel leftText rightLabel rightText
          pure (MkDiffArtifact DiffDifferent (Just diffPath))
    _ -> pure (MkDiffArtifact DiffUnavailable Nothing)

compareNormalizedArtifacts ::
  Maybe (Path Abs File) ->
  Maybe (Path Abs File) ->
  Path Abs File ->
  Path Abs File ->
  Path Abs File ->
  (Text -> Text) ->
  Text ->
  Text ->
  IO DiffArtifact
compareNormalizedArtifacts leftPath rightPath leftNormPath rightNormPath diffPath normalizeFn leftLabel rightLabel =
  case (leftPath, rightPath) of
    (Just left, Just right) -> do
      leftText <- normalizeFn <$> TIO.readFile (toFilePath left)
      rightText <- normalizeFn <$> TIO.readFile (toFilePath right)
      writeTextFile leftNormPath leftText
      writeTextFile rightNormPath rightText
      if leftText == rightText
        then pure (MkDiffArtifact DiffNone Nothing)
        else do
          writeComparisonFile diffPath leftLabel leftText rightLabel rightText
          pure (MkDiffArtifact DiffDifferent (Just diffPath))
    _ -> pure (MkDiffArtifact DiffUnavailable Nothing)

compareExitCodes :: Maybe Int -> Maybe Int -> Path Abs File -> IO DiffArtifact
compareExitCodes leftExit rightExit diffPath =
  case (leftExit, rightExit) of
    (Just left, Just right)
      | left == right -> pure (MkDiffArtifact DiffNone Nothing)
      | otherwise -> do
          writeComparisonFile diffPath "babelfish exit" (show right <> "\n") "monk exit" (show left <> "\n")
          pure (MkDiffArtifact DiffDifferent (Just diffPath))
    _ -> pure (MkDiffArtifact DiffUnavailable Nothing)

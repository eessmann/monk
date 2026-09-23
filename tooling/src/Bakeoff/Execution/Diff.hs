module Bakeoff.Execution.Diff
  ( buildDiffReport,
  )
where

import Bakeoff.Artifacts
  ( FixtureArtifacts (..),
  )
import Bakeoff.Process
  ( normalizeRuntimeStderr,
    writeComparisonFile,
  )
import Bakeoff.Types
import Data.ByteString qualified as B
import Data.Text.Encoding qualified as TE
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
      leftBytes <- B.readFile (toFilePath left)
      rightBytes <- B.readFile (toFilePath right)
      if leftBytes == rightBytes
        then pure (MkDiffArtifact DiffNone Nothing)
        else do
          writeComparisonFile diffPath leftLabel (displayBytes leftBytes) rightLabel (displayBytes rightBytes)
          pure (MkDiffArtifact DiffDifferent (Just diffPath))
    _ -> pure (MkDiffArtifact DiffUnavailable Nothing)

compareNormalizedArtifacts ::
  Maybe (Path Abs File) ->
  Maybe (Path Abs File) ->
  Path Abs File ->
  Path Abs File ->
  Path Abs File ->
  (B.ByteString -> B.ByteString) ->
  Text ->
  Text ->
  IO DiffArtifact
compareNormalizedArtifacts leftPath rightPath leftNormPath rightNormPath diffPath normalizeFn leftLabel rightLabel =
  case (leftPath, rightPath) of
    (Just left, Just right) -> do
      leftBytes <- normalizeFn <$> B.readFile (toFilePath left)
      rightBytes <- normalizeFn <$> B.readFile (toFilePath right)
      B.writeFile (toFilePath leftNormPath) leftBytes
      B.writeFile (toFilePath rightNormPath) rightBytes
      if leftBytes == rightBytes
        then pure (MkDiffArtifact DiffNone Nothing)
        else do
          writeComparisonFile diffPath leftLabel (displayBytes leftBytes) rightLabel (displayBytes rightBytes)
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

-- Text is only a presentation of already-compared bytes. Escapes retain every
-- differing byte when a stream is not UTF8.
displayBytes :: B.ByteString -> Text
displayBytes bytes = fromRight ("Non-UTF8 bytes: " <> show bytes) (TE.decodeUtf8' bytes)

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bakeoff.Report
  ( readHyperfineSummary,
    renderSummaryMarkdown,
  )
where

import Data.Aeson (FromJSON)
import Data.Text qualified as T
import Bakeoff.Process (readJsonFile)
import Bakeoff.Types
import Numeric qualified
import Path (Abs, Dir, File, Path, Rel, toFilePath)
import Path.IO qualified as PathIO
import System.FilePath (dropTrailingPathSeparator)

data HyperfineEnvelope = MkHyperfineEnvelope
  { results :: [HyperfineEntry]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data HyperfineEntry = MkHyperfineEntry
  { command :: Text,
    mean :: Double,
    stddev :: Double
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

readHyperfineSummary :: Text -> Path Abs File -> Path Abs File -> IO (Maybe HyperfineSummary)
readHyperfineSummary title jsonPath markdownPath = do
  exists <- PathIO.doesFileExist jsonPath
  if not exists
    then pure Nothing
    else do
      MkHyperfineEnvelope {results} <- readJsonFile jsonPath
      pure $
        Just
          MkHyperfineSummary
            { hyperfineTitle = title,
              hyperfineJsonPath = jsonPath,
              hyperfineMarkdownPath = markdownPath,
              hyperfineResults =
                [ MkHyperfineResult
                    { hyperfineCommand = command entry,
                      hyperfineMean = mean entry,
                      hyperfineStddev = stddev entry
                    }
                  | entry <- results
                ]
            }

renderSummaryMarkdown :: MetaReport -> [FixtureReport] -> [HyperfineSummary] -> Text
renderSummaryMarkdown meta fixtures benchmarkSummaries =
  T.unlines $
    [ "# Monk Bake-off",
      "",
      "## Run",
      "",
      "- Date: " <> show (metaTimestamp meta),
      "- CWD: `" <> dirText (metaCwd meta) <> "`",
      "- Output dir: `" <> dirText (metaOutputDir meta) <> "`",
      "- Host: `" <> metaHostOs meta <> "/" <> metaHostArch meta <> "`",
      "- Monk executable: `" <> fileText (toolsMonkExecutable (metaTools meta)) <> "`",
      "- Babelfish: `" <> fileText (toolsBabelfishPath (metaTools meta)) <> "` (" <> toolsBabelfishVersion (metaTools meta) <> ")",
      "- Fish: `" <> fileText (toolsFishPath (metaTools meta)) <> "` (" <> toolsFishVersion (metaTools meta) <> ")"
    ]
      <> hyperfineRunLine
      <> [ "",
           "## Summary",
           "",
           "- Total fixtures: " <> show totalFixtures,
           "- Skipped fixtures: " <> show skippedFixtures,
           "- Monk translation: " <> renderStatusCounts monkTranslationCounts,
           "- Babelfish translation: " <> renderStatusCounts babelfishTranslationCounts,
           "- Monk runtime: " <> renderStatusCounts monkRuntimeCounts,
           "- Babelfish runtime: " <> renderStatusCounts babelfishRuntimeCounts,
           "- Fixtures with any runtime diff: " <> show mismatchingFixtures
         ]
      <> skipSection
      <> mismatchSection
      <> benchmarkSection
  where
    totalFixtures = length fixtures
    skippedFixtures = length (filter (isJust . fixtureReportSkipReason) fixtures)
    monkTranslationCounts = collectTranslationCounts fixtureReportMonkTranslation fixtures
    babelfishTranslationCounts = collectTranslationCounts fixtureReportBabelfishTranslation fixtures
    monkRuntimeCounts = collectRuntimeCounts fixtureReportMonkRuntime fixtures
    babelfishRuntimeCounts = collectRuntimeCounts fixtureReportBabelfishRuntime fixtures
    mismatches = filter fixtureHasMismatch fixtures
    mismatchingFixtures = length mismatches
    skipCounts = countSkipReasons fixtures
    hyperfineRunLine =
      case toolsHyperfineVersion (metaTools meta) of
        Nothing -> []
        Just version -> ["- Hyperfine: `" <> version <> "`"]
    skipSection
      | null skipCounts = []
      | otherwise =
          [ "",
            "## Skips",
            ""
          ]
            <> map renderSkipCount skipCounts
    mismatchSection
      | null mismatches = []
      | otherwise =
          [ "",
            "## Mismatches",
            ""
          ]
            <> take 10 (map renderMismatch mismatches)
    benchmarkSection
      | null benchmarkSummaries = []
      | otherwise =
          [ "",
            "## Benchmarks",
            ""
          ]
            <> concatMap renderBenchmark benchmarkSummaries

renderStatusCounts :: [(CommandStatus, Int)] -> Text
renderStatusCounts counts =
  T.intercalate
    ", "
    [ renderPair status count
      | (status, count) <- counts
    ]
  where
    renderPair status count = renderCommandStatus status <> "=" <> show count

collectTranslationCounts ::
  (FixtureReport -> Maybe TranslationReport) ->
  [FixtureReport] ->
  [(CommandStatus, Int)]
collectTranslationCounts project fixtures =
  countStatuses (map (fmap translationStatus . project) fixtures)

collectRuntimeCounts ::
  (FixtureReport -> Maybe RuntimeReport) ->
  [FixtureReport] ->
  [(CommandStatus, Int)]
collectRuntimeCounts project fixtures =
  countStatuses (map (fmap runtimeStatus . project) fixtures)

countStatuses :: [Maybe CommandStatus] -> [(CommandStatus, Int)]
countStatuses statuses =
  [ (status, length (filter (== Just status) statuses))
    | status <- [CommandSucceeded, CommandFailed, CommandTimedOut, CommandSkipped]
  ]

countSkipReasons :: [FixtureReport] -> [(Text, Int)]
countSkipReasons fixtures =
  map summarize grouped
  where
    grouped =
      group
        . sort
        $ [ renderSkipReason reason
            | fixture <- fixtures,
              Just reason <- [fixtureReportSkipReason fixture]
          ]
    summarize [] = ("", 0)
    summarize (reason : rest) = (reason, 1 + length rest)

renderSkipCount :: (Text, Int) -> Text
renderSkipCount (reason, count) =
  "- " <> reason <> ": " <> show count

fixtureHasMismatch :: FixtureReport -> Bool
fixtureHasMismatch fixture =
  case fixtureReportDiff fixture of
    Just MkDiffReport {..} ->
      any ((== DiffDifferent) . diffArtifactStatus) [diffStdout, diffStderr, diffExitCode]
    Nothing ->
      any translationProblem [fixtureReportMonkTranslation fixture, fixtureReportBabelfishTranslation fixture]
        || any runtimeProblem [fixtureReportMonkRuntime fixture, fixtureReportBabelfishRuntime fixture]
  where
    translationProblem = \case
      Just report -> translationStatus report /= CommandSucceeded
      Nothing -> False
    runtimeProblem = \case
      Just report -> runtimeStatus report /= CommandSucceeded
      Nothing -> False

renderMismatch :: FixtureReport -> Text
renderMismatch fixture =
  "- `" <> relFileText (fixtureReportRelativePath fixture) <> "`: " <> mismatchReason fixture

mismatchReason :: FixtureReport -> Text
mismatchReason fixture =
  case fixtureReportDiff fixture of
    Just MkDiffReport {..}
      | any ((== DiffDifferent) . diffArtifactStatus) [diffStdout, diffStderr, diffExitCode] ->
          T.intercalate ", " (catMaybes [renderDiff "stdout" diffStdout, renderDiff "stderr" diffStderr, renderDiff "exit" diffExitCode])
    _ ->
      T.intercalate ", " (catMaybes [renderTranslationState ToolMonk (fixtureReportMonkTranslation fixture), renderTranslationState ToolBabelfish (fixtureReportBabelfishTranslation fixture), renderRuntimeState ToolMonk (fixtureReportMonkRuntime fixture), renderRuntimeState ToolBabelfish (fixtureReportBabelfishRuntime fixture)])
  where
    renderDiff label diffArtifact
      | diffArtifactStatus diffArtifact == DiffDifferent = Just label
      | otherwise = Nothing

renderTranslationState :: ToolName -> Maybe TranslationReport -> Maybe Text
renderTranslationState tool = \case
  Just report
    | translationStatus report /= CommandSucceeded ->
        Just (renderToolName tool <> " translation " <> renderCommandStatus (translationStatus report))
  _ -> Nothing

renderRuntimeState :: ToolName -> Maybe RuntimeReport -> Maybe Text
renderRuntimeState tool = \case
  Just report
    | runtimeStatus report /= CommandSucceeded ->
        Just (renderToolName tool <> " runtime " <> renderCommandStatus (runtimeStatus report))
  _ -> Nothing

renderBenchmark :: HyperfineSummary -> [Text]
renderBenchmark summary =
  [ "### " <> hyperfineTitle summary,
    "",
    "- JSON: `" <> fileText (hyperfineJsonPath summary) <> "`",
    "- Markdown: `" <> fileText (hyperfineMarkdownPath summary) <> "`"
  ]
    <> [ "- " <> hyperfineCommand result <> ": mean=" <> formatSeconds (hyperfineMean result) <> "s, stddev=" <> formatSeconds (hyperfineStddev result) <> "s"
         | result <- hyperfineResults summary
       ]
    <> [""]

formatSeconds :: Double -> Text
formatSeconds value =
  showFFloat 3 value

renderCommandStatus :: CommandStatus -> Text
renderCommandStatus = \case
  CommandSucceeded -> "succeeded"
  CommandFailed -> "failed"
  CommandTimedOut -> "timed_out"
  CommandSkipped -> "skipped"

renderToolName :: ToolName -> Text
renderToolName = \case
  ToolMonk -> "monk"
  ToolBabelfish -> "babelfish"

renderSkipReason :: SkipReason -> Text
renderSkipReason = \case
  SkipPlatformMismatch current allowed ->
    "platform mismatch (" <> current <> " not in " <> T.intercalate ", " allowed <> ")"
  SkipMissingPrereqs missing ->
    "missing prerequisites (" <> T.intercalate ", " (map toText missing) <> ")"

showFFloat :: Int -> Double -> Text
showFFloat digits value =
  T.pack (Numeric.showFFloat (Just digits) value "")

fileText :: Path b File -> Text
fileText =
  toText . toFilePath

relFileText :: Path Rel File -> Text
relFileText =
  fileText

dirText :: Path b Dir -> Text
dirText =
  toText . dropTrailingPathSeparator . toFilePath

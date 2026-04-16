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

data HyperfineEnvelope = HyperfineEnvelope
  { results :: [HyperfineEntry]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data HyperfineEntry = HyperfineEntry
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
      HyperfineEnvelope {results} <- readJsonFile jsonPath
      pure $
        Just
          HyperfineSummary
            { hsTitle = title,
              hsJsonPath = jsonPath,
              hsMarkdownPath = markdownPath,
              hsResults =
                [ HyperfineResult
                    { hrCommand = command entry,
                      hrMean = mean entry,
                      hrStddev = stddev entry
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
      "- Date: " <> show (mrTimestamp meta),
      "- CWD: `" <> dirText (mrCwd meta) <> "`",
      "- Output dir: `" <> dirText (mrOutputDir meta) <> "`",
      "- Host: `" <> mrHostOs meta <> "/" <> mrHostArch meta <> "`",
      "- Monk executable: `" <> fileText (rtMonkExecutable (mrTools meta)) <> "`",
      "- Babelfish: `" <> fileText (rtBabelfishPath (mrTools meta)) <> "` (" <> rtBabelfishVersion (mrTools meta) <> ")",
      "- Fish: `" <> fileText (rtFishPath (mrTools meta)) <> "` (" <> rtFishVersion (mrTools meta) <> ")"
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
    skippedFixtures = length (filter (isJust . frSkipReason) fixtures)
    monkTranslationCounts = collectTranslationCounts frMonkTranslation fixtures
    babelfishTranslationCounts = collectTranslationCounts frBabelfishTranslation fixtures
    monkRuntimeCounts = collectRuntimeCounts frMonkRuntime fixtures
    babelfishRuntimeCounts = collectRuntimeCounts frBabelfishRuntime fixtures
    mismatches = filter fixtureHasMismatch fixtures
    mismatchingFixtures = length mismatches
    skipCounts = countSkipReasons fixtures
    hyperfineRunLine =
      case rtHyperfineVersion (mrTools meta) of
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
  countStatuses (map (fmap trStatus . project) fixtures)

collectRuntimeCounts ::
  (FixtureReport -> Maybe RuntimeReport) ->
  [FixtureReport] ->
  [(CommandStatus, Int)]
collectRuntimeCounts project fixtures =
  countStatuses (map (fmap rrStatus . project) fixtures)

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
              Just reason <- [frSkipReason fixture]
          ]
    summarize [] = ("", 0)
    summarize (reason : rest) = (reason, 1 + length rest)

renderSkipCount :: (Text, Int) -> Text
renderSkipCount (reason, count) =
  "- " <> reason <> ": " <> show count

fixtureHasMismatch :: FixtureReport -> Bool
fixtureHasMismatch fixture =
  case frDiff fixture of
    Just DiffReport {..} ->
      any ((== DiffDifferent) . daStatus) [drStdout, drStderr, drExitCode]
    Nothing ->
      any translationProblem [frMonkTranslation fixture, frBabelfishTranslation fixture]
        || any runtimeProblem [frMonkRuntime fixture, frBabelfishRuntime fixture]
  where
    translationProblem = \case
      Just report -> trStatus report /= CommandSucceeded
      Nothing -> False
    runtimeProblem = \case
      Just report -> rrStatus report /= CommandSucceeded
      Nothing -> False

renderMismatch :: FixtureReport -> Text
renderMismatch fixture =
  "- `" <> relFileText (frRelativePath fixture) <> "`: " <> mismatchReason fixture

mismatchReason :: FixtureReport -> Text
mismatchReason fixture =
  case frDiff fixture of
    Just DiffReport {..}
      | any ((== DiffDifferent) . daStatus) [drStdout, drStderr, drExitCode] ->
          T.intercalate ", " (catMaybes [renderDiff "stdout" drStdout, renderDiff "stderr" drStderr, renderDiff "exit" drExitCode])
    _ ->
      T.intercalate ", " (catMaybes [renderTranslationState ToolMonk (frMonkTranslation fixture), renderTranslationState ToolBabelfish (frBabelfishTranslation fixture), renderRuntimeState ToolMonk (frMonkRuntime fixture), renderRuntimeState ToolBabelfish (frBabelfishRuntime fixture)])
  where
    renderDiff label diffArtifact
      | daStatus diffArtifact == DiffDifferent = Just label
      | otherwise = Nothing

renderTranslationState :: ToolName -> Maybe TranslationReport -> Maybe Text
renderTranslationState tool = \case
  Just report
    | trStatus report /= CommandSucceeded ->
        Just (renderToolName tool <> " translation " <> renderCommandStatus (trStatus report))
  _ -> Nothing

renderRuntimeState :: ToolName -> Maybe RuntimeReport -> Maybe Text
renderRuntimeState tool = \case
  Just report
    | rrStatus report /= CommandSucceeded ->
        Just (renderToolName tool <> " runtime " <> renderCommandStatus (rrStatus report))
  _ -> Nothing

renderBenchmark :: HyperfineSummary -> [Text]
renderBenchmark summary =
  [ "### " <> hsTitle summary,
    "",
    "- JSON: `" <> fileText (hsJsonPath summary) <> "`",
    "- Markdown: `" <> fileText (hsMarkdownPath summary) <> "`"
  ]
    <> [ "- " <> hrCommand result <> ": mean=" <> formatSeconds (hrMean result) <> "s, stddev=" <> formatSeconds (hrStddev result) <> "s"
         | result <- hsResults summary
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

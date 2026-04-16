{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Bakeoff.Execution
  ( defineFixtureRules,
    defineBenchmarkRules,
    runWorkerFixture,
  )
where

import Control.Exception (evaluate)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Bakeoff.Artifacts
  ( BakeoffOutputs (..),
    FixtureArtifacts (..),
    ensureParentDirectory,
    writeTextFile,
  )
import Bakeoff.Benchmark (runHyperfineSuite)
import Development.Shake
import Bakeoff.Fixture (FixtureMetadata (..), loadFixtureRecursive)
import Bakeoff.Process
import Bakeoff.Selection (summarizeFixtureMetadata)
import Bakeoff.Shell (ShellRunMode (..))
import Bakeoff.Types
import Monk.Diagnostics
  ( WarningCounts (..),
    confidenceScore,
    renderParseComment,
    renderTranslateError,
    renderTranslationNotes,
    renderWarning,
    summarizeWarnings,
    translationNoteCount,
  )
import Monk.Source
  ( SourceGraph (..),
    SourceGraphFailure (..),
    translateSourceGraph,
  )
import Monk.Translation
  ( Translation (..),
    Warning,
    defaultConfig,
    inlineStatements,
    renderFish,
    stateWarnings,
  )
import Path
  ( Abs,
    File,
    Path,
    toFilePath,
  )
import Path.IO qualified as PathIO
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (env), proc)
import System.Timeout (timeout)

data MonkTranslationArtifact = MkMonkTranslationArtifact
  { mtaOutput :: Text,
    mtaWarnings :: [Warning],
    mtaNoteCount :: Int,
    mtaStderrLines :: [Text]
  }

defineFixtureRules :: BakeoffConfig -> ResolvedTools -> [(String, String)] -> FixtureSpec -> FixtureArtifacts -> Rules ()
defineFixtureRules cfg tools processEnv fixture artifacts = do
  toFilePath (faMonkTranslateJson artifacts) %> \_ -> do
    need [toFilePath (specPath fixture)]
    liftIO (PathIO.ensureDir (faDir artifacts))
    report <- liftIO $ buildMonkTranslationReport cfg fixture artifacts
    liftIO $ do
      ensureParentDirectory (faMonkTranslateJson artifacts)
      writeJsonFile (faMonkTranslateJson artifacts) report

  toFilePath (faBabelfishTranslateJson artifacts) %> \_ -> do
    need [toFilePath (specPath fixture)]
    liftIO (PathIO.ensureDir (faDir artifacts))
    report <- liftIO $ buildBabelfishTranslationReport cfg tools fixture artifacts processEnv
    liftIO $ do
      ensureParentDirectory (faBabelfishTranslateJson artifacts)
      writeJsonFile (faBabelfishTranslateJson artifacts) report

  toFilePath (faMonkRuntimeJson artifacts) %> \_ -> do
    need [toFilePath (faMonkTranslateJson artifacts)]
    liftIO (PathIO.ensureDir (faDir artifacts))
    translationReport <- liftIO $ readJsonFile (faMonkTranslateJson artifacts)
    report <- liftIO $ buildRuntimeReport cfg tools fixture artifacts ToolMonk translationReport processEnv
    liftIO $ do
      ensureParentDirectory (faMonkRuntimeJson artifacts)
      writeJsonFile (faMonkRuntimeJson artifacts) report

  toFilePath (faBabelfishRuntimeJson artifacts) %> \_ -> do
    need [toFilePath (faBabelfishTranslateJson artifacts)]
    liftIO (PathIO.ensureDir (faDir artifacts))
    translationReport <- liftIO $ readJsonFile (faBabelfishTranslateJson artifacts)
    report <- liftIO $ buildRuntimeReport cfg tools fixture artifacts ToolBabelfish translationReport processEnv
    liftIO $ do
      ensureParentDirectory (faBabelfishRuntimeJson artifacts)
      writeJsonFile (faBabelfishRuntimeJson artifacts) report

  toFilePath (faDiffJson artifacts) %> \_ -> do
    need [toFilePath (faMonkRuntimeJson artifacts), toFilePath (faBabelfishRuntimeJson artifacts)]
    liftIO (PathIO.ensureDir (faDir artifacts))
    monkRuntime <- liftIO $ readJsonFile (faMonkRuntimeJson artifacts)
    babelfishRuntime <- liftIO $ readJsonFile (faBabelfishRuntimeJson artifacts)
    report <- liftIO $ buildDiffReport cfg artifacts monkRuntime babelfishRuntime
    liftIO $ do
      ensureParentDirectory (faDiffJson artifacts)
      writeJsonFile (faDiffJson artifacts) report

  toFilePath (faResultJson artifacts) %> \_ -> do
    need
      [ toFilePath (faMonkTranslateJson artifacts),
        toFilePath (faBabelfishTranslateJson artifacts),
        toFilePath (faMonkRuntimeJson artifacts),
        toFilePath (faBabelfishRuntimeJson artifacts),
        toFilePath (faDiffJson artifacts)
      ]
    monkTranslation <- liftIO $ readJsonFile (faMonkTranslateJson artifacts)
    babelfishTranslation <- liftIO $ readJsonFile (faBabelfishTranslateJson artifacts)
    monkRuntime <- liftIO $ readJsonFile (faMonkRuntimeJson artifacts)
    babelfishRuntime <- liftIO $ readJsonFile (faBabelfishRuntimeJson artifacts)
    diffReport <- liftIO $ readJsonFile (faDiffJson artifacts)
    let fixtureReport =
          MkFixtureReport
            { fixtureReportPath = specPath fixture,
              fixtureReportRelativePath = specRelativePath fixture,
              fixtureReportArtifactDir = specArtifactDir fixture,
              fixtureReportGroup = specGroup fixture,
              fixtureReportSelectionSources = specSelectionSources fixture,
              fixtureReportMetadata = summarizeFixtureMetadata (specMetadata fixture),
              fixtureReportSkipReason = specSkipReason fixture,
              fixtureReportMonkTranslation = Just monkTranslation,
              fixtureReportBabelfishTranslation = Just babelfishTranslation,
              fixtureReportMonkRuntime = Just monkRuntime,
              fixtureReportBabelfishRuntime = Just babelfishRuntime,
              fixtureReportDiff = Just diffReport
            }
    liftIO $ do
      ensureParentDirectory (faResultJson artifacts)
      writeJsonFile (faResultJson artifacts) fixtureReport

defineBenchmarkRules :: BakeoffConfig -> ResolvedTools -> BakeoffOutputs -> Rules ()
defineBenchmarkRules cfg tools outputs =
  case toolsHyperfinePath tools of
    Nothing -> pure ()
    Just hyperfinePath -> do
      toFilePath (boHyperfineAllJsonPath outputs) %> \_ -> do
        need [toFilePath (boBenchmarkPlanPath outputs)]
        liftIO $
          runHyperfineSuite
            hyperfinePath
            cfg
            outputs
            BenchmarkSuiteAll
            (boHyperfineAllJsonPath outputs)
            (boHyperfineAllMarkdownPath outputs)

      toFilePath (boHyperfineBenchmarkJsonPath outputs) %> \_ -> do
        need [toFilePath (boBenchmarkPlanPath outputs)]
        liftIO $
          runHyperfineSuite
            hyperfinePath
            cfg
            outputs
            BenchmarkSuiteBenchmark
            (boHyperfineBenchmarkJsonPath outputs)
            (boHyperfineBenchmarkMarkdownPath outputs)

buildMonkTranslationReport :: BakeoffConfig -> FixtureSpec -> FixtureArtifacts -> IO TranslationReport
buildMonkTranslationReport cfg fixture artifacts =
  case specSkipReason fixture of
    Just reason -> pure (skippedTranslationReport ToolMonk reason)
    Nothing -> do
      result <- timeoutIO (bakeoffTranslationTimeoutSeconds cfg) (translateFixtureWithMonk fixture)
      case result of
        Nothing -> do
          writeTextFile (faMonkTranslateStderr artifacts) "translation timed out\n"
          pure
            MkTranslationReport
              { translationTool = ToolMonk,
                translationStatus = CommandTimedOut,
                translationExitCode = Nothing,
                translationWarningCount = 0,
                translationNotesCount = 0,
                translationHighWarnings = 0,
                translationMediumWarnings = 0,
                translationLowWarnings = 0,
                translationConfidenceScore = Nothing,
                translationOutputPath = Nothing,
                translationStderrPath = Just (faMonkTranslateStderr artifacts),
                translationErrorMessage = Just "translation timed out"
              }
        Just (Left errText) -> do
          writeTextFile (faMonkTranslateStderr artifacts) (errText <> "\n")
          pure
            MkTranslationReport
              { translationTool = ToolMonk,
                translationStatus = CommandFailed,
                translationExitCode = Just 1,
                translationWarningCount = 0,
                translationNotesCount = 0,
                translationHighWarnings = 0,
                translationMediumWarnings = 0,
                translationLowWarnings = 0,
                translationConfidenceScore = Nothing,
                translationOutputPath = Nothing,
                translationStderrPath = Just (faMonkTranslateStderr artifacts),
                translationErrorMessage = Just errText
              }
        Just (Right artifact) -> do
          let MkWarningCounts {wcHigh, wcMedium, wcLow} = summarizeWarnings (mtaWarnings artifact)
              warnCount = length (mtaWarnings artifact)
              confidence = confidenceScore (mtaWarnings artifact)
          writeTextFile (faMonkFish artifacts) (mtaOutput artifact)
          writeTextFile (faMonkTranslateStderr artifacts) (T.unlines (mtaStderrLines artifact))
          pure
            MkTranslationReport
              { translationTool = ToolMonk,
                translationStatus = CommandSucceeded,
                translationExitCode = Just 0,
                translationWarningCount = warnCount,
                translationNotesCount = mtaNoteCount artifact,
                translationHighWarnings = wcHigh,
                translationMediumWarnings = wcMedium,
                translationLowWarnings = wcLow,
                translationConfidenceScore = Just confidence,
                translationOutputPath = Just (faMonkFish artifacts),
                translationStderrPath = Just (faMonkTranslateStderr artifacts),
                translationErrorMessage = Nothing
              }

buildBabelfishTranslationReport ::
  BakeoffConfig ->
  ResolvedTools ->
  FixtureSpec ->
  FixtureArtifacts ->
  [(String, String)] ->
  IO TranslationReport
buildBabelfishTranslationReport cfg tools fixture artifacts processEnv =
  case specSkipReason fixture of
    Just reason -> pure (skippedTranslationReport ToolBabelfish reason)
    Nothing -> do
      bashSource <- TIO.readFile (toFilePath (specPath fixture))
      let process = (proc (toFilePath (toolsBabelfishPath tools)) []) {env = Just processEnv}
      result <- runProcessText (Just (bakeoffTranslationTimeoutSeconds cfg)) process bashSource
      case result of
        Nothing -> do
          writeTextFile (faBabelfishTranslateStderr artifacts) "translation timed out\n"
          pure
            MkTranslationReport
              { translationTool = ToolBabelfish,
                translationStatus = CommandTimedOut,
                translationExitCode = Nothing,
                translationWarningCount = 0,
                translationNotesCount = 0,
                translationHighWarnings = 0,
                translationMediumWarnings = 0,
                translationLowWarnings = 0,
                translationConfidenceScore = Nothing,
                translationOutputPath = Nothing,
                translationStderrPath = Just (faBabelfishTranslateStderr artifacts),
                translationErrorMessage = Just "translation timed out"
              }
        Just MkProcessOutput {..} -> do
          writeTextFile (faBabelfishTranslateStderr artifacts) poStderr
          let exitCodeInt = exitCodeToInt poExitCode
          if poExitCode == ExitSuccess
            then do
              writeTextFile (faBabelfishFish artifacts) poStdout
              pure
                MkTranslationReport
                  { translationTool = ToolBabelfish,
                    translationStatus = CommandSucceeded,
                    translationExitCode = Just exitCodeInt,
                    translationWarningCount = 0,
                    translationNotesCount = 0,
                    translationHighWarnings = 0,
                    translationMediumWarnings = 0,
                    translationLowWarnings = 0,
                    translationConfidenceScore = Nothing,
                    translationOutputPath = Just (faBabelfishFish artifacts),
                    translationStderrPath = Just (faBabelfishTranslateStderr artifacts),
                    translationErrorMessage = Nothing
                  }
            else
              pure
                MkTranslationReport
                  { translationTool = ToolBabelfish,
                    translationStatus = CommandFailed,
                    translationExitCode = Just exitCodeInt,
                    translationWarningCount = 0,
                    translationNotesCount = 0,
                    translationHighWarnings = 0,
                    translationMediumWarnings = 0,
                    translationLowWarnings = 0,
                    translationConfidenceScore = Nothing,
                    translationOutputPath = Nothing,
                    translationStderrPath = Just (faBabelfishTranslateStderr artifacts),
                    translationErrorMessage = Just (translationFailureMessage poExitCode poStderr)
                  }

buildRuntimeReport ::
  BakeoffConfig ->
  ResolvedTools ->
  FixtureSpec ->
  FixtureArtifacts ->
  ToolName ->
  TranslationReport ->
  [(String, String)] ->
  IO RuntimeReport
buildRuntimeReport cfg tools fixture artifacts tool translationReport processEnv =
  case translationStatus translationReport of
    CommandSucceeded -> do
      let scriptPath =
            case tool of
              ToolMonk -> faMonkFish artifacts
              ToolBabelfish -> faBabelfishFish artifacts
          stdoutPath =
            case tool of
              ToolMonk -> faMonkStdout artifacts
              ToolBabelfish -> faBabelfishStdout artifacts
          stderrPath =
            case tool of
              ToolMonk -> faMonkRuntimeStderr artifacts
              ToolBabelfish -> faBabelfishRuntimeStderr artifacts
          exitCodePath =
            case tool of
              ToolMonk -> faMonkExitCode artifacts
              ToolBabelfish -> faBabelfishExitCode artifacts
          metadata = specMetadata fixture
      result <-
        runProcessText
          (Just (bakeoffRuntimeTimeoutSeconds cfg))
          (runtimeProcess tools processEnv (fmMode metadata) scriptPath (fmArgs metadata))
          (fmStdin metadata)
      case result of
        Nothing -> do
          writeTextFile stderrPath "runtime timed out\n"
          pure
            MkRuntimeReport
              { runtimeTool = tool,
                runtimeStatus = CommandTimedOut,
                runtimeExitCode = Nothing,
                runtimeStdoutPath = Nothing,
                runtimeStderrPath = Just stderrPath,
                runtimeErrorMessage = Just "runtime timed out"
              }
        Just MkProcessOutput {..} -> do
          let exitCodeInt = exitCodeToInt poExitCode
          writeTextFile stdoutPath poStdout
          writeTextFile stderrPath poStderr
          writeTextFile exitCodePath (show exitCodeInt <> "\n")
          pure
            MkRuntimeReport
              { runtimeTool = tool,
                runtimeStatus = CommandSucceeded,
                runtimeExitCode = Just exitCodeInt,
                runtimeStdoutPath = Just stdoutPath,
                runtimeStderrPath = Just stderrPath,
                runtimeErrorMessage = Nothing
              }
    _ ->
      pure
        MkRuntimeReport
          { runtimeTool = tool,
            runtimeStatus = CommandSkipped,
            runtimeExitCode = Nothing,
            runtimeStdoutPath = Nothing,
            runtimeStderrPath = Nothing,
            runtimeErrorMessage = Just "translation did not succeed"
          }

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

translateFixtureWithMonk :: FixtureSpec -> IO (Either Text MonkTranslationArtifact)
translateFixtureWithMonk fixture
  | fmRecursive (specMetadata fixture) = translateRecursiveFixture (specPath fixture)
  | otherwise = translateSingleFixture (specPath fixture)

translateSingleFixture :: Path Abs File -> IO (Either Text MonkTranslationArtifact)
translateSingleFixture = translateFixtureViaGraph False

translateRecursiveFixture :: Path Abs File -> IO (Either Text MonkTranslationArtifact)
translateRecursiveFixture = translateFixtureViaGraph True

translateFixtureViaGraph :: Bool -> Path Abs File -> IO (Either Text MonkTranslationArtifact)
translateFixtureViaGraph recursive path = do
  graphE <- translateSourceGraph defaultConfig recursive (toFilePath path)
  case graphE of
    Left err -> pure (Left (renderSourceGraphFailureText err))
    Right graph -> Right <$> buildMonkArtifactFromGraph recursive path graph

buildMonkArtifactFromGraph :: Bool -> Path Abs File -> SourceGraph -> IO MonkTranslationArtifact
buildMonkArtifactFromGraph recursive path graph = do
  let rootPath = toFilePath path
      orderedTranslations =
        mapMaybe (`M.lookup` sgTranslations graph) (sgOrder graph)
      translationWarnings = map (stateWarnings . trState) orderedTranslations
      allWarnings = concat translationWarnings
      totalNotes = sum (map translationNoteCount translationWarnings)
      stderrLines = concatMap (stderrLinesForPath graph) (sgOrder graph)
  output <-
    if recursive
      then do
        inlineWarnsRef <- newIORef []
        inlined <-
          inlineStatements
            (\msg -> modifyIORef' inlineWarnsRef (\msgs -> msgs <> [msg]))
            (sgTranslations graph)
            mempty
            rootPath
        inlineWarns <- readIORef inlineWarnsRef
        pure (renderFish inlined, inlineWarns)
      else
        case M.lookup rootPath (sgTranslations graph) of
          Just translation ->
            pure (renderTranslationSingle translation, [])
          Nothing ->
            pure ("", [])
  let (renderedOutput, inlineWarns) = output
  pure
    MkMonkTranslationArtifact
      { mtaOutput = renderedOutput,
        mtaWarnings = allWarnings,
        mtaNoteCount = totalNotes,
        mtaStderrLines = stderrLines <> inlineWarns
      }

stderrLinesForPath :: SourceGraph -> FilePath -> [Text]
stderrLinesForPath graph path =
  map renderParseComment (M.findWithDefault [] path (sgParseComments graph))
    <> case M.lookup path (sgTranslations graph) of
      Nothing -> []
      Just translation ->
        let warns = stateWarnings (trState translation)
         in map renderWarning warns <> renderTranslationNotes path warns

renderTranslationSingle :: Translation -> Text
renderTranslationSingle translation =
  renderFish (trStatements translation)

renderSourceGraphFailureText :: SourceGraphFailure -> Text
renderSourceGraphFailureText = \case
  SourceGraphParseErrors _ errs -> T.unlines (map renderParseComment errs)
  SourceGraphTranslateFailure _ err -> renderTranslateError err

runtimeProcess :: ResolvedTools -> [(String, String)] -> ShellRunMode -> Path Abs File -> [Text] -> CreateProcess
runtimeProcess tools processEnv runMode scriptPath args =
  case runMode of
    ShellRunExec ->
      (proc (toFilePath (toolsFishPath tools)) ("--no-config" : toFilePath scriptPath : map toString args)) {env = Just processEnv}
    ShellRunAuto ->
      runtimeProcess tools processEnv ShellRunSource scriptPath args
    _ ->
      (proc (toFilePath (toolsFishPath tools)) ["--no-config", "-c", T.unpack sourceCommand]) {env = Just processEnv}
  where
    quotedArgs = T.intercalate " " (map quoteArg args)
    sourceCommand =
      if null args
        then "source " <> quoteArg (toText (toFilePath scriptPath))
        else "source " <> quoteArg (toText (toFilePath scriptPath)) <> " " <> quotedArgs

quoteArg :: Text -> Text
quoteArg txt =
  "'" <> T.replace "'" "'\\''" txt <> "'"

translationFailureMessage :: ExitCode -> Text -> Text
translationFailureMessage exitCode stderrText
  | T.null (T.strip stderrText) = "translation failed with exit code " <> show (exitCodeToInt exitCode)
  | otherwise = T.strip stderrText

skippedTranslationReport :: ToolName -> SkipReason -> TranslationReport
skippedTranslationReport tool reason =
  MkTranslationReport
    { translationTool = tool,
      translationStatus = CommandSkipped,
      translationExitCode = Nothing,
      translationWarningCount = 0,
      translationNotesCount = 0,
      translationHighWarnings = 0,
      translationMediumWarnings = 0,
      translationLowWarnings = 0,
      translationConfidenceScore = Nothing,
      translationOutputPath = Nothing,
      translationStderrPath = Nothing,
      translationErrorMessage = Just (renderSkipReasonText reason)
    }

renderSkipReasonText :: SkipReason -> Text
renderSkipReasonText = \case
  SkipPlatformMismatch current allowed ->
    "skipped on " <> current <> " (allowed: " <> T.intercalate ", " allowed <> ")"
  SkipMissingPrereqs missing ->
    "missing prerequisites: " <> T.intercalate ", " missing

timeoutIO :: Int -> IO a -> IO (Maybe a)
timeoutIO seconds ioAction =
  timeout (seconds * 1_000_000) ioAction

runWorkerFixture :: ToolName -> Path Abs File -> Path Abs File -> IO (Maybe ToolName)
runWorkerFixture tool babelfishPath path =
  case tool of
    ToolMonk -> do
      recursive <- loadFixtureRecursive path
      translation <- if recursive then translateRecursiveFixture path else translateSingleFixture path
      case translation of
        Left _ -> pure (Just tool)
        Right artifact -> do
          _ <- evaluate (T.length (mtaOutput artifact))
          pure Nothing
    ToolBabelfish -> do
      source <- TIO.readFile (toFilePath path)
      result <- runProcessText Nothing (proc (toFilePath babelfishPath) []) source
      pure $
        case result of
          Just MkProcessOutput {poExitCode = ExitSuccess} -> Nothing
          _ -> Just tool

exitCodeToInt :: ExitCode -> Int
exitCodeToInt = \case
  ExitSuccess -> 0
  ExitFailure code -> code

{-# LANGUAGE LambdaCase #-}

module Bakeoff.Execution.Translation
  ( buildMonkTranslationReport,
    buildBabelfishTranslationReport,
    runWorkerFixture,
  )
where

import Bakeoff.Artifacts
  ( FixtureArtifacts (..),
    writeTextFile,
  )
import Bakeoff.Execution.Shared
  ( exitCodeToInt,
    skippedTranslationReport,
  )
import Bakeoff.Fixture (FixtureMetadata (..), loadFixtureRecursive)
import Bakeoff.Process
  ( ProcessOutput (..),
    runProcessText,
  )
import Bakeoff.Types
import Control.Exception (evaluate)
import Data.ByteString qualified as BS
import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Monk.Diagnostics
  ( DiagnosticCounts (..),
    renderDiagnostic,
    renderRuntimeRequirement,
    renderTranslationNotes,
    reviewRisk,
    summarizeDiagnostics,
    translationNoteCount,
  )
import Monk.Output (OutputTarget (OutputStdout), planCombinedOutputBundle, renderOutputBundle)
import Monk.Source
  ( SourceGraph,
    SourceGraphFailure (..),
    sourceGraphDiagnostics,
    sourceGraphRuntimeRequirements,
    sourceGraphStatistics,
    sourceRoot,
    translateSourceGraph,
  )
import Monk.Translation
  ( Diagnostic (..),
    ReviewRisk (..),
    RuntimeProgram (..),
    RuntimeRequirement (..),
    TranslationFailure (..),
    TranslationStatistics (..),
    fishFeatureName,
    nativeOperationName,
    platformCapabilityName,
  )
import Path
  ( Abs,
    File,
    Path,
    toFilePath,
  )
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (env), proc)
import System.Timeout (timeout)

data MonkTranslationArtifact = MkMonkTranslationArtifact
  { mtaOutput :: Text,
    mtaDiagnostics :: [Diagnostic],
    mtaRuntimeRequirements :: [RuntimeRequirement],
    mtaStatistics :: TranslationStatistics,
    mtaNoteCount :: Int,
    mtaStderrLines :: [Text]
  }

buildMonkTranslationReport :: BakeoffConfig -> FixtureSpec -> FixtureArtifacts -> IO TranslationReport
buildMonkTranslationReport cfg fixture artifacts =
  case specSkipReason fixture of
    Just reason -> pure (skippedTranslationReport ToolMonk reason)
    Nothing -> do
      result <- timeoutIO (bakeoffTranslationTimeoutSeconds cfg) (translateFixtureWithMonk (bakeoffTranslationSettings cfg) fixture)
      case result of
        Nothing -> do
          writeTextFile (faMonkTranslateStderr artifacts) "translation timed out\n"
          pure
            MkTranslationReport
              { translationTool = ToolMonk,
                translationStatus = CommandTimedOut,
                translationExitCode = Nothing,
                translationErrorCount = 0,
                translationWarningCount = 0,
                translationNotesCount = 0,
                translationReviewRisk = Nothing,
                translationInputBytes = Nothing,
                translationOutputBytes = Nothing,
                translationExpansionRatio = Nothing,
                translationStatistics = Nothing,
                translationHelperBytes = Nothing,
                translationHelperInvocations = 0,
                translationExternalRequirements = [],
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
                translationErrorCount = 1,
                translationWarningCount = 0,
                translationNotesCount = 0,
                translationReviewRisk = Just "unsafe",
                translationInputBytes = Nothing,
                translationOutputBytes = Nothing,
                translationExpansionRatio = Nothing,
                translationStatistics = Nothing,
                translationHelperBytes = Nothing,
                translationHelperInvocations = 0,
                translationExternalRequirements = [],
                translationOutputPath = Nothing,
                translationStderrPath = Just (faMonkTranslateStderr artifacts),
                translationErrorMessage = Just errText
              }
        Just (Right artifact) -> do
          inputBytes <- BS.length <$> readFileBS (toFilePath (specPath fixture))
          let MkDiagnosticCounts {dcErrors, dcWarnings} = summarizeDiagnostics (mtaDiagnostics artifact)
              outputBytes = textBytes (mtaOutput artifact)
          writeTextFile (faMonkFish artifacts) (mtaOutput artifact)
          writeTextFile (faMonkTranslateStderr artifacts) (T.unlines (mtaStderrLines artifact))
          pure
            MkTranslationReport
              { translationTool = ToolMonk,
                translationStatus = CommandSucceeded,
                translationExitCode = Just 0,
                translationErrorCount = dcErrors,
                translationWarningCount = dcWarnings,
                translationNotesCount = mtaNoteCount artifact,
                translationReviewRisk = Just (reviewRiskText (reviewRisk (mtaDiagnostics artifact))),
                translationInputBytes = Just inputBytes,
                translationOutputBytes = Just outputBytes,
                translationExpansionRatio = expansionRatio inputBytes outputBytes,
                translationStatistics = Just (mtaStatistics artifact),
                translationHelperBytes = Nothing,
                translationHelperInvocations = statisticsHelperCallSites (mtaStatistics artifact),
                translationExternalRequirements = map (runtimeProgramText . requirementProgram) (mtaRuntimeRequirements artifact),
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
                translationErrorCount = 0,
                translationWarningCount = 0,
                translationNotesCount = 0,
                translationReviewRisk = Nothing,
                translationInputBytes = Just (textBytes bashSource),
                translationOutputBytes = Nothing,
                translationExpansionRatio = Nothing,
                translationStatistics = Nothing,
                translationHelperBytes = Nothing,
                translationHelperInvocations = 0,
                translationExternalRequirements = [],
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
                    translationErrorCount = 0,
                    translationWarningCount = 0,
                    translationNotesCount = 0,
                    translationReviewRisk = Just "clean",
                    translationInputBytes = Just (textBytes bashSource),
                    translationOutputBytes = Just (textBytes poStdout),
                    translationExpansionRatio = expansionRatio (textBytes bashSource) (textBytes poStdout),
                    translationStatistics = Nothing,
                    translationHelperBytes = Nothing,
                    translationHelperInvocations = 0,
                    translationExternalRequirements = [],
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
                    translationErrorCount = 1,
                    translationWarningCount = 0,
                    translationNotesCount = 0,
                    translationReviewRisk = Just "unsafe",
                    translationInputBytes = Just (textBytes bashSource),
                    translationOutputBytes = Nothing,
                    translationExpansionRatio = Nothing,
                    translationStatistics = Nothing,
                    translationHelperBytes = Nothing,
                    translationHelperInvocations = 0,
                    translationExternalRequirements = [],
                    translationOutputPath = Nothing,
                    translationStderrPath = Just (faBabelfishTranslateStderr artifacts),
                    translationErrorMessage = Just (translationFailureMessage poExitCode poStderr)
                  }

translateFixtureWithMonk :: BakeoffTranslationSettings -> FixtureSpec -> IO (Either Text MonkTranslationArtifact)
translateFixtureWithMonk settings fixture
  | fmRecursive (specMetadata fixture) = translateRecursiveFixture settings (specPath fixture)
  | otherwise = translateSingleFixture settings (specPath fixture)

translateSingleFixture :: BakeoffTranslationSettings -> Path Abs File -> IO (Either Text MonkTranslationArtifact)
translateSingleFixture settings = translateFixtureViaGraph settings False

translateRecursiveFixture :: BakeoffTranslationSettings -> Path Abs File -> IO (Either Text MonkTranslationArtifact)
translateRecursiveFixture settings = translateFixtureViaGraph settings True

translateFixtureViaGraph :: BakeoffTranslationSettings -> Bool -> Path Abs File -> IO (Either Text MonkTranslationArtifact)
translateFixtureViaGraph settings recursive path = do
  graphE <- translateSourceGraph (bakeoffTranslateConfig settings) recursive (toFilePath path)
  case graphE of
    Left err -> pure (Left (renderSourceGraphFailureText err))
    Right graph -> buildMonkArtifactFromGraph graph

buildMonkArtifactFromGraph :: SourceGraph -> IO (Either Text MonkTranslationArtifact)
buildMonkArtifactFromGraph graph = do
  planned <- planCombinedOutputBundle OutputStdout graph
  pure $ case planned of
    Left diagnostic -> Left (renderDiagnostic diagnostic)
    Right bundle -> case L.lookup OutputStdout (renderOutputBundle bundle) of
      Nothing -> Left "combined planner did not produce its stdout entry"
      Just rendered ->
        Right
          MkMonkTranslationArtifact
            { mtaOutput = rendered,
              mtaDiagnostics = allDiagnostics,
              mtaRuntimeRequirements = allRequirements,
              mtaStatistics = sourceGraphStatistics graph,
              mtaNoteCount = translationNoteCount allDiagnostics,
              mtaStderrLines =
                map renderDiagnostic allDiagnostics
                  <> renderTranslationNotes (sourceRoot graph) allDiagnostics
                  <> map renderRuntimeRequirement allRequirements
            }
  where
    allDiagnostics = sourceGraphDiagnostics graph
    allRequirements = sourceGraphRuntimeRequirements graph

renderSourceGraphFailureText :: SourceGraphFailure -> Text
renderSourceGraphFailureText = \case
  MkSourceGraphFailure _ failure -> T.unlines (map renderDiagnostic (toList (failureDiagnostics failure)))

translationFailureMessage :: ExitCode -> Text -> Text
translationFailureMessage exitCode stderrText
  | T.null (T.strip stderrText) = "translation failed with exit code " <> show (exitCodeToInt exitCode)
  | otherwise = T.strip stderrText

timeoutIO :: Int -> IO a -> IO (Maybe a)
timeoutIO seconds =
  timeout (seconds * 1_000_000)

textBytes :: Text -> Int
textBytes = BS.length . encodeUtf8

expansionRatio :: Int -> Int -> Maybe Double
expansionRatio inputBytes outputBytes
  | inputBytes <= 0 = Nothing
  | otherwise = Just (fromIntegral outputBytes / fromIntegral inputBytes)

reviewRiskText :: ReviewRisk -> Text
reviewRiskText = \case
  Clean -> "clean"
  Review -> "review"
  Unsafe -> "unsafe"

runtimeProgramText :: RuntimeProgram -> Text
runtimeProgramText = \case
  RequiresCommand commandName -> commandName
  RequiresFishFeature featureName -> "fish:" <> fishFeatureName featureName
  RequiresPlatformCapability capability -> "platform:" <> platformCapabilityName capability
  RequiresNativeRuntime abi profile operations -> "monk-runtime:abi-" <> show abi <> ":" <> show profile <> ":" <> show (map nativeOperationName (toList operations))

runWorkerFixture :: BakeoffTranslationSettings -> ToolName -> Path Abs File -> Path Abs File -> IO (Maybe ToolName)
runWorkerFixture settings tool babelfishPath path =
  case tool of
    ToolMonk -> do
      recursive <- loadFixtureRecursive path
      translation <- if recursive then translateRecursiveFixture settings path else translateSingleFixture settings path
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

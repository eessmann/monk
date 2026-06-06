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
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
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
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (env), proc)
import System.Timeout (timeout)

data MonkTranslationArtifact = MkMonkTranslationArtifact
  { mtaOutput :: Text,
    mtaWarnings :: [Warning],
    mtaNoteCount :: Int,
    mtaStderrLines :: [Text]
  }

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
  (renderedOutput, inlineWarns) <-
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
      else case M.lookup rootPath (sgTranslations graph) of
        Just translation ->
          pure (renderTranslationSingle translation, [])
        Nothing ->
          pure ("", [])
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

translationFailureMessage :: ExitCode -> Text -> Text
translationFailureMessage exitCode stderrText
  | T.null (T.strip stderrText) = "translation failed with exit code " <> show (exitCodeToInt exitCode)
  | otherwise = T.strip stderrText

timeoutIO :: Int -> IO a -> IO (Maybe a)
timeoutIO seconds =
  timeout (seconds * 1_000_000)

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

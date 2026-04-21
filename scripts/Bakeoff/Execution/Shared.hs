{-# LANGUAGE LambdaCase #-}

module Bakeoff.Execution.Shared
  ( exitCodeToInt,
    skippedTranslationReport,
    renderSkipReasonText,
  )
where

import Data.Text qualified as T
import Bakeoff.Types
import System.Exit (ExitCode (..))

exitCodeToInt :: ExitCode -> Int
exitCodeToInt = \case
  ExitSuccess -> 0
  ExitFailure code -> code

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

{-# LANGUAGE LambdaCase #-}

module Bakeoff.Execution.Shared
  ( exitCodeToInt,
    skippedTranslationReport,
    renderSkipReasonText,
  )
where

import Bakeoff.Types
import Data.Text qualified as T
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
      translationStderrPath = Nothing,
      translationErrorMessage = Just (renderSkipReasonText reason)
    }

renderSkipReasonText :: SkipReason -> Text
renderSkipReasonText = \case
  SkipPlatformMismatch current allowed ->
    "skipped on " <> current <> " (allowed: " <> T.intercalate ", " allowed <> ")"
  SkipMissingPrereqs missing ->
    "missing prerequisites: " <> T.intercalate ", " missing

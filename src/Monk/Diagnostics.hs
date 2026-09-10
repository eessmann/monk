{-# LANGUAGE LambdaCase #-}

module Monk.Diagnostics
  ( DiagnosticCounts (..),
    renderDiagnostic,
    renderRuntimeRequirement,
    renderTranslationNotes,
    summarizeDiagnostics,
    reviewRisk,
    translationNoteCount,
  )
where

import Language.Fish.DSL (SourcePos (..), SourceRange (..))
import Monk.Translation.Types

data DiagnosticCounts = MkDiagnosticCounts
  { dcErrors :: Int,
    dcWarnings :: Int,
    dcNotes :: Int
  }
  deriving stock (Eq, Show)

renderDiagnostic :: Diagnostic -> Text
renderDiagnostic diagnostic =
  locationPrefix
    <> severityText (diagnosticSeverity diagnostic)
    <> "["
    <> diagnosticCodeText (diagnosticCode diagnostic)
    <> "]["
    <> riskText (diagnosticRisk diagnostic)
    <> "]: "
    <> diagnosticMessage diagnostic
  where
    locationPrefix = maybe "" ((<> ": ") . formatRange) (diagnosticRange diagnostic)

renderRuntimeRequirement :: RuntimeRequirement -> Text
renderRuntimeRequirement requirement =
  "note: runtime requirement: "
    <> runtimeProgramText (requirementProgram requirement)
    <> " ("
    <> show (length (requirementUses requirement))
    <> " uses)"

renderTranslationNotes :: FilePath -> [Diagnostic] -> [Text]
renderTranslationNotes path diagnostics =
  let MkDiagnosticCounts {dcErrors, dcWarnings, dcNotes} = summarizeDiagnostics diagnostics
      risk = reviewRisk diagnostics
      total = dcErrors + dcWarnings + dcNotes
      summary =
        "note: "
          <> toText path
          <> ": "
          <> show total
          <> " diagnostic(s) ("
          <> show dcErrors
          <> " error, "
          <> show dcWarnings
          <> " warning, "
          <> show dcNotes
          <> " note)"
      riskLine = "note: review risk " <> riskText risk
   in [summary, riskLine]

summarizeDiagnostics :: [Diagnostic] -> DiagnosticCounts
summarizeDiagnostics =
  foldl' tally (MkDiagnosticCounts 0 0 0)
  where
    tally counts diagnostic =
      case diagnosticSeverity diagnostic of
        DiagnosticError -> counts {dcErrors = dcErrors counts + 1}
        DiagnosticWarning -> counts {dcWarnings = dcWarnings counts + 1}
        DiagnosticNote -> counts {dcNotes = dcNotes counts + 1}

reviewRisk :: [Diagnostic] -> ReviewRisk
reviewRisk = foldl' max Clean . map diagnosticRisk

translationNoteCount :: [Diagnostic] -> Int
translationNoteCount = length . renderTranslationNotes ""

severityText :: DiagnosticSeverity -> Text
severityText = \case
  DiagnosticError -> "error"
  DiagnosticWarning -> "warning"
  DiagnosticNote -> "note"

riskText :: ReviewRisk -> Text
riskText = \case
  Clean -> "clean"
  Review -> "review"
  Unsafe -> "unsafe"

runtimeProgramText :: RuntimeProgram -> Text
runtimeProgramText = \case
  RequiresCommand commandName -> commandName
  RequiresFishFeature featureName -> "fish:" <> fishFeatureName featureName
  RequiresPlatformCapability capability -> "platform:" <> platformCapabilityName capability
  RequiresNativeRuntime abi profile operations -> "monk-runtime:abi-" <> show abi <> ":" <> show profile <> ":" <> show (map nativeOperationName (toList operations))

formatRange :: SourceRange -> Text
formatRange MkSourceRange {rangeStart = MkSourcePos {..}} =
  srcFile <> ":" <> show srcLine <> ":" <> show srcColumn

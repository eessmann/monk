{-# LANGUAGE LambdaCase #-}

module Monk.Diagnostics
  ( WarningSeverity (..),
    WarningCounts (..),
    renderParseComment,
    renderWarning,
    renderTranslateError,
    renderTranslationNotes,
    summarizeWarnings,
    confidenceScore,
    translationNoteCount,
    warningSeverity,
  )
where

import Data.Text qualified as T
import GHC.Show qualified as GHC
import Language.Fish.AST (SourcePos (..), SourceRange (..))
import Language.Fish.Translator.Monad (TranslateError (..), Warning (..))
import ShellCheck.Interface (Position (..), PositionedComment (..))

data WarningSeverity
  = WarnHigh
  | WarnMedium
  | WarnLow
  deriving stock (Eq, Show)

data WarningCounts = WarningCounts
  { wcHigh :: Int,
    wcMedium :: Int,
    wcLow :: Int
  }
  deriving stock (Eq, Show)

renderParseComment :: PositionedComment -> Text
renderParseComment pc =
  let pos = pcStartPos pc
      loc = formatPosition pos
   in loc <> ": " <> toText (GHC.show (pcComment pc))

renderWarning :: Warning -> Text
renderWarning Warning {warnMessage = msg, warnRange = mRange} =
  case mRange of
    Nothing -> "warning: " <> msg
    Just range -> formatRange range <> ": warning: " <> msg

renderTranslateError :: TranslateError -> Text
renderTranslateError = \case
  Unsupported msg mRange ->
    case mRange of
      Nothing -> "error: " <> msg
      Just range -> formatRange range <> ": error: " <> msg
  InternalError msg -> "error: " <> msg

renderTranslationNotes :: FilePath -> [Warning] -> [Text]
renderTranslationNotes path warns =
  let WarningCounts {wcHigh, wcMedium, wcLow} = summarizeWarnings warns
      total = wcHigh + wcMedium + wcLow
      score = confidenceScore warns
      header =
        "note: " <> toText path <> ": translation confidence " <> show score <> "/100"
      details =
        "note: "
          <> show total
          <> " warning(s) ("
          <> show wcHigh
          <> " high, "
          <> show wcMedium
          <> " medium, "
          <> show wcLow
          <> " low)"
      highRisk = "note: high-risk translations present; review recommended"
      detailLines = [details | total > 0] <> [highRisk | wcHigh > 0]
   in header : detailLines

summarizeWarnings :: [Warning] -> WarningCounts
summarizeWarnings warns =
  foldl' tally (WarningCounts 0 0 0) warns
  where
    tally counts warn =
      case warningSeverity warn of
        WarnHigh -> counts {wcHigh = wcHigh counts + 1}
        WarnMedium -> counts {wcMedium = wcMedium counts + 1}
        WarnLow -> counts {wcLow = wcLow counts + 1}

confidenceScore :: [Warning] -> Int
confidenceScore warns =
  let WarningCounts {wcHigh, wcMedium, wcLow} = summarizeWarnings warns
      raw = 100 - (wcHigh * 20) - (wcMedium * 10) - (wcLow * 4)
   in max 0 (min 100 raw)

translationNoteCount :: [Warning] -> Int
translationNoteCount = length . renderTranslationNotes ""

warningSeverity :: Warning -> WarningSeverity
warningSeverity Warning {warnMessage = msg} =
  let msgLower = T.toLower msg
   in if any (`T.isInfixOf` msgLower) highKeywords
        then WarnHigh
        else if any (`T.isInfixOf` msgLower) mediumKeywords
          then WarnMedium
          else WarnLow
  where
    highKeywords =
      [ "unsupported",
        "not supported",
        "no direct fish equivalent",
        "manual",
        "skipped"
      ]
    mediumKeywords =
      [ "emitting comment",
        "outside a function",
        "may require manual",
        "may need"
      ]

formatPosition :: Position -> Text
formatPosition pos =
  toText (posFile pos) <> ":" <> show (posLine pos) <> ":" <> show (posColumn pos)

formatRange :: SourceRange -> Text
formatRange SourceRange {rangeStart = SourcePos {..}} =
  srcFile <> ":" <> show srcLine <> ":" <> show srcColumn

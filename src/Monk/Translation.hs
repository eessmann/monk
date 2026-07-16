{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright: (c) 2025 Erich Essmann
-- SPDX-License-Identifier: MIT
-- Maintainer: Erich Essmann <essmanne@gmail.com>
--
-- Parse, translate, and render entry points for Monk.
module Monk.Translation
  ( TranslationResult (..),
    TranslationFailure (..),
    translateParseResult,
    translateBashFile,
    translateBashScript,
    renderTranslation,
    parseBashFile,
    parseBashScript,
    projectName,
    module Monk.Translation.Types,
  )
where

import Data.Char (isDigit)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Language.Bash.Parser (parseBashFile, parseBashScript)
import Language.Fish.DSL
  ( Script,
    SourcePos (..),
    SourceRange (..),
    renderScript,
  )
import Language.Fish.Translator qualified as Translator
import Language.Fish.Translator.Monad
  ( stateRuntimeRequirements,
    stateWarnings,
  )
import Language.Fish.Translator.Warning
  ( TranslateError (..),
    Warning (..),
    WarningSeverity (..),
    warnMessage,
    warningCodeText,
  )
import Monk.Translation.Types
import ShellCheck.Interface
  ( Comment (..),
    ParseResult,
    Position (..),
    PositionedComment (..),
    prComments,
    prRoot,
  )

data TranslationResult = MkTranslationResult
  { translationScript :: Script,
    translationDiagnostics :: [Diagnostic],
    translationRuntimeRequirements :: [RuntimeRequirement]
  }
  deriving stock (Show, Eq)

newtype TranslationFailure = MkTranslationFailure
  { failureDiagnostics :: NonEmpty Diagnostic
  }
  deriving stock (Show, Eq)

translateParseResult ::
  TranslateConfig ->
  ParseResult ->
  Either TranslationFailure TranslationResult
translateParseResult cfg parseResult =
  case prRoot parseResult of
    Nothing -> Left (parseFailure (prComments parseResult))
    Just _ ->
      case Translator.translateParseResult cfg parseResult of
        Left err -> Left (MkTranslationFailure (translateErrorDiagnostic err :| []))
        Right (script, translatorState) ->
          Right
            MkTranslationResult
              { translationScript = script,
                translationDiagnostics =
                  map positionedCommentDiagnostic (prComments parseResult)
                    <> map warningDiagnostic (stateWarnings translatorState),
                translationRuntimeRequirements = stateRuntimeRequirements translatorState
              }

translateBashFile ::
  TranslateConfig ->
  FilePath ->
  IO (Either TranslationFailure TranslationResult)
translateBashFile cfg path = do
  parseResult <- parseBashFile path
  pure $
    case parseResult of
      Left comments -> Left (parseFailure comments)
      Right parsed -> translateParseResult cfg parsed

translateBashScript ::
  TranslateConfig ->
  FilePath ->
  Text ->
  IO (Either TranslationFailure TranslationResult)
translateBashScript cfg fileName scriptText = do
  parsed <- parseBashScript fileName scriptText
  pure (translateParseResult cfg parsed)

renderTranslation :: TranslationResult -> Text
renderTranslation = renderScript . translationScript

parseFailure :: [PositionedComment] -> TranslationFailure
parseFailure comments =
  MkTranslationFailure
    ( fromMaybe
        (genericParseDiagnostic :| [])
        (NE.nonEmpty (map positionedCommentDiagnostic comments))
    )

positionedCommentDiagnostic :: PositionedComment -> Diagnostic
positionedCommentDiagnostic comment =
  MkDiagnostic
    { diagnosticCode = MkDiagnosticCode ("shellcheck." <> shellCheckCodeText (cCode payload)),
      diagnosticPhase = PhaseParse,
      diagnosticSeverity = shellCheckSeverity (show (cSeverity payload)),
      diagnosticRisk = shellCheckRisk (show (cSeverity payload)),
      diagnosticMessage = toText (cMessage payload),
      diagnosticRange = Just (positionRange (pcStartPos comment) (pcEndPos comment))
    }
  where
    payload = pcComment comment

warningDiagnostic :: Warning -> Diagnostic
warningDiagnostic warning =
  MkDiagnostic
    { diagnosticCode = MkDiagnosticCode (warningCodeText (warnCode warning)),
      diagnosticPhase = PhaseTranslate,
      diagnosticSeverity = DiagnosticWarning,
      diagnosticRisk = warningRisk (warnSeverity warning),
      diagnosticMessage = warnMessage warning,
      diagnosticRange = warnRange warning
    }

translateErrorDiagnostic :: TranslateError -> Diagnostic
translateErrorDiagnostic = \case
  Unsupported warning ->
    (warningDiagnostic warning)
      { diagnosticSeverity = DiagnosticError,
        diagnosticRisk = Unsafe
      }
  InternalError message ->
    MkDiagnostic
      { diagnosticCode = MkDiagnosticCode "monk.internal",
        diagnosticPhase = PhaseTranslate,
        diagnosticSeverity = DiagnosticError,
        diagnosticRisk = Unsafe,
        diagnosticMessage = message,
        diagnosticRange = Nothing
      }

genericParseDiagnostic :: Diagnostic
genericParseDiagnostic =
  MkDiagnostic
    { diagnosticCode = MkDiagnosticCode "shellcheck.parse",
      diagnosticPhase = PhaseParse,
      diagnosticSeverity = DiagnosticError,
      diagnosticRisk = Unsafe,
      diagnosticMessage = "Unable to parse Bash input",
      diagnosticRange = Nothing
    }

warningRisk :: WarningSeverity -> ReviewRisk
warningRisk = \case
  WarnHigh -> Unsafe
  WarnMedium -> Review
  WarnLow -> Review

shellCheckSeverity :: Text -> DiagnosticSeverity
shellCheckSeverity rendered
  | "Error" `T.isInfixOf` rendered = DiagnosticError
  | "Info" `T.isInfixOf` rendered || "Style" `T.isInfixOf` rendered = DiagnosticNote
  | otherwise = DiagnosticWarning

shellCheckRisk :: Text -> ReviewRisk
shellCheckRisk rendered
  | "Error" `T.isInfixOf` rendered = Unsafe
  | otherwise = Review

shellCheckCodeText :: (Show code) => code -> Text
shellCheckCodeText = toText . filter isDigit . show

positionRange :: Position -> Position -> SourceRange
positionRange start end = MkSourceRange (position start) (position end)
  where
    position source =
      MkSourcePos
        { srcFile = toText (posFile source),
          srcLine = fromInteger (posLine source),
          srcColumn = fromInteger (posColumn source)
        }

projectName :: Text
projectName = "monk"

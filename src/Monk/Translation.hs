{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright: (c) 2025 Erich Essmann
-- SPDX-License-Identifier: MIT
-- Maintainer: Erich Essmann <essmanne@gmail.com>
--
-- Parse, translate, and render entry points for Monk.
module Monk.Translation
  ( TranslationResult,
    translationScript,
    translationDiagnostics,
    translationRuntimeRequirements,
    translationStatistics,
    translationExecutionStrategy,
    TranslationFailure (..),
    translateParseResult,
    translateBashFile,
    translateBashScript,
    renderTranslation,
    parseBashFile,
    parseBashScript,
    parseCallerContract,
    projectName,
    module Monk.Translation.Types,
  )
where

import Data.List.NonEmpty qualified as NE
import Language.Fish.DSL
  ( Script,
    renderScript,
  )
import Language.Fish.Translator.Plan
  ( PlannedTranslation,
    compilePlannedDocument,
    compilePlannedTranslation,
    plannedDiagnostics,
    plannedRequirements,
    plannedScript,
    plannedStatistics,
  )
import Monk.Source.Environment (readSourceSnapshot, snapshotPath, snapshotText)
import Monk.Translation.Contract (parseCallerContract)
import Monk.Translation.ParseDiagnostics (genericParseDiagnostic, positionedCommentDiagnostic)
import Monk.Translation.Parser (parseBashFile, parseBashScript)
import Monk.Translation.Types
import ShellCheck.Interface (ParseResult, PositionedComment, prComments, prRoot)

-- The admitted artifact stays owned by its result. Public projections cannot
-- replace its script, provider binding, requirements or admission state.
data TranslationResult = MkTranslationResult
  { resultTranslation :: PlannedTranslation,
    resultParseDiagnostics :: [Diagnostic]
  }
  deriving stock (Show, Eq)

translationScript :: TranslationResult -> Script
translationScript = plannedScript . resultTranslation

translationDiagnostics :: TranslationResult -> [Diagnostic]
translationDiagnostics result = resultParseDiagnostics result <> plannedDiagnostics (resultTranslation result)

translationRuntimeRequirements :: TranslationResult -> [RuntimeRequirement]
translationRuntimeRequirements = plannedRequirements . resultTranslation

translationStatistics :: TranslationResult -> TranslationStatistics
translationStatistics = plannedStatistics . resultTranslation

translationExecutionStrategy :: TranslationResult -> ExecutionStrategy
translationExecutionStrategy = executionStrategyFor . translationRuntimeRequirements

newtype TranslationFailure = MkTranslationFailure
  { failureDiagnostics :: NonEmpty Diagnostic
  }
  deriving stock (Show, Eq)

-- | Advanced syntax-only entry. ShellCheck's ParseResult does not own original
-- source bytes, so forms needing exact source spelling (notably arithmetic
-- error diagnostics) reject here. Prefer 'translateBashScript' or
-- 'translateBashFile'; literal dependencies require 'Monk.Source' graph input.
translateParseResult ::
  TranslateConfig ->
  ParseResult ->
  Either TranslationFailure TranslationResult
translateParseResult cfg parseResult =
  case prRoot parseResult of
    Nothing -> Left (parseFailure (prComments parseResult))
    Just _ ->
      case compilePlannedTranslation cfg parseResult of
        Left diagnostics -> Left (MkTranslationFailure diagnostics)
        Right planned ->
          Right
            MkTranslationResult
              { resultTranslation = planned,
                resultParseDiagnostics = map positionedCommentDiagnostic (prComments parseResult)
              }

translateBashFile ::
  TranslateConfig ->
  FilePath ->
  IO (Either TranslationFailure TranslationResult)
translateBashFile cfg path = do
  snapshot <- readSourceSnapshot path
  case snapshot of
    Left diagnostic -> pure (Left (MkTranslationFailure (diagnostic :| [])))
    Right input -> translateBashScript cfg (snapshotPath input) (snapshotText input)

translateBashScript ::
  TranslateConfig ->
  FilePath ->
  Text ->
  IO (Either TranslationFailure TranslationResult)
translateBashScript cfg fileName scriptText = do
  parsed <- parseBashScript fileName scriptText
  pure $ case prRoot parsed of
    Nothing -> Left (parseFailure (prComments parsed))
    Just _ -> case compilePlannedDocument cfg scriptText parsed of
      Left diagnostics -> Left (MkTranslationFailure diagnostics)
      Right planned ->
        Right
          ( MkTranslationResult
              planned
              (map positionedCommentDiagnostic (prComments parsed))
          )

renderTranslation :: TranslationResult -> Text
renderTranslation = renderScript . translationScript

parseFailure :: [PositionedComment] -> TranslationFailure
parseFailure comments =
  MkTranslationFailure
    ( fromMaybe
        (genericParseDiagnostic :| [])
        (NE.nonEmpty (map positionedCommentDiagnostic comments))
    )

projectName :: Text
projectName = "monk"

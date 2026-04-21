-- |
-- Copyright: (c) 2025 Erich Essmann
-- SPDX-License-Identifier: MIT
-- Maintainer: Erich Essmann <essmanne@gmail.com>
--
-- Parse, translate, and render entry points for Monk.
{-# LANGUAGE OverloadedStrings #-}

module Monk.Translation
  ( TranslationResult (..),
    TranslationFailure (..),
    translateParseResult,
    translateBashFile,
    translateBashScript,
    translationWarnings,
    translationStatements,
    renderTranslation,
    flattenStatements,
    TranslateState,
    stateWarnings,
    stateErrexitEnabled,
    statePipefailEnabled,
    Translation (..),
    WarnFn,
    inlineStatements,
    renderFish,
    parseBashFile,
    parseBashScript,
    projectName,
    module Monk.Translation.Types,
  )
where

import Language.Bash.Parser (parseBashFile, parseBashScript)
import Language.Fish.AST (FishStatement (..))
import Language.Fish.Inline (Translation (..), WarnFn, inlineStatements)
import Language.Fish.Pretty (renderFish)
import Language.Fish.Translator qualified as Translator
import Language.Fish.Translator.Monad
  ( TranslateState,
    stateErrexitEnabled,
    statePipefailEnabled,
    stateWarnings,
  )
import Monk.Translation.Types
import ShellCheck.Interface (ParseResult, PositionedComment, prComments, prRoot)

-- | Result of a successful translation.
data TranslationResult = MkTranslationResult
  { -- | Root fish statement produced by the translator.
    translationStatement :: FishStatement,
    -- | Final translation state containing warnings and source mapping.
    translationState :: TranslateState
  }
  deriving stock (Show, Eq)

-- | Failure modes for parse and translation entry points.
data TranslationFailure
  -- | ShellCheck parse errors.
  = ParseErrors [PositionedComment]
  -- | Translation failed with a semantic error.
  | TranslateFailure TranslateError
  deriving stock (Show, Eq)

-- | Translate a parsed shell script into fish AST plus translation state.
translateParseResult ::
  TranslateConfig ->
  ParseResult ->
  Either TranslateError TranslationResult
translateParseResult cfg parseResult = do
  (stmt, st) <- Translator.translateParseResult cfg parseResult
  pure (MkTranslationResult stmt st)

-- | Parse and translate a Bash file on disk.
translateBashFile ::
  TranslateConfig ->
  FilePath ->
  IO (Either TranslationFailure TranslationResult)
translateBashFile cfg path = do
  parseResE <- parseBashFile path
  pure $
    case parseResE of
      Left errs -> Left (ParseErrors errs)
      Right parseRes ->
        case translateParseResult cfg parseRes of
          Left err -> Left (TranslateFailure err)
          Right res -> Right res

-- | Parse and translate Bash script text with an explicit source filename.
translateBashScript ::
  TranslateConfig ->
  FilePath ->
  Text ->
  IO (Either TranslationFailure TranslationResult)
translateBashScript cfg fileName scriptText = do
  parseRes <- parseBashScript fileName scriptText
  pure $
    case prRoot parseRes of
      Nothing -> Left (ParseErrors (prComments parseRes))
      Just _ ->
        case translateParseResult cfg parseRes of
          Left err -> Left (TranslateFailure err)
          Right res -> Right res

-- | Flatten the translated root statement into top-level statements.
translationWarnings :: TranslationResult -> [Warning]
translationWarnings = stateWarnings . translationState

-- | Flatten the translated root statement into top-level statements.
translationStatements :: TranslationResult -> [FishStatement]
translationStatements = flattenStatements . translationStatement

-- | Render a translation result as fish source text.
renderTranslation :: TranslationResult -> Text
renderTranslation = renderFish . translationStatements

-- | Convert a root statement into a top-level statement list.
flattenStatements :: FishStatement -> [FishStatement]
flattenStatements stmt =
  case stmt of
    StmtList xs -> xs
    other -> [other]

-- | Project name used in CLI and benchmark labels.
projectName :: Text
projectName = "monk"

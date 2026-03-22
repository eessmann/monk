-- |
-- Copyright: (c) 2025 Erich Essmann
-- SPDX-License-Identifier: MIT
-- Maintainer: Erich Essmann <essmanne@gmail.com>
--
-- See README for more info
{-# LANGUAGE OverloadedStrings #-}

module Monk
  ( TranslationResult (..),
    TranslationFailure (..),
    translateParseResult,
    translateBashFile,
    translateBashScript,
    translationStatements,
    renderTranslation,
    flattenStatements,
    defaultConfig,
    strictConfig,
    TranslateConfig (..),
    TranslateError (..),
    TranslateState (..),
    TranslationContext (..),
    Warning (..),
    Translation (..),
    WarnFn,
    inlineStatements,
    renderFish,
    parseBashFile,
    parseBashScript,
    projectName,
    module AST,
  )
where

import Language.Bash.Parser (parseBashFile, parseBashScript)
import Language.Fish.AST
import Language.Fish.AST qualified as AST
import Language.Fish.Inline (Translation (..), WarnFn, inlineStatements)
import Language.Fish.Pretty (renderFish)
import Language.Fish.Translator qualified as Translator
import Language.Fish.Translator.Monad
  ( TranslateConfig (..),
    TranslationContext (..),
    TranslateError (..),
    TranslateState (..),
    Warning (..),
    defaultConfig,
  )
import ShellCheck.Interface (ParseResult, PositionedComment, prComments, prRoot)

-- | Result of a successful translation.
data TranslationResult = TranslationResult
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
  pure (TranslationResult stmt st)

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

-- | Strict translation settings that fail on unsupported constructs.
strictConfig :: TranslateConfig
strictConfig = defaultConfig {strictMode = True}

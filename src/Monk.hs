-- |
-- Copyright: (c) 2025 Erich Essmann
-- SPDX-License-Identifier: MIT
-- Maintainer: Erich Essmann <essmanne@gmail.com>
--
-- See README for more info
{-# LANGUAGE OverloadedStrings #-}

module Monk
  ( TranslationResult (..), -- ^ Translation output (AST + state).
    TranslationFailure (..), -- ^ Parse or translation failures.
    translateParseResult, -- ^ Translate a ShellCheck ParseResult with source locations.
    translateBashFile, -- ^ Parse and translate a Bash file on disk.
    translateBashScript, -- ^ Parse and translate Bash script text.
    translationStatements, -- ^ Flatten top-level statements from a translation.
    renderTranslation, -- ^ Render a translation to Fish source.
    flattenStatements, -- ^ Flatten a FishStatement into a top-level list.
    defaultConfig, -- ^ Default translation configuration.
    strictConfig, -- ^ Strict translation configuration (unsupported constructs fail).
    TranslateConfig (..), -- ^ Translation configuration.
    TranslateError (..), -- ^ Translation errors.
    TranslateState (..), -- ^ Translation state (warnings, source ranges).
    Warning (..), -- ^ Non-fatal translation warnings.
    Translation (..), -- ^ Translation bundle for inlining.
    inlineStatements, -- ^ Inline translated sources (used by CLI/tests).
    renderFish, -- ^ Render a Fish AST (list of statements) to Text.
    parseBashFile, -- ^ Parse a Bash file from disk.
    parseBashScript, -- ^ Parse a Bash script from (filename, text) into a ShellCheck 'ParseResult'.
    projectName, -- ^ The name of the project.
    module AST,
  )
where

-- \^ The Fish AST.

import Language.Bash.Parser (parseBashFile, parseBashScript)
import Language.Fish.AST
import Language.Fish.AST qualified as AST
import Language.Fish.Inline (Translation (..), inlineStatements)
import Language.Fish.Pretty (renderFish)
import Language.Fish.Translator qualified as Translator
import Language.Fish.Translator.Monad
  ( TranslateConfig (..),
    TranslateError (..),
    TranslateState (..),
    Warning (..),
    defaultConfig,
  )
import ShellCheck.Interface (ParseResult, PositionedComment, prComments, prRoot)

data TranslationResult = TranslationResult
  { translationStatement :: FishStatement,
    translationState :: TranslateState
  }
  deriving stock (Show, Eq)

data TranslationFailure
  = ParseErrors [PositionedComment]
  | TranslateFailure TranslateError
  deriving stock (Show, Eq)

translateParseResult ::
  TranslateConfig ->
  ParseResult ->
  Either TranslateError TranslationResult
translateParseResult cfg parseResult = do
  (stmt, st) <- Translator.translateParseResult cfg parseResult
  pure (TranslationResult stmt st)

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

translationStatements :: TranslationResult -> [FishStatement]
translationStatements = flattenStatements . translationStatement

renderTranslation :: TranslationResult -> Text
renderTranslation = renderFish . translationStatements

flattenStatements :: FishStatement -> [FishStatement]
flattenStatements stmt =
  case stmt of
    StmtList xs -> xs
    other -> [other]

projectName :: Text
projectName = "monk"

strictConfig :: TranslateConfig
strictConfig = defaultConfig {strictMode = True}

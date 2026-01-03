-- |
-- Copyright: (c) 2025 Erich Essmann
-- SPDX-License-Identifier: MIT
-- Maintainer: Erich Essmann <essmanne@gmail.com>
--
-- See README for more info
{-# LANGUAGE OverloadedStrings #-}

module Monk
  ( translateRoot, -- ^ Convert a ShellCheck AST (Root) into a Fish AST.
    translateToken, -- ^ Convert a ShellCheck AST (Token) into a Fish AST.
    translateParseResult, -- ^ Translate a ShellCheck ParseResult with source locations.
    defaultConfig, -- ^ Default translation configuration.
    strictConfig, -- ^ Strict translation configuration (unsupported constructs fail).
    TranslateConfig (..), -- ^ Translation configuration.
    TranslateError (..), -- ^ Translation errors.
    TranslateState (..), -- ^ Translation state (warnings, source ranges).
    Warning (..), -- ^ Non-fatal translation warnings.
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
import Language.Fish.Pretty (renderFish)
import Language.Fish.Translator (translateParseResult, translateRoot, translateToken)
import Language.Fish.Translator.Monad
  ( TranslateConfig (..),
    TranslateError (..),
    TranslateState (..),
    Warning (..),
    defaultConfig,
  )

projectName :: Text
projectName = "monk"

strictConfig :: TranslateConfig
strictConfig = defaultConfig {strictMode = True}

-- |
-- Copyright: (c) 2025 Erich Essmann
-- SPDX-License-Identifier: MIT
-- Maintainer: Erich Essmann <essmanne@gmail.com>
--
-- See README for more info
module Monk
  ( translateRoot  -- ^ Convert a ShellCheck AST (Root) into a Fish AST.
  , translateToken -- ^ Convert a ShellCheck AST (Token) into a Fish AST.
  , translateParseResult -- ^ Translate a ShellCheck ParseResult with source locations.
  , defaultConfig -- ^ Default translation configuration.
  , renderFish     -- ^ Render a Fish AST (list of statements) to Text.
  , parseBashFile  -- ^ Parse a Bash file from disk.
  , parseBashScript -- ^ Parse a Bash script from (filename, text) into a ShellCheck 'ParseResult'.
  , projectName    -- ^ The name of the project.
  , module AST     -- ^ The Fish AST.
  ) where

import Language.Bash.Parser     (parseBashScript, parseBashFile)
import Language.Fish.Translator (translateRoot, translateToken, translateParseResult)
import Language.Fish.Pretty     (renderFish)
import qualified Language.Fish.AST as AST
import Language.Fish.AST
import Language.Fish.Translator.Monad (defaultConfig)


projectName :: Text
projectName = "monk"

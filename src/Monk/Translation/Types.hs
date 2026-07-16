-- |
-- Copyright: (c) 2025 Erich Essmann
-- SPDX-License-Identifier: MIT
-- Maintainer: Erich Essmann <essmanne@gmail.com>
--
-- Stable public translation, diagnostics, and runtime-requirement types.
module Monk.Translation.Types
  ( TranslateConfig (..),
    defaultConfig,
    strictConfig,
    DiagnosticCode (..),
    DiagnosticPhase (..),
    DiagnosticSeverity (..),
    ReviewRisk (..),
    Diagnostic (..),
    RuntimeProgram (..),
    RequirementUse (..),
    RuntimeRequirement (..),
  )
where

import Language.Fish.DSL (SourceRange)

newtype TranslateConfig = MkTranslateConfig
  { strictMode :: Bool
  }
  deriving stock (Show, Eq)

defaultConfig :: TranslateConfig
defaultConfig = MkTranslateConfig {strictMode = False}

strictConfig :: TranslateConfig
strictConfig = defaultConfig {strictMode = True}

newtype DiagnosticCode = MkDiagnosticCode
  { diagnosticCodeText :: Text
  }
  deriving stock (Show, Eq, Ord)

data DiagnosticPhase
  = PhaseParse
  | PhaseTranslate
  | PhaseSource
  | PhaseRuntime
  deriving stock (Show, Eq, Ord)

data DiagnosticSeverity
  = DiagnosticError
  | DiagnosticWarning
  | DiagnosticNote
  deriving stock (Show, Eq, Ord)

data ReviewRisk
  = Clean
  | Review
  | Unsafe
  deriving stock (Show, Eq, Ord)

data Diagnostic = MkDiagnostic
  { diagnosticCode :: DiagnosticCode,
    diagnosticPhase :: DiagnosticPhase,
    diagnosticSeverity :: DiagnosticSeverity,
    diagnosticRisk :: ReviewRisk,
    diagnosticMessage :: Text,
    diagnosticRange :: Maybe SourceRange
  }
  deriving stock (Show, Eq)

data RuntimeProgram
  = RequiresCommand Text
  | RequiresFishFeature Text
  deriving stock (Show, Eq, Ord)

data RequirementUse = MkRequirementUse
  { requirementReason :: Text,
    requirementRange :: Maybe SourceRange
  }
  deriving stock (Show, Eq, Ord)

data RuntimeRequirement = MkRuntimeRequirement
  { requirementProgram :: RuntimeProgram,
    requirementUses :: NonEmpty RequirementUse
  }
  deriving stock (Show, Eq)

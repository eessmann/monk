-- |
-- Copyright: (c) 2025 Erich Essmann
-- SPDX-License-Identifier: MIT
-- Maintainer: Erich Essmann <essmanne@gmail.com>
--
-- Public translation and diagnostics contract types.
module Monk.Translation.Types
  ( TranslateConfig (..),
    defaultConfig,
    strictConfig,
    WarningSeverity (..),
    WarningCode (..),
    Warning (..),
    TranslateError (..),
    warningCodeSeverity,
    warnMessage,
  )
where

import Language.Fish.AST (SourceRange)

-- | Configuration flags controlling translation behavior.
newtype TranslateConfig = MkTranslateConfig
  { -- | Fail on unsupported constructs.
    strictMode :: Bool
  }
  deriving stock (Show, Eq)

-- | Default translation settings used by the public API.
defaultConfig :: TranslateConfig
defaultConfig = MkTranslateConfig {strictMode = False}

-- | Strict translation settings that fail on unsupported constructs.
strictConfig :: TranslateConfig
strictConfig = defaultConfig {strictMode = True}

data WarningSeverity
  = WarnHigh
  | WarnMedium
  | WarnLow
  deriving stock (Eq, Ord, Show)

data WarningCode
  = UnsupportedConstruct
  | BestEffortSubshell
  | ExecFdRedirect
  | BackgroundTracking
  | SetOptionIssue
  | ReadIssue
  | ShoptIgnored
  | TrapIssue
  | ShiftIssue
  | ReadonlyNotEnforced
  | DeclareIssue
  | ScopeIssue
  | UnsetIssue
  | ForArithmeticIssue
  | ArithmeticIssue
  deriving stock (Eq, Ord, Show)

-- | Structured warning payload used for surfaced diagnostics and strict errors.
data Warning = MkWarning
  { warnCode :: WarningCode,
    warnSeverity :: WarningSeverity,
    warnDetail :: Maybe Text,
    warnRange :: Maybe SourceRange
  }
  deriving stock (Show, Eq)

-- | Translation errors for unsupported or invalid constructs.
data TranslateError
  = Unsupported Warning
  | InternalError Text
  deriving stock (Show, Eq)

warningCodeSeverity :: WarningCode -> WarningSeverity
warningCodeSeverity = \case
  UnsupportedConstruct -> WarnHigh
  BestEffortSubshell -> WarnHigh
  ExecFdRedirect -> WarnMedium
  BackgroundTracking -> WarnHigh
  SetOptionIssue -> WarnHigh
  ReadIssue -> WarnMedium
  ShoptIgnored -> WarnHigh
  TrapIssue -> WarnMedium
  ShiftIssue -> WarnMedium
  ReadonlyNotEnforced -> WarnHigh
  DeclareIssue -> WarnMedium
  ScopeIssue -> WarnMedium
  UnsetIssue -> WarnMedium
  ForArithmeticIssue -> WarnMedium
  ArithmeticIssue -> WarnHigh

warnMessage :: Warning -> Text
warnMessage MkWarning {warnCode, warnDetail} =
  case (warnCode, warnDetail) of
    (UnsupportedConstruct, Just detail) -> detail
    (UnsupportedConstruct, Nothing) -> "Unsupported construct"
    (BestEffortSubshell, _) -> "Subshell does not isolate environment in fish; best-effort translation emitted"
    (ExecFdRedirect, _) -> "exec with file descriptor redirection may require manual adjustment in fish"
    (BackgroundTracking, _) -> "Monk-managed background job IDs are only guaranteed for translated wait; PID-specific uses such as kill $! require manual review"
    (SetOptionIssue, Just detail) -> detail
    (SetOptionIssue, Nothing) -> "Bash set options require manual review"
    (ReadIssue, Just detail) -> detail
    (ReadIssue, Nothing) -> "read semantics may differ between bash and fish"
    (ShoptIgnored, _) -> "shopt has no fish equivalent; ignored"
    (TrapIssue, Just detail) -> detail
    (TrapIssue, Nothing) -> "trap handling requires manual review"
    (ShiftIssue, Just detail) -> detail
    (ShiftIssue, Nothing) -> "shift translation requires manual review"
    (ReadonlyNotEnforced, _) -> "readonly/declare -r has no direct fish equivalent; emitted set without enforcing readonly"
    (DeclareIssue, Just detail) -> detail
    (DeclareIssue, Nothing) -> "declare translation requires manual review"
    (ScopeIssue, Just detail) -> detail
    (ScopeIssue, Nothing) -> "scope translation requires manual review"
    (UnsetIssue, Just detail) -> detail
    (UnsetIssue, Nothing) -> "unset translation requires manual review"
    (ForArithmeticIssue, Just detail) -> detail
    (ForArithmeticIssue, Nothing) -> "arithmetic for-loop translation requires manual review"
    (ArithmeticIssue, Just detail) -> detail
    (ArithmeticIssue, Nothing) -> "arithmetic translation may lose side effects"

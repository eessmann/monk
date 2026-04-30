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
    allWarningCodes,
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

-- | All public warning codes, in constructor order.
allWarningCodes :: [WarningCode]
allWarningCodes =
  [ UnsupportedConstruct,
    BestEffortSubshell,
    ExecFdRedirect,
    BackgroundTracking,
    SetOptionIssue,
    ReadIssue,
    ShoptIgnored,
    TrapIssue,
    ShiftIssue,
    ReadonlyNotEnforced,
    DeclareIssue,
    ScopeIssue,
    UnsetIssue,
    ForArithmeticIssue,
    ArithmeticIssue
  ]

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

data WarningDetailPolicy
  = PreferDetail
  | IgnoreDetail

data WarningDescriptor = MkWarningDescriptor
  { warningDefaultSeverity :: WarningSeverity,
    warningDefaultMessage :: Text,
    warningDetailPolicy :: WarningDetailPolicy
  }

warningDescriptor :: WarningCode -> WarningDescriptor
warningDescriptor = \case
  UnsupportedConstruct ->
    MkWarningDescriptor WarnHigh "Unsupported construct" PreferDetail
  BestEffortSubshell ->
    MkWarningDescriptor WarnHigh "Subshell does not isolate environment in fish; best-effort translation emitted" IgnoreDetail
  ExecFdRedirect ->
    MkWarningDescriptor WarnMedium "exec with file descriptor redirection may require manual adjustment in fish" IgnoreDetail
  BackgroundTracking ->
    MkWarningDescriptor WarnHigh "Monk-managed background job IDs are only guaranteed for translated wait; PID-specific uses such as kill $! require manual review" IgnoreDetail
  SetOptionIssue ->
    MkWarningDescriptor WarnHigh "Bash set options require manual review" PreferDetail
  ReadIssue ->
    MkWarningDescriptor WarnMedium "read semantics may differ between bash and fish" PreferDetail
  ShoptIgnored ->
    MkWarningDescriptor WarnHigh "shopt has no fish equivalent; ignored" IgnoreDetail
  TrapIssue ->
    MkWarningDescriptor WarnMedium "trap handling requires manual review" PreferDetail
  ShiftIssue ->
    MkWarningDescriptor WarnMedium "shift translation requires manual review" PreferDetail
  ReadonlyNotEnforced ->
    MkWarningDescriptor WarnHigh "readonly/declare -r has no direct fish equivalent; emitted set without enforcing readonly" IgnoreDetail
  DeclareIssue ->
    MkWarningDescriptor WarnMedium "declare translation requires manual review" PreferDetail
  ScopeIssue ->
    MkWarningDescriptor WarnMedium "scope translation requires manual review" PreferDetail
  UnsetIssue ->
    MkWarningDescriptor WarnMedium "unset translation requires manual review" PreferDetail
  ForArithmeticIssue ->
    MkWarningDescriptor WarnMedium "arithmetic for-loop translation requires manual review" PreferDetail
  ArithmeticIssue ->
    MkWarningDescriptor WarnHigh "arithmetic translation may lose side effects" PreferDetail

warningCodeSeverity :: WarningCode -> WarningSeverity
warningCodeSeverity = warningDefaultSeverity . warningDescriptor

warnMessage :: Warning -> Text
warnMessage MkWarning {warnCode, warnDetail} =
  let descriptor = warningDescriptor warnCode
   in case (warningDetailPolicy descriptor, warnDetail) of
        (PreferDetail, Just detail) -> detail
        _ -> warningDefaultMessage descriptor

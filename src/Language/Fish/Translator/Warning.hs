{-# LANGUAGE LambdaCase #-}

-- | Internal warning model used while translating ShellCheck tokens.
--
-- Public consumers receive 'Monk.Translation.Types.Diagnostic' values instead;
-- this module keeps translation policy details out of the 0.4 API.
module Language.Fish.Translator.Warning
  ( WarningSeverity (..),
    WarningCode (..),
    Warning (..),
    TranslateError (..),
    allWarningCodes,
    warningCodeSeverity,
    warningCodeText,
    warnMessage,
  )
where

import Language.Fish.DSL (SourceRange)

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
  | SourceIssue
  | ProcessSubstitutionIssue
  | ShoptIgnored
  | TrapIssue
  | ShiftIssue
  | ReadonlyNotEnforced
  | DeclareIssue
  | ScopeIssue
  | UnsetIssue
  | ForArithmeticIssue
  | ArithmeticIssue
  | HereStringIssue
  | CompatibilityFallback
  deriving stock (Eq, Ord, Show)

allWarningCodes :: [WarningCode]
allWarningCodes =
  [ UnsupportedConstruct,
    BestEffortSubshell,
    ExecFdRedirect,
    BackgroundTracking,
    SetOptionIssue,
    ReadIssue,
    SourceIssue,
    ProcessSubstitutionIssue,
    ShoptIgnored,
    TrapIssue,
    ShiftIssue,
    ReadonlyNotEnforced,
    DeclareIssue,
    ScopeIssue,
    UnsetIssue,
    ForArithmeticIssue,
    ArithmeticIssue,
    HereStringIssue,
    CompatibilityFallback
  ]

data Warning = MkWarning
  { warnCode :: WarningCode,
    warnSeverity :: WarningSeverity,
    warnDetail :: Maybe Text,
    warnRange :: Maybe SourceRange
  }
  deriving stock (Show, Eq)

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
    warningDetailPolicy :: WarningDetailPolicy,
    warningStableCode :: Text
  }

warningDescriptor :: WarningCode -> WarningDescriptor
warningDescriptor = \case
  UnsupportedConstruct -> descriptor WarnHigh "Unsupported construct" PreferDetail "monk.unsupported"
  BestEffortSubshell -> descriptor WarnHigh "Subshell does not isolate environment in fish; best-effort translation emitted" IgnoreDetail "monk.subshell.best-effort"
  ExecFdRedirect -> descriptor WarnMedium "exec with file descriptor redirection may require manual adjustment in fish" IgnoreDetail "monk.exec-fd"
  BackgroundTracking -> descriptor WarnHigh "Monk-managed background job IDs are only guaranteed for translated wait; PID-specific uses such as kill $! require manual review" IgnoreDetail "monk.background-tracking"
  SetOptionIssue -> descriptor WarnHigh "Bash set options require manual review" PreferDetail "monk.set-option"
  ReadIssue -> descriptor WarnMedium "read semantics may differ between bash and fish" PreferDetail "monk.read"
  SourceIssue -> descriptor WarnMedium "source command requires manual review" PreferDetail "monk.source"
  ProcessSubstitutionIssue -> descriptor WarnMedium "process substitution translation requires manual review" PreferDetail "monk.process-substitution"
  ShoptIgnored -> descriptor WarnHigh "shopt has no fish equivalent; ignored" IgnoreDetail "monk.shopt-ignored"
  TrapIssue -> descriptor WarnMedium "trap handling requires manual review" PreferDetail "monk.trap"
  ShiftIssue -> descriptor WarnMedium "shift translation requires manual review" PreferDetail "monk.shift"
  ReadonlyNotEnforced -> descriptor WarnHigh "readonly/declare -r has no direct fish equivalent; emitted set without enforcing readonly" IgnoreDetail "monk.readonly"
  DeclareIssue -> descriptor WarnMedium "declare translation requires manual review" PreferDetail "monk.declare"
  ScopeIssue -> descriptor WarnMedium "scope translation requires manual review" PreferDetail "monk.scope"
  UnsetIssue -> descriptor WarnMedium "unset translation requires manual review" PreferDetail "monk.unset"
  ForArithmeticIssue -> descriptor WarnMedium "arithmetic for-loop translation requires manual review" PreferDetail "monk.for-arithmetic"
  ArithmeticIssue -> descriptor WarnHigh "arithmetic translation may lose side effects" PreferDetail "monk.arithmetic"
  HereStringIssue -> descriptor WarnMedium "here-string translation requires manual review" PreferDetail "monk.here-string"
  CompatibilityFallback -> descriptor WarnHigh "Bash compatibility fallback emitted" PreferDetail "monk.compatibility-fallback"
  where
    descriptor = MkWarningDescriptor

warningCodeSeverity :: WarningCode -> WarningSeverity
warningCodeSeverity = warningDefaultSeverity . warningDescriptor

warningCodeText :: WarningCode -> Text
warningCodeText = warningStableCode . warningDescriptor

warnMessage :: Warning -> Text
warnMessage MkWarning {warnCode, warnDetail} =
  let warning = warningDescriptor warnCode
   in case (warningDetailPolicy warning, warnDetail) of
        (PreferDetail, Just detail) -> detail
        _ -> warningDefaultMessage warning

-- | One typed inventory for execution, CLI selection, and release receipts.
module Monk.Tooling.Runtime.Suite
  ( Suite (..),
    allSuites,
    suiteName,
    parseSuite,
    suiteNeedsTranslator,
    requiredReceiptSuites,
  )
where

data Suite
  = CallbackDiagnostics
  | Descriptors
  | Digest
  | DirectOutput
  | DirectorySignals
  | Exec
  | Expansion
  | NativeLauncher
  | PatternParts
  | Portable
  | Printf
  | ProcessSubstitution
  | Protocol
  | Read
  | Session
  | Signals
  | ChildTransport
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Suites accepting the native runtime executable. Digest uses the separate
-- compiler-support checker and remains available through explicit selection.
allSuites :: [Suite]
allSuites = filter (/= Digest) [minBound .. maxBound]

suiteName :: Suite -> String
suiteName suite = case suite of
  CallbackDiagnostics -> "callback-diagnostics"
  Descriptors -> "descriptors"
  Digest -> "digest"
  DirectOutput -> "direct-output"
  DirectorySignals -> "directory-signals"
  Exec -> "exec"
  Expansion -> "expansion"
  NativeLauncher -> "native-launcher"
  PatternParts -> "pattern-parts"
  Portable -> "portable"
  Printf -> "printf"
  ProcessSubstitution -> "process-substitution"
  Protocol -> "protocol"
  Read -> "read"
  Session -> "session"
  Signals -> "signals"
  ChildTransport -> "child-transport"

parseSuite :: String -> Either String Suite
parseSuite name = maybe (Left ("unknown runtime check suite: " <> name)) Right (find ((== name) . suiteName) [minBound .. maxBound])

suiteNeedsTranslator :: Suite -> Bool
suiteNeedsTranslator suite = suite `elem` [CallbackDiagnostics, DirectOutput, DirectorySignals, NativeLauncher]

-- Preserve the established receipt contract and order.
requiredReceiptSuites :: [Suite]
requiredReceiptSuites =
  [ Protocol,
    Portable,
    Printf,
    Expansion,
    Session,
    Descriptors,
    Read,
    ProcessSubstitution,
    PatternParts,
    Exec,
    Signals,
    DirectOutput,
    NativeLauncher,
    ChildTransport
  ]

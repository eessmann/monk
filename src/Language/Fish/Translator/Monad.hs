{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Monad
  ( TranslateM,
    Hoisted (..),
    HoistedM,
    TranslateEffs,
    TranslateState (..),
    TranslateConfig (..),
    defaultConfig,
    TranslateError (..),
    Warning (..),
    WarningCode (..),
    WarningSeverity (..),
    HelperId (..),
    TranslationContext (..),
    runTranslate,
    runTranslateWithPositions,
    evalTranslate,
    evalTranslateWithPositions,
    warnMessage,
    stateWarnings,
    stateErrexitEnabled,
    statePipefailEnabled,
    addWarning,
    addWarningOnce,
    unsupported,
    noteUnsupported,
    unsupportedStmt,
    ensureHelper,
    withFunctionScope,
    withCommandSubstScope,
    addLocalVars,
    isLocalVar,
    withTokenRange,
    isErrexitEnabled,
    isPipefailEnabled,
    setErrexitEnabled,
    setPipefailEnabled,
    preambleStatements,
  )
where

import Prelude hiding (Reader, State, ask, get, gets, modify, runReader, runState)
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Language.Fish.AST
import Language.Fish.Translator.Hoist (Hoisted (..))
import Polysemy (Sem, run)
import Polysemy.Error (Error, runError, throw)
import Polysemy.Input (Input, input, runInputConst)
import Polysemy.Reader (Reader, ask, runReader)
import Polysemy.State (State, get, gets, modify, runState)
import Polysemy.Writer (Writer, runWriter, tell)
import ShellCheck.AST (Id, Token, getId)
import ShellCheck.Interface (Position (..))

-- | Configuration flags controlling translation behavior
data TranslateConfig = MkTranslateConfig
  { -- | Fail on unsupported constructs
    strictMode :: Bool
  }
  deriving stock (Show, Eq)

-- | Default translation settings used by the public API.
defaultConfig :: TranslateConfig
defaultConfig =
  MkTranslateConfig
    { strictMode = False
    }

-- | Context flags describing where we are in the script
data TranslationContext = MkTranslationContext
  { inFunction :: Bool,
    inCommandSubst :: Bool,
    localVars :: Set.Set Text
  }
  deriving stock (Show, Eq)

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

data HelperId
  = HelperPipefail
  | HelperBackground
  | HelperReadRuntime
  | HelperProcSubOut
  deriving stock (Eq, Ord, Show)

-- | Structured warning payload used for both surfaced diagnostics and strict errors.
data Warning = MkWarning
  { warnCode :: WarningCode,
    warnSeverity :: WarningSeverity,
    warnDetail :: Maybe Text,
    warnRange :: Maybe SourceRange
  }
  deriving stock (Show, Eq)

-- | Translation errors for unsupported or invalid constructs
data TranslateError
  = Unsupported Warning
  | InternalError Text
  deriving stock (Show, Eq)

-- | Mutable translation state
data TranslateState = MkTranslateState
  { warnings :: [Warning],
    context :: TranslationContext,
    tokenRanges :: M.Map Id SourceRange,
    rangeStack :: [SourceRange],
    errexitEnabled :: Bool,
    pipefailEnabled :: Bool,
    registeredHelpers :: Set.Set HelperId,
    warningOnceCodes :: Set.Set WarningCode,
    preamble :: [FishStatement]
  }
  deriving stock (Show, Eq)

type TranslateEffs =
  [ Input (M.Map Id SourceRange),
    Reader TranslateConfig,
    State TranslateState,
    Writer [Warning],
    Error TranslateError
  ]

type TranslateM = Sem TranslateEffs

type HoistedM a = TranslateM (Hoisted a)

runTranslate :: TranslateConfig -> TranslateM a -> Either TranslateError (a, TranslateState)
runTranslate cfg = runTranslateWithPositions cfg mempty

runTranslateWithPositions ::
  TranslateConfig ->
  M.Map Id (Position, Position) ->
  TranslateM a ->
  Either TranslateError (a, TranslateState)
runTranslateWithPositions cfg positions m =
  let ranges = toSourceRanges positions
      initState =
        MkTranslateState
          { warnings = [],
            context = MkTranslationContext False False Set.empty,
            tokenRanges = ranges,
            rangeStack = [],
            errexitEnabled = False,
            pipefailEnabled = False,
            registeredHelpers = Set.empty,
            warningOnceCodes = Set.empty,
            preamble = []
          }
      result =
        run
          . runError
          . runWriter
          . runState initState
          . runReader cfg
          . runInputConst ranges
          $ m
   in case result of
        Left err -> Left err
        Right (warns, (st, a)) ->
          Right (a, st {warnings = warnings st <> warns})

evalTranslate :: TranslateConfig -> TranslateM a -> Either TranslateError a
evalTranslate cfg m = fmap fst (runTranslate cfg m)

evalTranslateWithPositions ::
  TranslateConfig ->
  M.Map Id (Position, Position) ->
  TranslateM a ->
  Either TranslateError a
evalTranslateWithPositions cfg positions m =
  fmap fst (runTranslateWithPositions cfg positions m)

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

warningSeverity :: WarningCode -> WarningSeverity
warningSeverity = \case
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

mkWarning :: WarningCode -> Maybe Text -> Maybe SourceRange -> Warning
mkWarning code detail range =
  MkWarning
    { warnCode = code,
      warnSeverity = warningSeverity code,
      warnDetail = detail,
      warnRange = range
    }

currentRange :: TranslateM (Maybe SourceRange)
currentRange = gets (listToMaybe . rangeStack)

stateWarnings :: TranslateState -> [Warning]
stateWarnings = warnings

stateErrexitEnabled :: TranslateState -> Bool
stateErrexitEnabled = errexitEnabled

statePipefailEnabled :: TranslateState -> Bool
statePipefailEnabled = pipefailEnabled

addWarning :: WarningCode -> Maybe Text -> TranslateM ()
addWarning code detail = do
  range <- currentRange
  tell [mkWarning code detail range]

addWarningOnce :: WarningCode -> Maybe Text -> TranslateM ()
addWarningOnce code detail = do
  st <- get
  if Set.member code (warningOnceCodes st)
    then pure ()
    else do
      modify (\s -> s {warningOnceCodes = Set.insert code (warningOnceCodes s)})
      addWarning code detail

unsupported :: WarningCode -> Maybe Text -> TranslateM ()
unsupported code detail = do
  cfg <- ask
  range <- currentRange
  let warning = mkWarning code detail range
  if strictMode cfg
    then throw (Unsupported warning)
    else tell [warning]

noteUnsupported :: WarningCode -> Maybe Text -> TranslateM FishStatement
noteUnsupported code detail = do
  unsupported code detail
  pure (Comment ("NOTE: " <> warnMessage (mkWarning code detail Nothing)))

unsupportedStmt :: WarningCode -> Maybe Text -> TranslateM FishStatement
unsupportedStmt code detail = do
  unsupported code detail
  pure (Comment ("Unsupported: " <> warnMessage (mkWarning code detail Nothing)))

ensureHelper :: HelperId -> [FishStatement] -> TranslateM ()
ensureHelper helper stmts = do
  st <- get
  if Set.member helper (registeredHelpers st)
    then pure ()
    else
      modify
        ( \s ->
            s
              { registeredHelpers = Set.insert helper (registeredHelpers s),
                preamble = preamble s <> stmts
              }
        )

withFunctionScope :: TranslateM a -> TranslateM a
withFunctionScope action = do
  st <- get
  let ctx = context st
      newCtx = ctx {inFunction = True, localVars = Set.empty}
  modify (\s -> s {context = newCtx})
  result <- action
  modify (\s -> s {context = ctx})
  pure result

withCommandSubstScope :: TranslateM a -> TranslateM a
withCommandSubstScope action = do
  st <- get
  let ctx = context st
      newCtx = ctx {inCommandSubst = True}
  modify (\s -> s {context = newCtx})
  result <- action
  modify (\s -> s {context = ctx})
  pure result

addLocalVars :: [Text] -> TranslateM ()
addLocalVars names =
  modify
    ( \s ->
        let ctx = context s
         in s {context = ctx {localVars = Set.union (localVars ctx) (Set.fromList names)}}
    )

isLocalVar :: Text -> TranslateM Bool
isLocalVar name = do
  ctx <- gets context
  pure (Set.member name (localVars ctx))

withTokenRange :: Token -> TranslateM a -> TranslateM a
withTokenRange tok action = do
  let tokId = getId tok
  ranges <- input
  let mRange = M.lookup tokId ranges
  case mRange of
    Nothing -> action
    Just range -> do
      modify (\st -> st {rangeStack = range : rangeStack st})
      result <- action
      modify (\st -> st {rangeStack = drop 1 (rangeStack st)})
      pure result

isErrexitEnabled :: TranslateM Bool
isErrexitEnabled = gets errexitEnabled

isPipefailEnabled :: TranslateM Bool
isPipefailEnabled = gets pipefailEnabled

setErrexitEnabled :: Bool -> TranslateM ()
setErrexitEnabled enabled =
  modify (\st -> st {errexitEnabled = enabled})

setPipefailEnabled :: Bool -> TranslateM ()
setPipefailEnabled enabled =
  modify (\st -> st {pipefailEnabled = enabled})

preambleStatements :: TranslateM [FishStatement]
preambleStatements = gets preamble

toSourceRanges :: M.Map Id (Position, Position) -> M.Map Id SourceRange
toSourceRanges = M.map (\(startPos, endPos) -> MkSourceRange (toSourcePos startPos) (toSourcePos endPos))

toSourcePos :: Position -> SourcePos
toSourcePos pos =
  MkSourcePos
    { srcFile = toText (posFile pos),
      srcLine = fromInteger (posLine pos),
      srcColumn = fromInteger (posColumn pos)
    }

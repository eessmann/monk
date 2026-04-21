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
import Monk.Translation.Types
  ( TranslateConfig (..),
    TranslateError (..),
    Warning (..),
    WarningCode (..),
    WarningSeverity (..),
    defaultConfig,
    warnMessage,
    warningCodeSeverity,
  )
import Polysemy (Sem, run)
import Polysemy.Error (Error, catch, runError, throw)
import Polysemy.Input (Input, input, runInputConst)
import Polysemy.Reader (Reader, ask, runReader)
import Polysemy.State (State, get, gets, modify, runState)
import ShellCheck.AST (Id, Token, getId)
import ShellCheck.Interface (Position (..))

-- | Context flags describing where we are in the script
data TranslationContext = MkTranslationContext
  { inFunction :: Bool,
    inCommandSubst :: Bool,
    localVars :: Set.Set Text
  }
  deriving stock (Show, Eq)

data HelperId
  = HelperPipefail
  | HelperBackground
  | HelperReadRuntime
  | HelperProcSubOut
  deriving stock (Eq, Ord, Show)

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
          . runState initState
          . runReader cfg
          . runInputConst ranges
          $ m
   in case result of
        Left err -> Left err
        Right (st, a) -> Right (a, st)

evalTranslate :: TranslateConfig -> TranslateM a -> Either TranslateError a
evalTranslate cfg m = fmap fst (runTranslate cfg m)

evalTranslateWithPositions ::
  TranslateConfig ->
  M.Map Id (Position, Position) ->
  TranslateM a ->
  Either TranslateError a
evalTranslateWithPositions cfg positions m =
  fmap fst (runTranslateWithPositions cfg positions m)

mkWarning :: WarningCode -> Maybe Text -> Maybe SourceRange -> Warning
mkWarning code detail range =
  MkWarning
    { warnCode = code,
      warnSeverity = warningCodeSeverity code,
      warnDetail = detail,
      warnRange = range
    }

currentRange :: TranslateM (Maybe SourceRange)
currentRange = gets (listToMaybe . rangeStack)

appendWarning :: Warning -> TranslateM ()
appendWarning warning =
  modify (\st -> st {warnings = warnings st <> [warning]})

stateWarnings :: TranslateState -> [Warning]
stateWarnings = warnings

stateErrexitEnabled :: TranslateState -> Bool
stateErrexitEnabled = errexitEnabled

statePipefailEnabled :: TranslateState -> Bool
statePipefailEnabled = pipefailEnabled

addWarning :: WarningCode -> Maybe Text -> TranslateM ()
addWarning code detail = do
  range <- currentRange
  appendWarning (mkWarning code detail range)

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
    else appendWarning warning

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

withScopedField ::
  (TranslateState -> a) ->
  (a -> TranslateState -> TranslateState) ->
  (a -> a) ->
  TranslateM b ->
  TranslateM b
withScopedField getField setField updateField action = do
  original <- gets getField
  modify (setField (updateField original))
  let restore = modify (setField original)
  result <-
    catch
      action
      ( \err -> do
          restore
          throw err
      )
  restore
  pure result

withScopedContext :: (TranslationContext -> TranslationContext) -> TranslateM a -> TranslateM a
withScopedContext =
  withScopedField context (\ctx st -> st {context = ctx})

withScopedRange :: SourceRange -> TranslateM a -> TranslateM a
withScopedRange range =
  withScopedField rangeStack (\ranges st -> st {rangeStack = ranges}) (range :)

withFunctionScope :: TranslateM a -> TranslateM a
withFunctionScope =
  withScopedContext (\ctx -> ctx {inFunction = True, localVars = Set.empty})

withCommandSubstScope :: TranslateM a -> TranslateM a
withCommandSubstScope =
  withScopedContext (\ctx -> ctx {inCommandSubst = True})

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
    Just range -> withScopedRange range action

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

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Monad
  ( TranslateM,
    MonadTranslate,
    Hoisted (..),
    HoistedM,
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

import Control.Monad.Except (MonadError, catchError, throwError)
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Language.Fish.Translator.DSL
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

data TranslateEnv = MkTranslateEnv
  { envConfig :: TranslateConfig,
    envTokenRanges :: M.Map Id SourceRange
  }
  deriving stock (Show, Eq)

-- | Mutable translation state
data TranslateState = MkTranslateState
  { warnings :: [Warning],
    context :: TranslationContext,
    rangeStack :: [SourceRange],
    errexitEnabled :: Bool,
    pipefailEnabled :: Bool,
    registeredHelpers :: Set.Set HelperId,
    warningOnceCodes :: Set.Set WarningCode,
    preamble :: [FishStatement]
  }
  deriving stock (Show, Eq)

type MonadTranslate m =
  ( MonadReader TranslateEnv m,
    MonadState TranslateState m,
    MonadError TranslateError m
  )

newtype TranslateM a
  = MkTranslateM
      (ReaderT TranslateEnv (StateT TranslateState (Either TranslateError)) a)
  deriving newtype (Functor, Applicative, Monad, MonadReader TranslateEnv, MonadState TranslateState, MonadError TranslateError)

type HoistedM a = TranslateM (Hoisted a)

runTranslate :: TranslateConfig -> TranslateM a -> Either TranslateError (a, TranslateState)
runTranslate cfg = runTranslateWithPositions cfg mempty

runTranslateWithPositions ::
  TranslateConfig ->
  M.Map Id (Position, Position) ->
  TranslateM a ->
  Either TranslateError (a, TranslateState)
runTranslateWithPositions cfg positions (MkTranslateM m) =
  let ranges = toSourceRanges positions
      env =
        MkTranslateEnv
          { envConfig = cfg,
            envTokenRanges = ranges
          }
      initState =
        MkTranslateState
          { warnings = [],
            context = MkTranslationContext False False Set.empty,
            rangeStack = [],
            errexitEnabled = False,
            pipefailEnabled = False,
            registeredHelpers = Set.empty,
            warningOnceCodes = Set.empty,
            preamble = []
          }
   in runStateT (runReaderT m env) initState

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

currentRange :: (MonadState TranslateState m) => m (Maybe SourceRange)
currentRange = gets (listToMaybe . rangeStack)

appendWarning :: (MonadState TranslateState m) => Warning -> m ()
appendWarning warning =
  modify (\st -> st {warnings = warnings st <> [warning]})

stateWarnings :: TranslateState -> [Warning]
stateWarnings = warnings

stateErrexitEnabled :: TranslateState -> Bool
stateErrexitEnabled = errexitEnabled

statePipefailEnabled :: TranslateState -> Bool
statePipefailEnabled = pipefailEnabled

addWarning :: (MonadState TranslateState m) => WarningCode -> Maybe Text -> m ()
addWarning code detail = do
  range <- currentRange
  appendWarning (mkWarning code detail range)

addWarningOnce :: (MonadState TranslateState m) => WarningCode -> Maybe Text -> m ()
addWarningOnce code detail = do
  st <- get
  if Set.member code (warningOnceCodes st)
    then pure ()
    else do
      modify (\s -> s {warningOnceCodes = Set.insert code (warningOnceCodes s)})
      addWarning code detail

unsupported :: (MonadTranslate m) => WarningCode -> Maybe Text -> m ()
unsupported code detail = do
  cfg <- asks envConfig
  range <- currentRange
  let warning = mkWarning code detail range
  if strictMode cfg
    then throwError (Unsupported warning)
    else appendWarning warning

noteUnsupported :: (MonadTranslate m) => WarningCode -> Maybe Text -> m FishStatement
noteUnsupported code detail = do
  unsupported code detail
  pure (Comment ("NOTE: " <> warnMessage (mkWarning code detail Nothing)))

unsupportedStmt :: (MonadTranslate m) => WarningCode -> Maybe Text -> m FishStatement
unsupportedStmt code detail = do
  unsupported code detail
  pure (Comment ("Unsupported: " <> warnMessage (mkWarning code detail Nothing)))

ensureHelper :: (MonadState TranslateState m) => HelperId -> [FishStatement] -> m ()
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
  (MonadState TranslateState m, MonadError TranslateError m) =>
  (TranslateState -> a) ->
  (a -> TranslateState -> TranslateState) ->
  (a -> a) ->
  m b ->
  m b
withScopedField getField setField updateField action = do
  original <- gets getField
  modify (setField (updateField original))
  let restore = modify (setField original)
  result <-
    catchError
      action
      ( \err -> do
          restore
          throwError err
      )
  restore
  pure result

withScopedContext ::
  (MonadState TranslateState m, MonadError TranslateError m) =>
  (TranslationContext -> TranslationContext) ->
  m a ->
  m a
withScopedContext =
  withScopedField context (\ctx st -> st {context = ctx})

withScopedRange ::
  (MonadState TranslateState m, MonadError TranslateError m) =>
  SourceRange ->
  m a ->
  m a
withScopedRange range =
  withScopedField rangeStack (\ranges st -> st {rangeStack = ranges}) (range :)

withFunctionScope ::
  (MonadState TranslateState m, MonadError TranslateError m) =>
  m a ->
  m a
withFunctionScope =
  withScopedContext (\ctx -> ctx {inFunction = True, localVars = Set.empty})

withCommandSubstScope ::
  (MonadState TranslateState m, MonadError TranslateError m) =>
  m a ->
  m a
withCommandSubstScope =
  withScopedContext (\ctx -> ctx {inCommandSubst = True})

addLocalVars :: (MonadState TranslateState m) => [Text] -> m ()
addLocalVars names =
  modify
    ( \s ->
        let ctx = context s
         in s {context = ctx {localVars = Set.union (localVars ctx) (Set.fromList names)}}
    )

isLocalVar :: (MonadState TranslateState m) => Text -> m Bool
isLocalVar name = do
  ctx <- gets context
  pure (Set.member name (localVars ctx))

withTokenRange ::
  (MonadReader TranslateEnv m, MonadState TranslateState m, MonadError TranslateError m) =>
  Token ->
  m a ->
  m a
withTokenRange tok action = do
  let tokId = getId tok
  ranges <- asks envTokenRanges
  let mRange = M.lookup tokId ranges
  case mRange of
    Nothing -> action
    Just range -> withScopedRange range action

isErrexitEnabled :: (MonadState TranslateState m) => m Bool
isErrexitEnabled = gets errexitEnabled

isPipefailEnabled :: (MonadState TranslateState m) => m Bool
isPipefailEnabled = gets pipefailEnabled

setErrexitEnabled :: (MonadState TranslateState m) => Bool -> m ()
setErrexitEnabled enabled =
  modify (\st -> st {errexitEnabled = enabled})

setPipefailEnabled :: (MonadState TranslateState m) => Bool -> m ()
setPipefailEnabled enabled =
  modify (\st -> st {pipefailEnabled = enabled})

preambleStatements :: (MonadState TranslateState m) => m [FishStatement]
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

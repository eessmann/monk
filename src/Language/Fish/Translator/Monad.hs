{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Monad
  ( TranslateM,
    TranslateState (..),
    TranslateConfig (..),
    defaultConfig,
    TranslateError (..),
    Warning (..),
    TranslationContext (..),
    runTranslate,
    runTranslateWithPositions,
    evalTranslate,
    evalTranslateWithPositions,
    addWarning,
    unsupported,
    unsupportedStmt,
    withFunctionScope,
    addLocalVars,
    isLocalVar,
    withTokenRange,
  )
where

import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Language.Fish.AST (FishStatement (..), SourcePos (..), SourceRange (..))
import ShellCheck.AST (Id, Token, getId)
import ShellCheck.Interface (Position (..))

-- | Configuration flags controlling translation behavior
data TranslateConfig = TranslateConfig
  { -- | Fail on unsupported constructs
    strictMode :: Bool,
    -- | Keep original comments, if available
    preserveComments :: Bool
  }
  deriving stock (Show, Eq)

defaultConfig :: TranslateConfig
defaultConfig =
  TranslateConfig
    { strictMode = False,
      preserveComments = True
    }

-- | Context flags describing where we are in the script
data TranslationContext = TranslationContext
  { inFunction :: Bool,
    inLoop :: Bool,
    localVars :: Set.Set Text
  }
  deriving stock (Show, Eq)

-- | Lightweight warning structure for non-fatal issues
data Warning = Warning
  { warnMessage :: Text,
    warnRange :: Maybe SourceRange
  }
  deriving stock (Show, Eq)

-- | Translation errors for unsupported or invalid constructs
data TranslateError
  = Unsupported Text (Maybe SourceRange)
  | InternalError Text
  deriving stock (Show, Eq)

-- | Mutable translation state
data TranslateState = TranslateState
  { -- | map Fish fragments back to source hints
    sourceMap :: M.Map SourceRange Text,
    warnings :: [Warning],
    context :: TranslationContext,
    config :: TranslateConfig,
    tokenRanges :: M.Map Id SourceRange,
    rangeStack :: [SourceRange]
  }
  deriving stock (Show, Eq)

type TranslateM = StateT TranslateState (Either TranslateError)

runTranslate :: TranslateConfig -> TranslateM a -> Either TranslateError (a, TranslateState)
runTranslate cfg = runTranslateWithPositions cfg mempty

runTranslateWithPositions ::
  TranslateConfig ->
  M.Map Id (Position, Position) ->
  TranslateM a ->
  Either TranslateError (a, TranslateState)
runTranslateWithPositions cfg positions m =
  let initState =
        TranslateState
          { sourceMap = mempty,
            warnings = [],
            context = TranslationContext False False Set.empty,
            config = cfg,
            tokenRanges = toSourceRanges positions,
            rangeStack = []
          }
   in runStateT m initState

evalTranslate :: TranslateConfig -> TranslateM a -> Either TranslateError a
evalTranslate cfg m = fmap fst (runTranslate cfg m)

evalTranslateWithPositions ::
  TranslateConfig ->
  M.Map Id (Position, Position) ->
  TranslateM a ->
  Either TranslateError a
evalTranslateWithPositions cfg positions m =
  fmap fst (runTranslateWithPositions cfg positions m)

addWarning :: Text -> TranslateM ()
addWarning msg = do
  st <- get
  let range = listToMaybe (rangeStack st)
  modify' (\s -> s {warnings = warnings s <> [Warning msg range]})

unsupported :: Text -> TranslateM ()
unsupported msg = do
  st <- get
  let range = listToMaybe (rangeStack st)
      isStrict = strictMode (config st)
  if isStrict
    then lift (Left (Unsupported msg range))
    else modify' (\s -> s {warnings = warnings s <> [Warning msg range]})

unsupportedStmt :: Text -> TranslateM FishStatement
unsupportedStmt msg = do
  unsupported msg
  pure (Comment ("Unsupported: " <> msg))

withFunctionScope :: TranslateM a -> TranslateM a
withFunctionScope action = do
  st <- get
  let ctx = context st
      newCtx = ctx {inFunction = True, localVars = Set.empty}
  modify' (\s -> s {context = newCtx})
  result <- action
  modify' (\s -> s {context = ctx})
  pure result

addLocalVars :: [Text] -> TranslateM ()
addLocalVars names =
  modify'
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
  mRange <- gets (M.lookup tokId . tokenRanges)
  case mRange of
    Nothing -> action
    Just range -> do
      modify' (\st -> st {rangeStack = range : rangeStack st})
      result <- action
      modify' (\st -> st {rangeStack = drop 1 (rangeStack st)})
      pure result

toSourceRanges :: M.Map Id (Position, Position) -> M.Map Id SourceRange
toSourceRanges = M.map (\(startPos, endPos) -> SourceRange (toSourcePos startPos) (toSourcePos endPos))

toSourcePos :: Position -> SourcePos
toSourcePos pos =
  SourcePos
    { srcFile = toText (posFile pos),
      srcLine = fromInteger (posLine pos),
      srcColumn = fromInteger (posColumn pos)
    }

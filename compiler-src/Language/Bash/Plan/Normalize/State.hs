{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Normalization separates lexical context, finite mutable flow facts, and
-- accumulated discoveries. Every mutation opens a fresh nominal flow world.
module Language.Bash.Plan.Normalize.State
  ( Normalization,
    NormalizationContext (..),
    FactData (..),
    Discoveries (..),
    FlowFacts,
    initialNormalization,
    rebaseNormalization,
    updateNormalization,
    withNormalizationFacts,
    DenseProof,
    lookupDense,
    DenseUpdate,
    writeDenseAt,
    appendDenseBy,
    transitionDense,
    nConfig,
    nPositions,
    nRuntimeOrigin,
    nConstants,
    nNumeric,
    nContinueNumeric,
    nBreakNumeric,
    nVariables,
    nFunctions,
    nLocalFunctions,
    nResolutionFunctions,
    nDefinitions,
    nFunctionDependencies,
    nCurrentDependencies,
    nAllFunctions,
    nFunction,
    nControl,
    enterNormalizationLoop,
    nLocals,
    nDirect,
    nReserved,
    nDocument,
    nSourceStack,
    nResolutionStable,
    nCommandLine,
    nFunctionBodies,
    nSourceReturns,
    nDescriptors,
    nWritableDescriptors,
    nDirectoryFacts,
    nDirectoryOutcomes,
    nEvaluatedPrograms,
    nArrays,
    nErrTrapWrites,
  )
where

import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Language.Bash.Plan qualified as P
import Language.Bash.Plan.Control (Control, LoopKey, LoopTarget)
import Language.Bash.Plan.Control qualified as Control
import Language.Bash.Plan.Directory qualified as Directory
import Language.Bash.Plan.Facts (ArrayShape (..), DenseLength, appendDense, writeDense)
import Language.Bash.Plan.Identity (DefinitionIdentity)
import Monk.Compiler.Index (World, withWorld)
import Monk.Translation.Types (TranslateConfig)
import ShellCheck.AST (Id)
import ShellCheck.Interface (Position)

data NormalizationContext scope = NormalizationContext
  { contextConfig :: TranslateConfig,
    contextPositions :: M.Map Id (Position, Position),
    contextRuntimeOrigin :: Text,
    contextControl :: Control scope,
    contextDirect :: Bool,
    contextDocument :: Maybe Text,
    contextSourceStack :: [Text],
    contextCommandLine :: Int,
    contextDescriptors :: S.Set Int,
    contextWritableDescriptors :: S.Set Int
  }

data FactData = FactData
  { factConstants :: M.Map Text Text,
    factNumeric :: S.Set Text,
    factContinueNumeric :: M.Map LoopKey (S.Set Text),
    factBreakNumeric :: M.Map LoopKey (S.Set Text),
    factVariables :: S.Set Text,
    factFunctions :: S.Set Text,
    factLocalFunctions :: S.Set Text,
    factResolutionFunctions :: S.Set Text,
    factDefinitions :: M.Map Text DefinitionIdentity,
    factFunctionDependencies :: M.Map Text (M.Map Text DefinitionIdentity),
    factCurrentDependencies :: M.Map Text DefinitionIdentity,
    factLocals :: S.Set Text,
    factResolutionStable :: Bool,
    factFunctionBodies :: M.Map Text (P.ScopedBody Control.FunctionRootKind),
    factSourceReturns :: S.Set P.SourceEntryContext,
    factDirectoryFacts :: Directory.DirectoryFacts,
    factDirectoryOutcomes :: Maybe (Directory.DirectoryFacts, Directory.DirectoryFacts),
    factArrays :: M.Map Text ArrayShape,
    factErrTrapWrites :: S.Set Text
  }

data Discoveries = Discoveries
  { discoveredAllFunctions :: S.Set Text,
    discoveredReserved :: S.Set Text,
    discoveredEvaluatedPrograms :: [Text]
  }

type role FlowFacts nominal

data FlowFacts world = FlowFacts (World world) FactData

data Normalization scope = forall world. Normalization (NormalizationContext scope) (FlowFacts world) Discoveries

-- | Evidence is minted only by a lookup on the current opaque fact owner.
-- Reading an unindexed shape from nArrays cannot manufacture a world proof.
type role DenseProof nominal

data DenseProof (world :: Type) = DenseProof Text DenseLength

-- A checked change retains the fact world and binding identity until applied.
type role DenseUpdate nominal

data DenseUpdate (world :: Type) = DenseUpdate Text DenseLength

lookupDense :: FlowFacts world -> Text -> Maybe (DenseProof world)
lookupDense (FlowFacts _ facts) name = case M.lookup name (factArrays facts) of
  Just (DenseArray size) -> Just (DenseProof name size)
  _ -> Nothing

writeDenseAt :: FlowFacts world -> DenseProof world -> Int -> Maybe (DenseUpdate world)
writeDenseAt _ (DenseProof name size) offset = DenseUpdate name <$> writeDense size offset

appendDenseBy :: FlowFacts world -> DenseProof world -> Natural -> DenseUpdate world
appendDenseBy _ (DenseProof name size) count = DenseUpdate name (appendDense size count)

-- | Prove and apply in one world, then establish a fresh world. No unindexed
-- length or saved update can be transplanted after a mutation or callback.
transitionDense :: (forall world. FlowFacts world -> Either failure (DenseUpdate world)) -> Normalization scope -> Either failure (Normalization scope)
transitionDense prove (Normalization context facts discoveries) = do
  DenseUpdate name size <- prove facts
  let current = flowData facts
      updated =
        current
          { factArrays = M.insert name (DenseArray size) (factArrays current),
            factVariables = S.insert name (factVariables current),
            factNumeric = S.delete name (factNumeric current),
            factConstants = M.delete name (factConstants current)
          }
  pure (initialNormalization context updated discoveries)

flowData :: FlowFacts world -> FactData
flowData (FlowFacts _ facts) = facts

initialNormalization :: NormalizationContext scope -> FactData -> Discoveries -> Normalization scope
initialNormalization context facts discoveries =
  withWorld (\world -> Normalization context (FlowFacts world facts) discoveries)

-- | Old-world evidence cannot escape this continuation into the new facts.
updateNormalization :: (NormalizationContext scope -> NormalizationContext scope) -> (FactData -> FactData) -> (Discoveries -> Discoveries) -> Normalization scope -> Normalization scope
updateNormalization updateContext updateFacts updateDiscoveries (Normalization context facts discoveries) =
  initialNormalization (updateContext context) (updateFacts (flowData facts)) (updateDiscoveries discoveries)

withNormalizationFacts :: Normalization scope -> (forall world. FlowFacts world -> result) -> result
withNormalizationFacts (Normalization _ facts _) consume = consume facts

nConfig :: Normalization scope -> TranslateConfig
nConfig (Normalization context _ _) = contextConfig context

nPositions :: Normalization scope -> M.Map Id (Position, Position)
nPositions (Normalization context _ _) = contextPositions context

nRuntimeOrigin :: Normalization scope -> Text
nRuntimeOrigin (Normalization context _ _) = contextRuntimeOrigin context

nConstants :: Normalization scope -> M.Map Text Text
nConstants (Normalization _ facts _) = factConstants (flowData facts)

nNumeric :: Normalization scope -> S.Set Text
nNumeric (Normalization _ facts _) = factNumeric (flowData facts)

nContinueNumeric :: Normalization scope -> M.Map LoopKey (S.Set Text)
nContinueNumeric (Normalization _ facts _) = factContinueNumeric (flowData facts)

nBreakNumeric :: Normalization scope -> M.Map LoopKey (S.Set Text)
nBreakNumeric (Normalization _ facts _) = factBreakNumeric (flowData facts)

nVariables :: Normalization scope -> S.Set Text
nVariables (Normalization _ facts _) = factVariables (flowData facts)

nFunctions :: Normalization scope -> S.Set Text
nFunctions (Normalization _ facts _) = factFunctions (flowData facts)

nLocalFunctions :: Normalization scope -> S.Set Text
nLocalFunctions (Normalization _ facts _) = factLocalFunctions (flowData facts)

nResolutionFunctions :: Normalization scope -> S.Set Text
nResolutionFunctions (Normalization _ facts _) = factResolutionFunctions (flowData facts)

nDefinitions :: Normalization scope -> M.Map Text DefinitionIdentity
nDefinitions (Normalization _ facts _) = factDefinitions (flowData facts)

nFunctionDependencies :: Normalization scope -> M.Map Text (M.Map Text DefinitionIdentity)
nFunctionDependencies (Normalization _ facts _) = factFunctionDependencies (flowData facts)

nCurrentDependencies :: Normalization scope -> M.Map Text DefinitionIdentity
nCurrentDependencies (Normalization _ facts _) = factCurrentDependencies (flowData facts)

nAllFunctions :: Normalization scope -> S.Set Text
nAllFunctions (Normalization _ _ discoveries) = discoveredAllFunctions discoveries

nControl :: Normalization scope -> Control scope
nControl (Normalization context _ _) = contextControl context

nFunction :: Normalization scope -> Maybe Text
nFunction = Control.activeFunction . nControl

-- Mint the owned target and clear only the new depth's edge facts together.
enterNormalizationLoop :: LoopTarget scope -> Normalization scope -> Normalization scope
enterNormalizationLoop target =
  updateNormalization
    (\context -> context {contextDirect = False})
    (\facts -> facts {factBreakNumeric = M.delete (Control.loopKey target) (factBreakNumeric facts), factContinueNumeric = M.delete (Control.loopKey target) (factContinueNumeric facts)})
    id

-- Rebase only the lexical context. Mutable facts retain their opaque world
-- until the next transition, independently of the new control scope.
rebaseNormalization :: Control fresh -> Normalization old -> Normalization fresh
rebaseNormalization control (Normalization context facts discoveries) =
  Normalization (context {contextControl = control}) facts discoveries

nLocals :: Normalization scope -> S.Set Text
nLocals (Normalization _ facts _) = factLocals (flowData facts)

nDirect :: Normalization scope -> Bool
nDirect (Normalization context _ _) = contextDirect context

nReserved :: Normalization scope -> S.Set Text
nReserved (Normalization _ _ discoveries) = discoveredReserved discoveries

nDocument :: Normalization scope -> Maybe Text
nDocument (Normalization context _ _) = contextDocument context

nSourceStack :: Normalization scope -> [Text]
nSourceStack (Normalization context _ _) = contextSourceStack context

nResolutionStable :: Normalization scope -> Bool
nResolutionStable (Normalization _ facts _) = factResolutionStable (flowData facts)

nCommandLine :: Normalization scope -> Int
nCommandLine (Normalization context _ _) = contextCommandLine context

nFunctionBodies :: Normalization scope -> M.Map Text (P.ScopedBody Control.FunctionRootKind)
nFunctionBodies (Normalization _ facts _) = factFunctionBodies (flowData facts)

nSourceReturns :: Normalization scope -> S.Set P.SourceEntryContext
nSourceReturns (Normalization _ facts _) = factSourceReturns (flowData facts)

nDescriptors :: Normalization scope -> S.Set Int
nDescriptors (Normalization context _ _) = contextDescriptors context

nWritableDescriptors :: Normalization scope -> S.Set Int
nWritableDescriptors (Normalization context _ _) = contextWritableDescriptors context

nDirectoryFacts :: Normalization scope -> Directory.DirectoryFacts
nDirectoryFacts (Normalization _ facts _) = factDirectoryFacts (flowData facts)

nDirectoryOutcomes :: Normalization scope -> Maybe (Directory.DirectoryFacts, Directory.DirectoryFacts)
nDirectoryOutcomes (Normalization _ facts _) = factDirectoryOutcomes (flowData facts)

nEvaluatedPrograms :: Normalization scope -> [Text]
nEvaluatedPrograms (Normalization _ _ discoveries) = discoveredEvaluatedPrograms discoveries

nArrays :: Normalization scope -> M.Map Text ArrayShape
nArrays (Normalization _ facts _) = factArrays (flowData facts)

nErrTrapWrites :: Normalization scope -> S.Set Text
nErrTrapWrites (Normalization _ facts _) = factErrTrapWrites (flowData facts)

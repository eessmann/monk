-- | Finite flow joins, exit facts and callback invalidation. These operations
-- consume normalized facts; they never walk source syntax.
module Language.Bash.Plan.Normalize.Flow
  ( joinStates,
    loopExit,
    joinContinueNumeric,
    invalidateTrapWrites,
    entryContext,
    joinSourceExit,
  )
where

import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Language.Bash.Plan qualified as P
import Language.Bash.Plan.Control qualified as Control
import Language.Bash.Plan.Directory qualified as Directory
import Language.Bash.Plan.Facts (ArrayShape (..), joinArrayShapes)
import Language.Bash.Plan.Normalize.State

joinStates :: Normalization before -> Normalization after -> Normalization before
joinStates before after =
  updateNormalization id (\currentFacts -> currentFacts {factConstants = M.mergeWithKey (\_ a b -> if a == b then Just a else Nothing) (const mempty) (const mempty) (nConstants before) (nConstants after), factArrays = joinArrayShapes (nArrays before) (nArrays after), factErrTrapWrites = nErrTrapWrites before <> nErrTrapWrites after, factVariables = nVariables before `S.intersection` nVariables after, factNumeric = nNumeric before `S.intersection` nNumeric after, factContinueNumeric = M.unionWith S.intersection (nContinueNumeric before) (nContinueNumeric after), factBreakNumeric = M.unionWith S.intersection (nBreakNumeric before) (nBreakNumeric after), factResolutionStable = nResolutionStable before && nResolutionStable after, factDirectoryFacts = Directory.joinDirectoryFacts (nDirectoryFacts before) (nDirectoryFacts after), factDirectoryOutcomes = Nothing, factResolutionFunctions = nResolutionFunctions before <> nResolutionFunctions after, factCurrentDependencies = nCurrentDependencies before <> nCurrentDependencies after, factSourceReturns = nSourceReturns before <> nSourceReturns after}) (\currentDiscoveries -> currentDiscoveries {discoveredReserved = nReserved before <> nReserved after}) before

-- Break exits bypass trailing writes; no fallthrough constant is an exit proof.
loopExit :: Normalization before -> Normalization after -> Normalization before
loopExit before after =
  let joined = joinStates before after
   in updateNormalization id (\currentFacts -> currentFacts {factConstants = mempty, factNumeric = nNumeric joined `S.intersection` maybe (nNumeric joined) (\target -> M.findWithDefault (nNumeric joined) (Control.loopKey target) (nBreakNumeric after)) (Control.loopTarget (nControl after)), factContinueNumeric = nContinueNumeric before, factBreakNumeric = nBreakNumeric before}) id joined

-- Continue edges bypass subsequent statements. Intersect their finite numeric
-- facts with ordinary fallthrough before admitting the owned backedge.
joinContinueNumeric :: Normalization scope -> Normalization scope
joinContinueNumeric flow = updateNormalization id (\currentFacts -> currentFacts {factNumeric = nNumeric flow `S.intersection` maybe (nNumeric flow) (\target -> M.findWithDefault (nNumeric flow) (Control.loopKey target) (nContinueNumeric flow)) (Control.loopTarget (nControl flow))}) id flow

-- ERR callbacks can change bindings after a failing command. Keeping the
-- possible write set monotonic is conservative across replacement and calls.
invalidateTrapWrites :: Normalization scope -> Normalization scope
invalidateTrapWrites flow =
  let names = nErrTrapWrites flow
   in updateNormalization id (\currentFacts -> currentFacts {factConstants = M.withoutKeys (nConstants flow) names, factNumeric = nNumeric flow S.\\ names, factArrays = M.mapWithKey (\name size -> if S.member name names then UnknownArray else size) (nArrays flow)}) id flow

-- Return edges carry only finite semantic facts; no executable syntax or
-- continuation is retained. Source completion joins these with fallthrough.
entryContext :: Normalization scope -> P.SourceEntryContext
entryContext flow = P.SourceEntryContext (nDefinitions flow) (nVariables flow) (nConstants flow) (nArrays flow) (nLocals flow) (nNumeric flow) (nDirectoryFacts flow)

joinSourceExit :: Normalization scope -> P.SourceEntryContext -> Normalization scope
joinSourceExit flow facts =
  let shared :: (Eq a) => M.Map Text a -> M.Map Text a -> M.Map Text a
      shared = M.mergeWithKey (\_ a b -> if a == b then Just a else Nothing) (const mempty) (const mempty)
      definitions = shared (nDefinitions flow) (P.sourceEntryDefinitions facts)
      names = M.keysSet definitions
   in updateNormalization id (\currentFacts -> currentFacts {factDefinitions = definitions, factFunctions = nFunctions flow `S.intersection` names, factLocalFunctions = nLocalFunctions flow `S.intersection` names, factFunctionBodies = M.restrictKeys (nFunctionBodies flow) names, factConstants = shared (nConstants flow) (P.sourceEntryConstants facts), factArrays = joinArrayShapes (nArrays flow) (P.sourceEntryArrays facts), factVariables = nVariables flow `S.intersection` P.sourceEntryVariables facts, factLocals = nLocals flow `S.intersection` P.sourceEntryLocals facts, factNumeric = nNumeric flow `S.intersection` P.sourceEntryNumericVariables facts, factDirectoryFacts = Directory.joinDirectoryFacts (nDirectoryFacts flow) (P.sourceEntryDirectoryFacts facts), factDirectoryOutcomes = Nothing}) id flow

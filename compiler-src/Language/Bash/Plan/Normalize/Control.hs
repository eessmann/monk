{-# LANGUAGE RankNTypes #-}

-- | Normalization constructs semantic exits only with a witness retained in
-- the resulting same-scope node. The materializer consumes that witness again.
module Language.Bash.Plan.Normalize.Control
  ( module Control,
    breakStatement,
    continueStatement,
    returnStatement,
    shiftStatement,
    setArgumentsStatement,
    ControlCallbacks (..),
    normalizeLoop,
    normalizeFunction,
    normalizeFunctionWith,
    checkDirectoryLoop,
    varyingSource,
  )
where

import Control.Monad.State.Strict (MonadState (get), gets)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Language.Bash.Plan qualified as P
import Language.Bash.Plan.Control as Control
import Language.Bash.Plan.Facts (ArrayShape (UnknownArray))
import Language.Bash.Plan.Identity
  ( DefinitionIdentity (SourceDefinition),
    occurrenceId,
    sourceId,
  )
import Language.Bash.Plan.Normalize.Context
import Language.Bash.Plan.Normalize.Flow
import Language.Bash.Plan.Normalize.State
import Monk.Source.Location (SourcePos (..), SourceRange (..))
import Monk.Translation.Types
import ShellCheck.AST
import Prelude hiding (get, gets, identity, local, put)

breakStatement :: LoopTarget scope -> P.StatementNode scope
breakStatement = P.Break

continueStatement :: LoopTarget scope -> P.StatementNode scope
continueStatement = P.Continue

returnStatement :: ReturnTarget scope -> Maybe P.Scalar -> P.StatementNode scope
returnStatement = P.Return

shiftStatement :: ShiftTarget scope -> Int -> P.StatementNode scope
shiftStatement = P.ShiftArguments

setArgumentsStatement :: SetArgumentsTarget scope -> [P.Word] -> P.StatementNode scope
setArgumentsStatement = P.SetArguments

-- | Recursive syntax stays in one walker; scoped flow owns its transitions.
data ControlCallbacks = ControlCallbacks
  { controlStatement :: forall scope. Token -> Normalize scope (P.Statement scope),
    controlStatements :: forall scope. [Token] -> Normalize scope [P.Statement scope],
    controlRedirected :: forall scope. Token -> [Token] -> Token -> Normalize scope (P.StatementNode scope)
  }

normalizeLoop :: ControlCallbacks -> Token -> Bool -> [Token] -> [Token] -> Normalize scope (P.StatementNode scope)
normalizeLoop callbacks token inverted condition body = scopedLoop $ \target before -> do
  modify' (updateNormalization id (\facts -> facts {factConstants = mempty}) id)
  conditionValue <- traverse (controlStatement callbacks) condition
  bodyValue <- traverse (controlStatement callbacks) body
  modify' joinContinueNumeric
  after <- get
  unless
    (nNumeric before `S.isSubsetOf` nNumeric after)
    (reject token "loop-numeric-flow" "Loop body invalidates a numeric fact required by the next iteration")
  checkDirectoryLoop token before after (any varyingSource (conditionValue <> bodyValue))
  let owned = P.whileBody (Control.rootWitness (nControl after)) target conditionValue bodyValue
  pure (P.WhileLoop inverted owned, loopExit before after)

-- A relative dependency cannot freeze the first iteration's cwd when a
-- directory transition invalidates that fact on the loop backedge.
checkDirectoryLoop :: Token -> Normalization before -> Normalization after -> Bool -> Normalize scope ()
checkDirectoryLoop token before after varying = do
  unless (nArrays before == nArrays after) (reject token "array-loop-shape" "Array shape must remain invariant across loop backedges")
  when
    (nDirectoryFacts before /= nDirectoryFacts after && varying)
    (reject token "source-directory-loop" "A relative source in a directory-changing loop requires an invariant absolute execution cwd")

varyingSource :: P.Statement scope -> Bool
varyingSource (P.Statement _ node) = case node of
  P.SourceBody request nestedBody ->
    (not (T.isPrefixOf "/" (P.sourceRequestTarget request)) && not (maybe False (T.isPrefixOf "/") (P.sourceRequestWorkingDirectory request))) || P.withScopedBody nestedBody (\_ -> any varyingSource)
  P.Sequence values -> any varyingSource values
  P.AssignmentCommand _ values -> any varyingSource values
  P.Redirected _ value -> varyingSource value
  P.And left right -> any varyingSource [left, right]
  P.Or left right -> any varyingSource [left, right]
  P.Negate value -> varyingSource value
  P.Conditional condition yes no -> any varyingSource (condition <> yes <> no)
  P.WhileLoop _ body -> P.withWhileBody body (\_ _ condition values -> any varyingSource (condition <> values))
  P.ForLoop _ _ _ body -> P.withForBody body (\_ _ -> any varyingSource)
  P.ArithmeticFor initial body -> varyingSource initial || P.withArithmeticBody body (\_ _ predicate increment values -> any varyingSource (predicate : increment : values))
  P.Case _ arms -> any (\(P.CaseArm _ values _) -> any varyingSource values) arms
  P.Approximate _ values -> any varyingSource values
  _ -> False

normalizeFunction :: ControlCallbacks -> Token -> Text -> Token -> Normalize scope (P.StatementNode scope)
normalizeFunction callbacks = normalizeFunctionWith callbacks []

normalizeFunctionWith :: ControlCallbacks -> [Token] -> Token -> Text -> Token -> Normalize scope (P.StatementNode scope)
normalizeFunctionWith callbacks redirects token name body = do
  checkedName token name
  checkedFunctionCommand token name
  cfg <- gets nConfig
  when
    (entryMode cfg == Sourceable && not (S.member name (callerExportedFunctions (callerContract cfg))))
    (reject token "undeclared-function-export" "Sourceable function definitions must be declared caller exports")
  direct <- gets nDirect
  active <- gets nFunction
  unless (direct && isNothing active) (reject token "function-context" "Conditional or nested function definitions need call-time binding analysis")
  stack <- gets nSourceStack
  location <- tokenRange token
  let Id definitionId = getId token
      source = sourceId (maybe (fromMaybe "<input>" (viaNonEmpty last stack)) (srcFile . rangeStart) location)
  occurrence <- maybe (reject token "definition-occurrence" "Function definition requires a valid parser occurrence identity") pure (occurrenceId definitionId)
  let identity = SourceDefinition source occurrence
  modify' (\s -> updateNormalization id (\currentFacts -> currentFacts {factFunctions = S.insert name (nFunctions s), factLocalFunctions = S.insert name (nLocalFunctions s), factDefinitions = M.insert name identity (nDefinitions s)}) id s)
  scopedControl (Control.withFunctionControl name) $ \bodyRoot before -> do
    modify' (\s -> updateNormalization (\currentContext -> currentContext {contextDirect = True}) (\currentFacts -> currentFacts {factConstants = mempty, factNumeric = mempty, factArrays = M.map (const UnknownArray) (nArrays s), factVariables = if entryMode cfg == Sourceable then initializedImports (callerContract cfg) else nVariables s, factLocals = mempty, factContinueNumeric = mempty, factBreakNumeric = mempty, factResolutionStable = True, factCurrentDependencies = mempty}) id s)
    bodyValue <-
      if null redirects
        then case body of
          T_BraceGroup _ statements -> controlStatements callbacks statements
          T_Redirecting _ [] (T_BraceGroup _ statements) -> controlStatements callbacks statements
          _ -> (: []) <$> controlStatement callbacks body
        else do
          range <- runtimeTokenRange token
          redirected <- controlRedirected callbacks token redirects body
          pure [P.Statement range redirected]
    after <- get
    let owned = P.scopedBody bodyRoot bodyValue
        restored = updateNormalization id (\currentFacts -> currentFacts {factResolutionFunctions = (if nResolutionStable after && nDirectoryFacts before == nDirectoryFacts after then S.delete else S.insert) name (nResolutionFunctions before), factFunctionDependencies = M.insert name (nCurrentDependencies after) (nFunctionDependencies before), factFunctionBodies = M.insert name owned (nFunctionBodies before), factArrays = M.union (nArrays before) (M.map (const UnknownArray) (nArrays after)), factErrTrapWrites = nErrTrapWrites before <> nErrTrapWrites after}) (\currentDiscoveries -> currentDiscoveries {discoveredReserved = nReserved before <> nReserved after}) before
    pure (P.DefineFunction name owned, restored)

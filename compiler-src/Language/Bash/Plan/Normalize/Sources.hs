{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Bash.Plan.Normalize.Sources
  ( SourceCallbacks (..),
    normalizeSourceCall,
  )
where

import Control.Monad.State.Strict (MonadState (get))
import Data.Text qualified as T
import Language.Bash.Plan qualified as P
import Language.Bash.Plan.Directory qualified as Directory
import Language.Bash.Plan.Normalize.Context
import Language.Bash.Plan.Normalize.Control qualified as Control
import Language.Bash.Plan.Normalize.Flow
import Language.Bash.Plan.Normalize.State
import Language.Bash.Plan.Normalize.Syntax
import ShellCheck.AST
import ShellCheck.ASTLib (getLiteralString)
import ShellCheck.Interface
  ( ParseResult (prRoot, prTokenPositions),
  )
import Prelude hiding (get, gets, identity, local, put)

data SourceCallbacks = SourceCallbacks
  { sourceWords :: forall scope. [Token] -> Normalize scope [P.Word],
    sourceStatement :: forall scope. Token -> Normalize scope (P.Statement scope)
  }

normalizeSourceCall :: SourceCallbacks -> Token -> [Token] -> Normalize scope (P.StatementNode scope)
normalizeSourceCall callbacks token arguments = case arguments of
  pathToken : argv -> do
    literal <- maybe (reject token "computed-source" "Source requires a literal immutable dependency") (pure . toText) (getLiteralString pathToken)
    when (T.null literal) (reject token "source-target" "Source requires a nonempty target")
    before <- get
    unless (nResolutionStable before || T.isPrefixOf "/" literal) (reject token "source-resolution-state" "Source resolution follows an unsupported environment mutation")
    unless
      (isNothing (nFunction before) || T.isPrefixOf "/" literal)
      (reject token "source-function-context" "A function source requires an absolute immutable target independent of invocation cwd")
    when
      (Directory.directoryLocation (nDirectoryFacts before) == Directory.UnknownDirectory && not (T.isPrefixOf "/" literal))
      (reject token "source-directory-state" "Relative source resolution requires a known execution cwd on this control edge")
    argvValue <- sourceWords callbacks argv
    range <- tokenRange token
    let Id ordinal = getId token
        entry = entryContext before
        request = P.SourceRequest ordinal range literal argvValue P.SharedSource (nSourceStack before) entry (case Directory.directoryLocation (nDirectoryFacts before) of Directory.KnownDirectory path -> Just path; Directory.RelativeDirectory path -> Just path; _ -> Nothing)
    SourceDocument text parsed origin <- lift (NormalizationNeedsSource request NormalizationComplete)
    let name = documentName parsed
    when (name `elem` nSourceStack before) (reject token "source-cycle" "Recursive source cycles are outside the initial envelope")
    root <- maybe (reject token "source-parse" "Dependency has no parse root") pure (prRoot parsed)
    scopedControl (Control.withSourceControl (if any P.guaranteesField argvValue then Control.OwnSourceArguments else Control.BorrowCallerArguments)) $ \bodyRoot _ -> do
      modify'
        ( \s ->
            updateNormalization (\currentContext -> currentContext {contextDocument = Just text, contextPositions = prTokenPositions parsed, contextRuntimeOrigin = origin, contextSourceStack = nSourceStack before <> [name]}) (\currentFacts -> currentFacts {factSourceReturns = mempty}) (\currentDiscoveries -> currentDiscoveries {discoveredAllFunctions = nAllFunctions s <> functionNames root, discoveredReserved = nReserved s <> sourceNames root}) s
        )
      body <- sourceStatement callbacks root
      after <- get
      let joined = foldl' joinSourceExit after (nSourceReturns after)
          restored =
            updateNormalization
              (\context -> context {contextDocument = nDocument before, contextPositions = nPositions before, contextRuntimeOrigin = nRuntimeOrigin before, contextSourceStack = nSourceStack before})
              (\facts -> facts {factSourceReturns = nSourceReturns before})
              id
              (rebaseNormalization (nControl before) joined)
          owned = P.scopedBody bodyRoot [body]
      pure (P.SourceBody request owned, restored)
  [] -> reject token "source-target" "Source requires a target operand"

{-# LANGUAGE RankNTypes #-}

module Unit.NormalizationFacts (unitNormalizationFactTests) where

import Data.Map.Strict qualified as M
import Language.Bash.Plan qualified as P
import Language.Bash.Plan.Directory qualified as Directory
import Language.Bash.Plan.Facts (ArrayShape (..), DenseLength (..))
import Language.Bash.Plan.Identity
import Language.Bash.Plan.Normalize.Control qualified as Control
import Language.Bash.Plan.Normalize.State
import Monk.Translation.Types (EntryMode (..), strictConfig)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as H

unitNormalizationFactTests :: TestTree
unitNormalizationFactTests =
  testGroup
    "Owned normalization facts"
    [ H.testCase "source, imported and absent identities are disjoint" $ do
        occurrenceId (-1) H.@?= Nothing
        case occurrenceId 0 of
          Nothing -> H.assertFailure "zero is a valid parser occurrence"
          Just occurrence -> do
            let sourceDefinition = SourceDefinition (sourceId "<import>") occurrence
            H.assertBool "a source spelling cannot forge an import" (sourceDefinition /= ImportedDefinition (importedOccurrence 0))
            H.assertBool "absence is a separate definition kind" (sourceDefinition /= AbsentDefinition)
            H.assertBool "source document participates in definition identity" (sourceDefinition /= SourceDefinition (sourceId "other") occurrence),
      H.testCase "dense lookup uses current map and preserves valid append bounds" $ do
        withArrays (M.singleton "values" (DenseArray (KnownLength 2))) $ \flow -> do
          denseWrite flow "values" 2 H.@?= Just (KnownLength 3)
          denseWrite flow "values" 3 H.@?= Nothing
          denseWrite flow "missing" 0 H.@?= Nothing,
      H.testCase "invalidation cannot reprove a carried old array shape" $ do
        withArrays (M.singleton "values" (DenseArray (KnownLength 2))) $ \before -> do
          let after = updateNormalization id (\facts -> facts {factArrays = M.singleton "values" UnknownArray}) id before
          denseWrite before "values" 0 H.@?= Just (KnownLength 2)
          denseWrite after "values" 0 H.@?= Nothing,
      H.testCase "control scope permissions preserve function and source ownership" $ do
        forM_ [Standalone, Sourceable] $ \mode -> Control.withEntryControl mode $ \root -> do
          H.assertBool "entry return ownership" (isJust (Control.returnTarget root) == (mode == Sourceable))
          H.assertBool "entry shift ownership" (isJust (Control.shiftTarget root) == (mode == Standalone))
          H.assertBool "entry set ownership" (isJust (Control.setArgumentsTarget root) == (mode == Standalone))
          checkFunctionScope mode root
          Control.withFunctionControl "f" root (\_ -> checkFunctionScope mode),
      H.testCase "owned loop targets do not cross control roots" $
        Control.withEntryControl Standalone $ \root -> do
          H.assertBool "entry has no loop" (isNothing (Control.loopTarget root))
          Control.withLoopControl root $ \outer outerScope ->
            Control.withLoopControl outerScope $ \inner innerScope -> do
              H.assertBool "nested finite keys are distinct" (Control.loopKey outer /= Control.loopKey inner)
              Control.loopTarget innerScope H.@?= Just inner
              Control.consumeLoop (Control.rootWitness innerScope) inner H.@?= Control.loopKey inner
              Control.withFunctionControl "f" innerScope (const checkCleared)
              Control.withSourceControl Control.OwnSourceArguments innerScope (const checkCleared)
              Control.withChildControl innerScope (const checkCleared)
              Control.withHandlerControl innerScope (const checkCleared),
      H.testCase "existential bodies compare structure without comparing scope names" $ do
        let body literal = Control.withEntryControl Standalone $ \root ->
              Control.withLoopControl root $ \target loop -> P.forBody (Control.rootWitness loop) target [P.Statement Nothing (P.Invoke (P.Builtin "echo") [P.OneField (P.Literal literal)]), P.Statement Nothing (P.Break target)]
        body "a" H.@?= body "a"
        H.assertBool "semantic payload participates in equality" (body "a" /= body "b"),
      H.testCase "runtime-sized dense arrays permit only index-zero replacement" $ do
        withArrays (M.singleton "values" (DenseArray DynamicLength)) $ \flow -> do
          denseWrite flow "values" 0 H.@?= Just DynamicLength
          denseWrite flow "values" 1 H.@?= Nothing
    ]

denseWrite :: Normalization scope -> Text -> Int -> Maybe DenseLength
denseWrite flow name index = do
  updated <-
    either (const Nothing) Just $
      transitionDense
        ( \facts -> do
            proof <- maybe (Left ()) Right (lookupDense facts name)
            maybe (Left ()) Right (writeDenseAt facts proof index)
        )
        flow
  case M.lookup name (nArrays updated) of
    Just (DenseArray size) -> Just size
    _ -> Nothing

withArrays :: Map Text ArrayShape -> (forall scope. Normalization scope -> result) -> result
withArrays arrays consume = Control.withEntryControl Standalone $ \control -> consume (initialNormalization (context control) facts (Discoveries mempty mempty []))
  where
    context control =
      NormalizationContext
        { contextConfig = strictConfig,
          contextPositions = mempty,
          contextRuntimeOrigin = "test",
          contextControl = control,
          contextDirect = True,
          contextDocument = Nothing,
          contextSourceStack = [],
          contextCommandLine = 1,
          contextDescriptors = mempty,
          contextWritableDescriptors = mempty
        }
    facts =
      FactData
        { factConstants = mempty,
          factNumeric = mempty,
          factContinueNumeric = mempty,
          factBreakNumeric = mempty,
          factVariables = mempty,
          factFunctions = mempty,
          factLocalFunctions = mempty,
          factResolutionFunctions = mempty,
          factDefinitions = mempty,
          factFunctionDependencies = mempty,
          factCurrentDependencies = mempty,
          factLocals = mempty,
          factResolutionStable = True,
          factFunctionBodies = mempty,
          factSourceReturns = mempty,
          factDirectoryFacts = Directory.MkDirectoryFacts Directory.InitialDirectory False,
          factDirectoryOutcomes = Nothing,
          factArrays = arrays,
          factErrTrapWrites = mempty
        }

checkCleared :: Control.Control scope -> H.Assertion
checkCleared control = H.assertBool "new control root clears inherited loop target" (isNothing (Control.loopTarget control))

checkFunctionScope :: EntryMode -> Control.Control scope -> H.Assertion
checkFunctionScope mode functionScope = do
  H.assertBool "function shift ownership" (isJust (Control.shiftTarget functionScope) == (mode == Standalone || isJust (Control.activeFunction functionScope)))
  forM_ [Control.BorrowCallerArguments, Control.OwnSourceArguments] $ \arguments ->
    Control.withSourceControl arguments functionScope $ \_ sourced -> do
      H.assertBool "source return target" (maybe False Control.returnsFromSource (Control.returnTarget sourced))
      H.assertBool "source argv depends on effective arguments" (isJust (Control.shiftTarget sourced) == (arguments == Control.OwnSourceArguments))
      H.assertBool "source cannot replace argv" (isNothing (Control.setArgumentsTarget sourced))
      Control.withChildControl sourced $ \_ child -> H.assertBool "child retains admitted source argv permissions" (isJust (Control.shiftTarget child) == isJust (Control.shiftTarget sourced))

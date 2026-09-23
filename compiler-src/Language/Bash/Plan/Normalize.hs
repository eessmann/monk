{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Language.Bash.Plan.Normalize
  ( normalizeSource,
    normalizeDocument,
    beginNormalization,
    beginNormalizationWithOrigin,
    NormalizationResult (..),
    SourceDocument (..),
  )
where

import Control.Monad.State.Strict (get, gets, put)
import Data.Char (isDigit)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Language.Bash.Arithmetic.Plan qualified as A
import Language.Bash.Arithmetic.Source qualified as ArithmeticSource
import Language.Bash.Parser (parseBashFragment)
import Language.Bash.Plan qualified as P
import Language.Bash.Plan.Directory qualified as Directory
import Language.Bash.Plan.Effects
  ( admitPipeline,
    closeChildRegion,
    mayWriteBuiltin,
  )
import Language.Bash.Plan.Effects qualified as Effects
import Language.Bash.Plan.Facts (ArrayShape (..), DenseLength (..))
import Language.Bash.Plan.Identity
  ( DefinitionIdentity (AbsentDefinition, ImportedDefinition),
    importedOccurrence,
  )
import Language.Bash.Plan.Normalize.Commands
  ( freezeComparison,
    safeBuiltins,
    unsupportedBuiltins,
    validatePrintf,
  )
import Language.Bash.Plan.Normalize.Commands qualified as Commands
import Language.Bash.Plan.Normalize.Context
import Language.Bash.Plan.Normalize.Control qualified as Control
import Language.Bash.Plan.Normalize.Flow
import Language.Bash.Plan.Normalize.Literal
import Language.Bash.Plan.Normalize.Sources qualified as Sources
import Language.Bash.Plan.Normalize.State
import Language.Bash.Plan.Normalize.Syntax
import Language.Bash.Plan.Normalize.Words (processToken)
import Language.Bash.Plan.Normalize.Words qualified as Words
import Language.Bash.Plan.Operator qualified as Operator
import Monk.Compiler.Context
  ( Context,
    Phase (Parsed),
    withContext,
  )
import Monk.Compiler.Context qualified as Compilation
import Monk.Runtime.Integer qualified as Integer
import Monk.Source.Location (SourcePos (..), SourceRange (..))
import Monk.Translation.Contract (validateCallerContract)
import Monk.Translation.Types
import ShellCheck.AST
import ShellCheck.ASTLib (getLiteralString)
import ShellCheck.Interface (ParseResult (..), Position (..))
import Prelude hiding (get, gets, identity, local, put)

normalizeSource :: TranslateConfig -> ParseResult -> Either (NonEmpty Diagnostic) P.SourcePlan
normalizeSource cfg = withoutSources . startNormalization cfg Nothing Nothing

normalizeDocument :: TranslateConfig -> Text -> ParseResult -> Either (NonEmpty Diagnostic) P.SourcePlan
normalizeDocument cfg document = withoutSources . startNormalization cfg (Just document) Nothing

beginNormalization :: TranslateConfig -> Text -> ParseResult -> NormalizationResult P.SourcePlan
beginNormalization cfg document = startNormalization cfg (Just document) Nothing

beginNormalizationWithOrigin :: TranslateConfig -> Text -> Text -> ParseResult -> NormalizationResult P.SourcePlan
beginNormalizationWithOrigin cfg origin document = startNormalization cfg (Just document) (Just origin)

withoutSources :: NormalizationResult a -> Either (NonEmpty Diagnostic) a
withoutSources = \case
  NormalizationFailed errors -> Left errors
  NormalizationComplete value -> Right value
  NormalizationNeedsSource request _ -> Left (diagnostic (P.sourceRequestRange request) "source-environment" "Literal source requires an explicit graph environment" :| [])

startNormalization :: TranslateConfig -> Maybe Text -> Maybe Text -> ParseResult -> NormalizationResult P.SourcePlan
startNormalization cfg document origin parsed =
  withContext cfg $ \context -> normalizeParsed (ParsedInput context document origin parsed)

-- The parsed input and the source-resumption continuation retain one owner.
data ParsedInput owner target entry provider (phase :: Phase) where
  ParsedInput :: Context owner target entry provider -> Maybe Text -> Maybe Text -> ParseResult -> ParsedInput owner target entry provider Parsed

normalizeParsed :: ParsedInput owner target entry provider Parsed -> NormalizationResult P.SourcePlan
normalizeParsed (ParsedInput compilation document origin parsed) = Control.withContextControl compilation $ \entryRoot control -> do
  let cfg = Compilation.contextConfig compilation
  when
    (entryMode cfg == Standalone && callerContract cfg /= emptyCallerContract)
    (NormalizationFailed (diagnostic Nothing "caller-contract-mode" "Standalone execution cannot carry a caller contract" :| []))
  either
    (\message -> NormalizationFailed (diagnostic Nothing "caller-contract" message :| []))
    pure
    (validateCallerContract (callerContract cfg))
  root <- maybe (NormalizationFailed (diagnostic Nothing "parse-root" "Missing Bash parse root" :| [])) pure (prRoot parsed)
  let imports = callerContract cfg
      initial =
        initialNormalization
          NormalizationContext
            { contextConfig = cfg,
              contextPositions = prTokenPositions parsed,
              contextRuntimeOrigin = fromMaybe (documentName parsed) origin,
              contextControl = control,
              contextDirect = True,
              contextDocument = document,
              contextSourceStack = [documentName parsed],
              contextCommandLine = 1,
              contextDescriptors = S.fromList [0, 1, 2],
              contextWritableDescriptors = S.fromList [1, 2]
            }
          FactData
            { factConstants = M.singleton "IFS" " \t\n",
              factNumeric = mempty,
              factContinueNumeric = mempty,
              factBreakNumeric = mempty,
              factVariables = initializedImports imports,
              factFunctions = M.keysSet (callerFunctions imports),
              factLocalFunctions = mempty,
              factResolutionFunctions = mempty,
              factDefinitions = M.fromList [(name, ImportedDefinition (importedOccurrence index)) | (index, name) <- zip [0 ..] (M.keys (callerFunctions imports))],
              factFunctionDependencies = mempty,
              factCurrentDependencies = mempty,
              factLocals = mempty,
              factResolutionStable = True,
              factFunctionBodies = mempty,
              factSourceReturns = mempty,
              factDirectoryFacts = Directory.MkDirectoryFacts Directory.InitialDirectory False,
              factDirectoryOutcomes = Nothing,
              factArrays = mempty,
              factErrTrapWrites = mempty
            }
          Discoveries
            { discoveredAllFunctions = functionNames root,
              discoveredReserved = sourceNames root <> contractNames imports,
              discoveredEvaluatedPrograms = []
            }
  when
    (entryMode cfg == Sourceable && callerAmbientEffects imports /= NoRelevantAmbientEffects)
    (NormalizationFailed (diagnostic Nothing "caller-effects" "Sourceable execution requires explicit no-relevant-ambient-effects obligations" :| []))
  (statement, final) <- runStateT (normalizeStatement root) initial
  pure (P.ownSourcePlan (P.sealSourcePlan compilation (P.entryBody entryRoot [statement]) (nReserved final)))

normalizeStatement :: Token -> Normalize scope (P.Statement scope)
normalizeStatement token = do
  range <- runtimeTokenRange token
  modify' (updateNormalization (\currentContext -> currentContext {contextCommandLine = maybe 1 (srcLine . rangeStart) range}) (\currentFacts -> currentFacts {factDirectoryOutcomes = Nothing}) id)
  node <- case token of
    T_Script _ _ body -> P.Sequence <$> normalizeStatements body
    T_BraceGroup _ body -> P.Sequence <$> normalizeStatements body
    T_Redirecting _ [] body -> statementNode <$> normalizeStatement body
    T_Redirecting _ redirects body -> normalizeRedirected token redirects body
    T_Annotation _ _ body -> statementNode <$> normalizeStatement body
    T_Pipeline _ [] [body] -> statementNode <$> normalizeStatement body
    T_Pipeline _ pipes body
      | all ordinaryPipe pipes,
        length body > 1 -> do
          childrenValue <- traverse (normalizeChild token . (: [])) body
          stages <- maybe (reject token "empty-pipeline" "A pipeline needs stages") pure (NE.nonEmpty childrenValue)
          case admitPipeline stages of
            Right () -> pure (P.Pipeline stages)
            Left message -> do
              mode <- gets (entryMode . nConfig)
              if mode == Standalone then pure (P.SupervisedPipeline stages) else reject token "pipeline-signal-lifetime" message
    T_Subshell _ body -> P.Subshell <$> normalizeChild token body
    T_Backgrounded _ body -> do
      requireSession token
      P.Background <$> normalizeChild token [body]
    T_Banged _ body -> do
      value <- nested body
      modify' (\flow -> updateNormalization id (\currentFacts -> currentFacts {factDirectoryOutcomes = fmap (\(success, failure) -> (failure, success)) (nDirectoryOutcomes flow)}) id flow)
      pure (P.Negate value)
    T_AndIf _ a b -> normalizeAndOr P.And a b
    T_OrIf _ a b -> normalizeAndOr P.Or a b
    T_IfExpression _ branches other -> normalizeIf branches other
    T_WhileExpression _ condition body -> normalizeLoop token False condition body
    T_UntilExpression _ condition body -> normalizeLoop token True condition body
    T_ForArithmetic _ initial predicate increment body -> do
      (initialValue, initialExit) <- arithmeticStatement initial
      scopedLoop $ \target before -> do
        modify' (updateNormalization id (\facts -> facts {factConstants = mempty}) id)
        (predicateValue0, predicateExit) <- arithmeticStatement predicate
        let predicateValue = case (predicate, predicateValue0) of
              (TA_Sequence _ [], P.Statement location (P.ArithmeticCommand site _ bindings)) -> P.Statement location (P.ArithmeticCommand site (A.ArithmeticLiteral 1) bindings)
              _ -> predicateValue0
        bodyValue <- normalizeStatements body
        modify' (updateNormalization id (\facts -> facts {factConstants = mempty}) id . joinContinueNumeric)
        (incrementValue, incrementExit) <- arithmeticStatement increment
        after <- get
        unless (nNumeric before `S.isSubsetOf` nNumeric after) (reject token "loop-numeric-flow" "Arithmetic loop invalidates an integer fact required by the next iteration")
        Control.checkDirectoryLoop token before after (Control.varyingSource initialValue || any Control.varyingSource (predicateValue : incrementValue : bodyValue))
        let exits = foldl' joinStates (foldl' joinStates (loopExit before after) initialExit) (predicateExit <> incrementExit)
            restored = updateNormalization id (\facts -> facts {factContinueNumeric = nContinueNumeric before, factBreakNumeric = nBreakNumeric before}) id exits
            owned = P.arithmeticBody (Control.rootWitness (nControl after)) target predicateValue incrementValue bodyValue
        pure (P.ArithmeticFor initialValue owned, restored)
    T_ForIn _ name values body -> do
      unless (name == "_") (checkedName token (toText name))
      rejectArrayScalar token (toText name)
      storage <- storageFor token False (toText name)
      wordsValue <- normalizeWords values
      let binder = toText name
          numericWords = all (\case P.OneField (P.Literal value) -> numericLiteral value; _ -> False) wordsValue
      scopedLoop $ \target before -> do
        modify' (\flow -> updateNormalization id (\facts -> facts {factVariables = S.insert binder (nVariables flow), factConstants = mempty, factNumeric = (if numericWords then S.insert else S.delete) binder (nNumeric flow)}) id flow)
        bodyValue <- traverse normalizeStatement body
        modify' joinContinueNumeric
        after <- get
        unless (S.delete binder (nNumeric before) `S.isSubsetOf` nNumeric after) (reject token "loop-numeric-flow" "Loop body invalidates a numeric fact required at the next iteration")
        Control.checkDirectoryLoop token before after (any Control.varyingSource bodyValue)
        let owned = P.forBody (Control.rootWitness (nControl after)) target bodyValue
        pure (P.ForLoop storage (toText name) wordsValue owned, loopExit before after)
    T_Function _ _ _ name body -> normalizeFunction token (toText name) body
    T_CaseExpression _ value arms -> do
      scalar <- normalizeScalar value
      before <- get
      put (updateNormalization (\currentContext -> currentContext {contextDirect = False}) id id before)
      armValues <- forM arms $ \(ending, patterns, body) -> do
        entry <- get
        patternsValue <- forM patterns $ \patternToken -> do
          prefix <- get
          patternValue <- normalizePattern patternToken
          afterPattern <- get
          put (joinStates prefix afterPattern)
          pure patternValue
        bodyValue <- traverse normalizeStatement body
        afterBody <- get
        put (joinStates entry afterBody)
        pure (P.CaseArm patternsValue bodyValue (case ending of CaseBreak -> P.StopCase; CaseFallThrough -> P.FallThrough; CaseContinue -> P.Retest))
      modify' (updateNormalization (\currentContext -> currentContext {contextDirect = nDirect before}) (\currentFacts -> currentFacts {factConstants = mempty}) id)
      pure (P.Case scalar armValues)
    T_SimpleCommand _ assignments command -> normalizeCommand token assignments command
    T_Condition _ _ condition -> normalizeCondition condition
    T_Arithmetic {} -> do
      (site, expression, bindings) <- normalizeArithmeticAt token
      pure (P.ArithmeticCommand site expression bindings)
    _ -> reject token "unsupported-syntax" ("No admitted semantics for " <> tokenKind token <> " in statement context")
  modify' invalidateTrapWrites
  pure (P.Statement range node)
  where
    statementNode (P.Statement _ node) = node
    ordinaryPipe (T_Pipe _ "|") = True
    ordinaryPipe _ = False
    arithmeticStatement value = do
      ((site, expression, bindings), (numeric, variables), potentialFailure) <- normalizeArithmeticRegion value
      errorExit <- get
      -- The next loop clause executes only after successful header evaluation.
      modify' (updateNormalization id (\currentFacts -> currentFacts {factNumeric = numeric, factVariables = variables}) id)
      range <- runtimeTokenRange value
      pure (P.Statement range (P.ArithmeticCommand site expression bindings), [errorExit | potentialFailure])

normalizeRedirected :: Token -> [Token] -> Token -> Normalize scope (P.StatementNode scope)
normalizeRedirected _ redirects function@(T_Function _ _ _ name body) = normalizeFunctionWith redirects function (toText name) body
normalizeRedirected parent redirects body = do
  before <- get
  operations <- traverse normalizeRedirect redirects
  writable <- gets nWritableDescriptors
  let fallible = any (\case P.OpenDescriptor {} -> True; _ -> False) operations
  when fallible (modify' (updateNormalization (\currentContext -> currentContext {contextDirect = False}) id id))
  value <- normalizeStatement body
  when fallible $ do
    let effects = redirectedOperandEffects value
    unless (S.null (Effects.effectWrites effects) && not (Effects.effectSubstitution effects)) (reject parent "redirect-assignment-effects" "Effectful builtin or assignment operands need expansion before file opens")
  after <- get
  unless
    (nLocals before == nLocals after)
    (reject parent "redirect-local-scope" "A local declaration inside a redirected compound needs an explicit function-frame lifetime")
  when
    (not (S.member 1 writable) && mayWriteBuiltin (nFunctionBodies before) [value])
    (reject parent "redirect-closed-writer" "A builtin writing a closed stdout needs its original error and owner termination semantics")
  put (updateNormalization (\currentContext -> currentContext {contextDescriptors = nDescriptors before, contextWritableDescriptors = nWritableDescriptors before}) id id (if fallible then joinStates before after else after))
  pure (P.Redirected operations value)

redirectedOperandEffects :: P.Statement scope -> Effects.Effects
redirectedOperandEffects (P.Statement _ node) = case node of
  P.AssignmentCommand _ statements -> foldMap assignmentEffects statements
  P.DeclarationCommand values -> foldMap (\case P.DeclareLocal _ _ value -> foldMap Effects.scalarEffects value; P.DeclareExport _ _ value -> foldMap Effects.scalarEffects value) values
  P.SetArguments _ values -> foldMap Effects.wordEffects values
  P.SourceBody request _ -> foldMap Effects.wordEffects (P.sourceRequestArguments request)
  P.Wait values -> foldMap Effects.wordEffects values
  P.Return _ value -> foldMap Effects.scalarEffects value
  P.Exit value -> foldMap Effects.scalarEffects value
  _ -> mempty
  where
    assignmentEffects (P.Statement _ assignment) = case assignment of
      P.Assign _ _ value -> Effects.scalarEffects value
      P.AssignArray _ _ values -> foldMap Effects.wordEffects values
      P.AppendArray _ _ values -> foldMap Effects.wordEffects values
      P.AssignArrayElement _ _ _ value -> Effects.scalarEffects value
      _ -> mempty

normalizeRedirect :: Token -> Normalize scope P.Redirection
normalizeRedirect token = case token of
  T_FdRedirect _ source operation -> case operation of
    T_IoDuplicate _ operator target -> do
      input <- direction operator
      descriptor <- sourceNumber source input
      if target == "-"
        then modify' (\flow -> updateNormalization (\currentContext -> currentContext {contextDescriptors = S.delete descriptor (nDescriptors flow), contextWritableDescriptors = S.delete descriptor (nWritableDescriptors flow)}) id id flow) >> pure (P.CloseDescriptor descriptor input)
        else do
          targetNumber <- number target
          available <- gets nDescriptors
          unless (S.member targetNumber available) (reject token "redirect-closed-source" "Duplicating a closed descriptor needs owned Bash error semantics")
          modify' (\flow -> updateNormalization (\currentContext -> currentContext {contextDescriptors = S.insert descriptor (nDescriptors flow), contextWritableDescriptors = (if S.member targetNumber (nWritableDescriptors flow) then S.insert else S.delete) descriptor (nWritableDescriptors flow)}) id id flow)
          pure (P.DuplicateDescriptor descriptor targetNumber input)
    T_IoFile _ operator file -> do
      input <- direction operator
      descriptor <- sourceNumber source input
      mode <- case operator of
        T_Less {} -> pure P.ReadFile
        T_Greater {} -> pure P.WriteFile
        T_DGREAT {} -> pure P.AppendFile
        T_CLOBBER {} -> pure P.WriteFile
        T_LESSGREAT {} -> pure P.ReadWriteFile
        _ -> reject token "redirect-mode" "File redirect mode has no owned primitive"
      let writable = mode /= P.ReadFile
      operationValue <-
        if getLiteralString file == Just "/dev/null" && mode /= P.ReadWriteFile
          then pure (P.NullDescriptor descriptor input)
          else do
            requireSession token
            evaluated <- gets nEvaluatedPrograms
            unless (null evaluated) (reject token "eval-file-diagnostic" "Eval file opens need exact nested diagnostic source locations")
            wordsValue <- case processToken file of
              Just endpoint -> (: []) . P.OneField <$> normalizeProcess endpoint
              Nothing -> normalizeWords [file]
            scalar <- case wordsValue of
              [P.OneField value] -> pure value
              _ -> reject token "redirect-cardinality" "File opens require a single proved path field"
            let effects = Effects.scalarEffects scalar
            unless (isJust (processToken file) || (S.null (Effects.effectWrites effects) && not (Effects.effectSubstitution effects))) (reject token "redirect-path-effects" "Effectful paths require pre-redirection command expansion facts")
            pure (P.OpenDescriptor descriptor mode scalar)
      modify' (\flow -> updateNormalization (\currentContext -> currentContext {contextDescriptors = S.insert descriptor (nDescriptors flow), contextWritableDescriptors = (if writable then S.insert else S.delete) descriptor (nWritableDescriptors flow)}) id id flow)
      pure operationValue
    T_HereString _ value -> do
      requireSession token
      descriptor <- sourceNumber source True
      scalar <- normalizeScalar value
      inputValue descriptor scalar True
    T_HereDoc _ dashed _ _ parts -> do
      requireSession token
      descriptor <- sourceNumber source True
      values <- if dashed == Dashed then heredocParts True parts else traverse normalizeScalar parts
      inputValue descriptor (compact values) False
    _ -> reject token "redirect-shape" "No owned semantics for this redirection operation"
  _ -> reject token "redirect-shape" "Expected an explicit descriptor operation"
  where
    direction = \case
      T_Less {} -> pure True
      T_LESSGREAT {} -> pure True
      T_LESSAND {} -> pure True
      T_Greater {} -> pure False
      T_GREATAND {} -> pure False
      T_DGREAT {} -> pure False
      T_CLOBBER {} -> pure False
      _ -> reject token "redirect-mode" "This descriptor mode has no admitted primitive"
    sourceNumber "" input = pure (if input then 0 else 1)
    sourceNumber value _ = number value
    number value = descriptorNumber token (toText value)
    inputValue descriptor scalar newline = do
      let effects = Effects.scalarEffects scalar
      unless (S.null (Effects.effectWrites effects) && not (Effects.effectSubstitution effects)) (reject token "redirect-input-effects" "Effectful input construction requires pre-redirection command expansion facts")
      modify' (\flow -> updateNormalization (\currentContext -> currentContext {contextDescriptors = S.insert descriptor (nDescriptors flow), contextWritableDescriptors = S.delete descriptor (nWritableDescriptors flow)}) id id flow)
      pure (P.InputDescriptor descriptor scalar newline)

-- Tab stripping applies to source bytes before expansion, never to tabs
-- introduced by a parameter value or command substitution.
heredocParts :: Bool -> [Token] -> Normalize scope [P.Scalar]
heredocParts _ [] = pure []
heredocParts atStart (part : rest) = case part of
  T_Literal _ value -> do
    let (next, stripped) = foldl' strip (atStart, []) value
        strip (leading, result) char
          | leading && char == '\t' = (True, result)
          | otherwise = (char == '\n', char : result)
    (P.Literal (toText (reverse stripped)) :) <$> heredocParts next rest
  _ -> do
    scalar <- normalizeScalar part
    (scalar :) <$> heredocParts False rest

normalizeChild :: Token -> [Token] -> Normalize scope P.ChildRegion
normalizeChild token body = do
  range <- runtimeTokenRange token
  scopedControl Control.withChildControl $ \bodyRoot before -> do
    modify' (updateNormalization (\context -> context {contextDirect = True}) (\facts -> facts {factErrTrapWrites = mempty}) id)
    bodyValue <- normalizeStatements body
    after <- get
    let owned = P.scopedBody bodyRoot bodyValue
    child <- either (reject token "child-snapshot") pure (closeChildRegion range (M.union (nFunctionBodies before) (nFunctionBodies after)) owned)
    let restored = updateNormalization id (\facts -> facts {factCurrentDependencies = nCurrentDependencies before <> nCurrentDependencies after}) (\discoveries -> discoveries {discoveredReserved = nReserved before <> nReserved after}) before
    pure (child, restored)

nested :: Token -> Normalize scope (P.Statement scope)
nested token = do
  direct <- gets nDirect
  modify' (updateNormalization (\currentContext -> currentContext {contextDirect = False}) id id)
  value <- normalizeStatement token
  modify' (updateNormalization (\currentContext -> currentContext {contextDirect = direct}) id id)
  pure value

normalizeAndOr :: (P.Statement scope -> P.Statement scope -> P.StatementNode scope) -> Token -> Token -> Normalize scope (P.StatementNode scope)
normalizeAndOr constructor a b = do
  left <- nested a
  before <- get
  let onSuccess = case constructor left left of P.And {} -> True; _ -> False
      select success flow = case nDirectoryOutcomes flow of
        Just (yes, no) -> updateNormalization id (\currentFacts -> currentFacts {factDirectoryFacts = if success then yes else no}) id flow
        Nothing -> flow
  put (select onSuccess before)
  right <- nested b
  after <- get
  put (joinStates (select (not onSuccess) before) after)
  pure (constructor left right)

normalizeIf :: [([Token], [Token])] -> [Token] -> Normalize scope (P.StatementNode scope)
normalizeIf [] other = P.Sequence <$> traverse nested other
normalizeIf ((condition, body) : remaining) other = do
  conditionValue <- traverse nested condition
  before <- get
  forM_ (nDirectoryOutcomes before) $ \(success, _) -> modify' (updateNormalization id (\currentFacts -> currentFacts {factDirectoryFacts = success}) id)
  bodyValue <- traverse nested body
  yes <- get
  put before
  forM_ (nDirectoryOutcomes before) $ \(_, failure) -> modify' (updateNormalization id (\currentFacts -> currentFacts {factDirectoryFacts = failure}) id)
  otherValue <- normalizeIf remaining other
  no <- get
  put (updateNormalization (\currentContext -> currentContext {contextDirect = nDirect before}) id id (joinStates yes no))
  pure (P.Conditional conditionValue bodyValue [P.Statement Nothing otherValue])

normalizeCommand :: Token -> [Token] -> [Token] -> Normalize scope (P.StatementNode scope)
normalizeCommand token assignments commands
  | not (null assignments), not (null commands) = normalizePrefixed token assignments commands
normalizeCommand token assignments commands = case commands of
  [] -> P.AssignmentCommand False <$> traverse (normalizeAssignment False) assignments
  headToken : arguments -> do
    unless (null assignments) (reject token "command-prefix" "Command-prefix assignment lifetime is not yet materialized")
    name <- resolveHead headToken
    functions <- gets nFunctions
    if S.member name functions
      then do
        active <- gets nFunction
        descriptors <- gets nDescriptors
        writable <- gets nWritableDescriptors
        unless (descriptors == S.fromList [0, 1, 2] && S.fromList [1, 2] `S.isSubsetOf` writable) (reject token "call-descriptor-context" "Function invocation requires its admitted open standard descriptor context")
        when (Just name == active) (reject token "recursion" "Recursive calls are outside the finite initial call envelope")
        cfg <- gets nConfig
        localDefinitions <- gets nLocalFunctions
        resolutionFunctions <- gets nResolutionFunctions
        definitions <- gets nDefinitions
        summaries <- gets nFunctionDependencies
        possible <- gets nAllFunctions
        let dependencies = M.findWithDefault mempty name summaries
        unless
          (all (\(dependency, identity) -> if identity == AbsentDefinition then M.notMember dependency definitions && not (S.member dependency possible) else M.lookup dependency definitions == Just identity) (M.toList dependencies))
          (reject token "call-binding-context" "A function dependency was redefined after this body was normalized")
        locals <- gets nLocals
        initialized <- gets nVariables
        when
          (not (S.member name localDefinitions) && not (locals `S.isSubsetOf` initialized))
          (reject token "import-unset-environment" "Imported functions cannot observe an uninitialized local environment fallback")
        let target
              | S.member name localDefinitions = name
              | otherwise = maybe name functionTarget (M.lookup name (callerFunctions (callerContract cfg)))
            importedEffects =
              maybe False (not . S.null . S.intersection (S.fromList resolutionVariables) . functionWrites) (M.lookup name (callerFunctions (callerContract cfg)))
            importedDirectoryEffects = M.findWithDefault noDirectoryPermissions name (callerFunctionDirectories (callerContract cfg))
            writesDirectory access = access `elem` [WriteDirectory, ReadWriteDirectory]
            directoryAfterImport facts =
              Directory.MkDirectoryFacts
                (if writesDirectory (directoryCwd importedDirectoryEffects) then Directory.UnknownDirectory else Directory.directoryLocation facts)
                (Directory.directoryPreviousProved facts && not (writesDirectory (directoryOldpwd importedDirectoryEffects)))
        checkedFunctionCommand token target
        wordsValue <- normalizeWords arguments
        modify'
          ( \s ->
              updateNormalization id (\currentFacts -> currentFacts {factConstants = mempty, factNumeric = mempty, factArrays = M.map (const UnknownArray) (nArrays s), factResolutionStable = nResolutionStable s && not (S.member name resolutionFunctions || importedEffects), factDirectoryFacts = if S.member name resolutionFunctions || importedEffects then Directory.MkDirectoryFacts Directory.UnknownDirectory False else directoryAfterImport (nDirectoryFacts s), factCurrentDependencies = nCurrentDependencies s <> dependencies <> maybe mempty (M.singleton name) (M.lookup name definitions)}) id s
          )
        pure (P.Invoke (P.Function target) wordsValue)
      else do
        allFunctions <- gets nAllFunctions
        when (S.member name allFunctions) (reject token "call-binding" "The function's call-time definition is not definite")
        active <- gets nFunction
        unless (isNothing active || name `elem` ["command", "builtin"]) $
          modify' (\flow -> updateNormalization id (\currentFacts -> currentFacts {factCurrentDependencies = M.insert name AbsentDefinition (nCurrentDependencies flow)}) id flow)
        builtinCommand name arguments
  where
    builtinCommand name arguments = case name of
      "eval" -> normalizeEval token arguments
      "read" -> normalizeRead token arguments
      "trap" -> normalizeTrap token arguments
      "local" -> do
        direct <- gets nDirect
        active <- gets nFunction
        sourced <- gets (Control.inSource . nControl)
        when sourced (reject token "source-local-context" "A sourced local declaration needs its caller function's declaration scope")
        unless
          (direct && isJust active)
          (reject token "local-context" "Local bindings require a direct function-body declaration")
        normalizeDeclarations True token arguments
      "export" -> normalizeDeclarations False token arguments
      "readonly" -> do
        cfg <- gets nConfig
        unless
          (allowsApproximation cfg ReadonlyUnchecked)
          (reject token "readonly" "Readonly enforcement requires the named readonly-unchecked approximation")
        when
          (entryMode cfg == Sourceable)
          (reject token "readonly-caller" "Readonly caller binding attributes cannot be approximated by this sourceable contract")
        unless (length arguments == 1) (reject token "readonly-form" "Readonly approximation admits one explicit assignment; multiple operands need declaration expansion sequencing")
        values <- traverse (normalizeAssignment False) arguments
        range <- runtimeTokenRange token
        pure (P.Approximate ReadonlyUnchecked [P.Statement range (P.AssignmentCommand True values)])
      "unset" -> do
        names <- traverse (literalName token) arguments
        traverse_ (storageFor token False) names
        active <- gets nFunction
        unless (isNothing active) (reject token "unset-dynamic-local" "Function unset needs a modeled dynamic binding deletion")
        locals <- gets nLocals
        when (any (`S.member` locals) names) (reject token "unset-local" "Deleting local bindings requires explicit frame restoration")
        modify'
          ( \s ->
              updateNormalization id (\currentFacts -> currentFacts {factConstants = foldr M.delete (nConstants s) names, factNumeric = nNumeric s <> S.fromList names, factArrays = foldr M.delete (nArrays s) names, factVariables = nVariables s S.\\ S.fromList names, factResolutionStable = nResolutionStable s && not (any (`elem` resolutionVariables) names)}) id s
          )
        pure (P.Sequence [P.Statement Nothing (P.Erase nameValue) | nameValue <- names])
      "shift" -> do
        control <- gets nControl
        target <-
          maybe
            (reject token "source-argv-mutation" "Shift requires an owned argv; literal sources need a provably nonempty effective argument list")
            pure
            (Control.shiftTarget control)
        count <- case traverse getLiteralString arguments of
          Just [] -> pure 1
          Just [literal]
            | not (null literal),
              all isDigit literal,
              Just value <- readMaybe literal :: Maybe Integer,
              value <= min (toInteger (maxBound :: Int)) (2 ^ (63 :: Int) - 1) ->
                pure (fromInteger value)
          _ -> reject token "shift-operand" "Shift requires one nonnegative literal count"
        pure (Control.shiftStatement target count)
      "cd" -> normalizeDirectory token name arguments
      "pwd" -> normalizeDirectory token name arguments
      "pushd" -> normalizeDirectory token name arguments
      "popd" -> normalizeDirectory token name arguments
      "test" -> normalizeFixedTest token "test" arguments
      "[" -> normalizeFixedTest token "[" arguments
      "set" -> normalizeSet token arguments
      "wait" -> do
        requireSession token
        evaluated <- gets nEvaluatedPrograms
        unless (null evaluated) (reject token "eval-wait-diagnostic" "Eval wait needs exact nested diagnostic source locations")
        values <- normalizeWords (case arguments of marker : rest | getLiteralString marker == Just "--" -> rest; _ -> arguments)
        numeric <- gets nNumeric
        let pidScalar = \case
              P.LastBackgroundPid -> True
              P.Literal value -> T.all isDigit value
              P.Variable bindingName -> S.member bindingName numeric
              P.ArithmeticValue {} -> True
              P.ArgumentCount -> True
              P.LastStatus -> True
              _ -> False
            pidWord = \case P.OneField value -> pidScalar value; P.SplitFields value -> pidScalar value; _ -> False
        unless (all pidWord values) (reject token "wait-operand" "Wait operands require proved numeric PID values; job specs and options are outside this envelope")
        pure (P.Wait values)
      "return" -> do
        control <- gets nControl
        target <- maybe (reject token "return-context" "Return requires an owned function or source boundary") pure (Control.returnTarget control)
        value <- optionalStatus arguments
        when (Control.returnsFromSource target) $ modify' (\s -> updateNormalization id (\currentFacts -> currentFacts {factSourceReturns = S.insert (entryContext s) (nSourceReturns s)}) id s)
        pure (Control.returnStatement target value)
      "exit" -> P.Exit <$> optionalStatus arguments
      "break" -> loopControl True arguments
      "continue" -> loopControl False arguments
      "source" -> normalizeSourceCall token arguments
      "." -> normalizeSourceCall token arguments
      "builtin" -> case arguments of
        next : rest -> do
          selected <- literalName token next
          unless (selected `elem` ("trap" : "read" : "wait" : "eval" : "local" : "export" : "cd" : "pwd" : "pushd" : "popd" : safeBuiltins)) (reject token "builtin" "The selected builtin has no admitted operand semantics")
          builtinCommand selected rest
        [] -> reject token "builtin" "Builtin requires a command operand"
      "command" -> case arguments of
        next : rest -> do
          selected <- resolveHead next
          if selected `elem` ("trap" : "read" : "wait" : "eval" : "local" : "export" : "cd" : "pwd" : "pushd" : "popd" : safeBuiltins)
            then builtinCommand selected rest
            else do
              when (selected `elem` unsupportedBuiltins) (reject token "command-builtin" "This dispatched builtin has no admitted semantics")
              P.Invoke (P.External selected) <$> normalizeConsumerWords selected rest
        [] -> reject token "command" "Command requires an executable operand"
      _ -> do
        when (name `elem` unsupportedBuiltins) (reject token "builtin" ("No admitted semantics for builtin " <> name))
        when (name == "printf") (validatePrintf token arguments)
        cfg <- gets nConfig
        when
          (entryMode cfg == Sourceable && name `notElem` safeBuiltins)
          (reject token "ambient-dispatch" "Sourceable external dispatch requires an explicit import")
        P.Invoke (if name `elem` safeBuiltins then P.Builtin name else P.External name) <$> normalizeConsumerWords name arguments
    optionalStatus [] = pure Nothing
    optionalStatus [value] = do
      scalar <- normalizeScalar value
      constants <- gets nConstants
      let literal text = case readMaybe (toString text) :: Maybe Integer of
            Just number | show number == text, number >= negate (2 ^ (63 :: Int)), number < 2 ^ (63 :: Int) -> pure (Just (P.Literal (show (number `mod` 256))))
            _ -> reject token "status-argument" "Status operands require canonical signed-64-bit decimal data"
      case scalar of
        P.Literal text -> literal text
        P.Variable name | Just text <- M.lookup name constants -> literal text
        P.LastStatus -> pure (Just scalar)
        _ -> reject token "status-argument" "A status operand needs a proven bounded decimal value or the previous status"
    optionalStatus _ = reject token "status-argument" "Exit and return accept at most one status argument"
    loopControl isBreak [] = do
      control <- gets nControl
      target <- maybe (reject token "loop-control" "Loop control has no owned target") pure (Control.loopTarget control)
      if isBreak
        then do
          modify' (\s -> updateNormalization id (\currentFacts -> currentFacts {factBreakNumeric = M.insertWith S.intersection (Control.loopKey target) (nNumeric s) (nBreakNumeric s)}) id s)
          pure (Control.breakStatement target)
        else do
          modify' (\s -> updateNormalization id (\currentFacts -> currentFacts {factContinueNumeric = M.insertWith S.intersection (Control.loopKey target) (nNumeric s) (nContinueNumeric s)}) id s)
          pure (Control.continueStatement target)
    loopControl _ _ = reject token "loop-control-depth" "Only the immediate owned loop target is currently admitted"

-- Proved operands are parsed once during translation and then use the same
-- statement normalizer as the enclosing script. No eval reaches the renderer.
normalizePrefixed :: Token -> [Token] -> [Token] -> Normalize scope (P.StatementNode scope)
normalizePrefixed token assignments command = do
  requireSession token
  case command of
    headToken : _ | isJust (getLiteralString headToken) -> pure ()
    _ -> reject token "prefix-command" "Temporary command bindings require a literal executable identity"
  invocation <- normalizeCommand token [] command
  makeInvocation <- case invocation of
    P.Invoke callTarget@(P.External _) wordsValue -> pure (\values -> P.PrefixedInvoke values callTarget wordsValue)
    P.Invoke callTarget@(P.Builtin name) wordsValue | name `elem` safeBuiltins -> pure (\values -> P.PrefixedInvoke values callTarget wordsValue)
    P.Read options target | all ifsAssignment assignments, not (targetIsIfs target) -> pure (\values -> P.PrefixedRead values options target)
    _ -> reject token "prefix-command" "Temporary bindings require ordinary calls or an IFS-only read prefix"
  before <- get
  names <- forM assignments $ \assignment -> case assignment of
    T_Assignment _ Assign name [] _ -> checkedName assignment (toText name) >> rejectArrayScalar assignment (toText name) >> pure (toText name)
    _ -> reject token "prefix-assignment" "Temporary command bindings require replacing scalar assignments"
  let nameSet = S.fromList names
  values <- forM assignments $ \assignment -> do
    planned <- normalizeAssignment False assignment
    case planned of
      P.Statement _ (P.Assign storage name scalar) -> do
        unless (S.null (Effects.effectWrites (Effects.scalarEffects scalar) `S.intersection` nameSet)) (reject assignment "prefix-rhs-write" "Prefix RHS writes to temporary binding names require a separate scope proof")
        pure (storage, name, scalar)
      _ -> reject assignment "prefix-assignment" "Temporary command bindings require scalar values"
  after <- get
  let restoreMap :: M.Map Text a -> M.Map Text a -> M.Map Text a
      restoreMap old new = M.restrictKeys old nameSet <> M.withoutKeys new nameSet
      restoreSet old new = (old `S.intersection` nameSet) <> (new S.\\ nameSet)
      rhsWrites = foldMap (Effects.effectWrites . Effects.scalarEffects . (\(_, _, value) -> value)) values
  put (updateNormalization id (\currentFacts -> currentFacts {factConstants = restoreMap (nConstants before) (nConstants after), factNumeric = restoreSet (nNumeric before) (nNumeric after), factArrays = restoreMap (nArrays before) (nArrays after), factVariables = restoreSet (nVariables before) (nVariables after), factResolutionStable = nResolutionStable before && S.null (rhsWrites `S.intersection` S.fromList resolutionVariables)}) id after)
  pure (makeInvocation values)
  where
    ifsAssignment (T_Assignment _ Assign "IFS" [] _) = True
    ifsAssignment _ = False
    targetIsIfs (P.ReadScalars names) = any ((== "IFS") . snd) names
    targetIsIfs (P.ReadArray _ name) = name == "IFS"
    targetIsIfs _ = False

normalizeTrap :: Token -> [Token] -> Normalize scope (P.StatementNode scope)
normalizeTrap token operands = do
  requireSession token
  arguments <- traverse (\operand -> maybe (reject operand "trap-handler" "Trap operands require literal source text and signal names") (pure . toText) (getLiteralString operand)) operands
  (handler, kind) <- case arguments of
    [source, signal] -> (source,) <$> trapKind signal
    ["--", source, signal] -> (source,) <$> trapKind signal
    _ -> reject token "trap-form" "Trap requires one literal handler and EXIT or ERR"
  if handler == "-"
    then pure (P.SetTrap kind Nothing)
    else do
      before <- get
      when (length (nEvaluatedPrograms before) >= 64) (reject token "trap-depth" "Nested compiled callback definitions exceed the finite admission bound")
      let Id ordinal = getId token
          file = documentNameFrom before <> ":trap:" <> show ordinal
          parsed = parseBashFragment (toString file) handler
      root <- maybe (reject token "trap-parse" "Literal trap body is not valid Bash syntax") pure (prRoot parsed)
      scopedControl Control.withHandlerControl $ \bodyRoot _ -> do
        modify' (updateNormalization (\currentContext -> currentContext {contextPositions = prTokenPositions parsed, contextDocument = Just handler, contextDirect = False}) (\currentFacts -> currentFacts {factConstants = mempty, factNumeric = mempty, factArrays = M.map (const UnknownArray) (nArrays before), factErrTrapWrites = mempty}) (\currentDiscoveries -> currentDiscoveries {discoveredEvaluatedPrograms = handler : nEvaluatedPrograms before}))
        body <- normalizeStatement root
        unless (admittedHandler body) (reject token "trap-body" "Initial callbacks require builtin/scalar control flow without external lookup or context transfers")
        after <- get
        let effects = Effects.statementEffects body
            writes = if kind == P.ErrTrap then Effects.effectWrites effects else mempty
        let restored = updateNormalization id (\currentFacts -> currentFacts {factErrTrapWrites = nErrTrapWrites before <> writes}) (\currentDiscoveries -> currentDiscoveries {discoveredReserved = nReserved before <> nReserved after}) before
            owned = P.scopedBody bodyRoot [body]
        pure (P.SetTrap kind (Just owned), restored)
  where
    trapKind "EXIT" = pure P.ExitTrap
    trapKind "0" = pure P.ExitTrap
    trapKind "ERR" = pure P.ErrTrap
    trapKind _ = reject token "trap-signal" "Only deferred EXIT and ERR callbacks are admitted"
    documentNameFrom flow = fromMaybe "<input>" (viaNonEmpty last (nSourceStack flow))
    admittedHandler :: P.Statement scope -> Bool
    admittedHandler (P.Statement _ node) = case node of
      P.Sequence values -> all admittedHandler values
      P.AssignmentCommand _ values -> all admittedHandler values
      P.Assign {} -> True
      P.AssignArray {} -> True
      P.AppendArray {} -> True
      P.AssignArrayElement {} -> True
      P.Invoke (P.Builtin _) _ -> True
      P.And left right -> all admittedHandler [left, right]
      P.Or left right -> all admittedHandler [left, right]
      P.Negate value -> admittedHandler value
      P.Conditional condition yes no -> all admittedHandler (condition <> yes <> no)
      P.WhileLoop _ body -> P.withWhileBody body (\_ _ condition values -> all admittedHandler (condition <> values))
      P.ForLoop _ _ _ body -> P.withForBody body (\_ _ -> all admittedHandler)
      P.Case _ arms -> all (\(P.CaseArm _ body _) -> all admittedHandler body) arms
      P.PatternCondition {} -> True
      P.NumericCondition {} -> True
      P.ArithmeticCommand {} -> True
      P.SetTrap _ body -> maybe True (\owned -> P.withScopedBody owned (\_ -> all admittedHandler)) body
      P.Exit {} -> True
      _ -> False

normalizeEval :: Token -> [Token] -> Normalize scope (P.StatementNode scope)
normalizeEval token operands = do
  arguments <- normalizeWords operands
  before <- get
  let known = \case
        P.Literal text -> Just text
        P.Variable name -> M.lookup name (nConstants before)
        P.Concat parts -> T.concat <$> traverse known parts
        _ -> Nothing
      knownWord = \case P.OneField value -> known value; _ -> Nothing
  values <- maybe (reject token "eval" "Eval operands need pure, single-field, compile-time-proved source text") pure (traverse knownWord arguments)
  let sourceArguments = case values of "--" : rest -> rest; _ -> values
  when (maybe False (T.isPrefixOf "-") (listToMaybe sourceArguments)) (reject token "eval-option" "Eval option errors need a separate diagnostic contract")
  let source = T.intercalate " " sourceArguments
  when
    (source `elem` nEvaluatedPrograms before || length (nEvaluatedPrograms before) >= 64)
    (reject token "eval-recursion" "Eval source exceeds the finite compilation nesting envelope")
  range <- tokenRange token
  let Id ordinal = getId token
      name = maybe "<input>" (srcFile . rangeStart) range <> ":eval:" <> show ordinal
      parsed = parseBashFragment (toString name) source
  root <- maybe (reject token "eval-parse" "Proved eval source is not a valid Bash program") pure (prRoot parsed)
  modify'
    ( \flow ->
        updateNormalization (\currentContext -> currentContext {contextDocument = Just source, contextPositions = prTokenPositions parsed}) id (\currentDiscoveries -> currentDiscoveries {discoveredEvaluatedPrograms = source : nEvaluatedPrograms before, discoveredAllFunctions = nAllFunctions flow <> functionNames root, discoveredReserved = nReserved flow <> sourceNames root}) flow
    )
  body <- normalizeStatement root
  modify'
    (updateNormalization (\currentContext -> currentContext {contextDocument = nDocument before, contextPositions = nPositions before, contextCommandLine = nCommandLine before}) id (\currentDiscoveries -> currentDiscoveries {discoveredEvaluatedPrograms = nEvaluatedPrograms before}))
  pure $ case body of
    P.Statement _ (P.Sequence []) -> P.Invoke (P.Builtin "true") []
    _ -> P.Sequence [body]

normalizeDeclarations :: Bool -> Token -> [Token] -> Normalize scope (P.StatementNode scope)
normalizeDeclarations local parent operands = do
  when (null operands) (reject parent "declaration-inspection" "Declaration inspection requires an explicit binding operation")
  -- Expansions observe the command-entry binding scope, with expansion effects
  -- sequenced, before any declaration installs its destination.
  values <- forM operands $ \operand -> do
    (name, value) <- case operand of
      T_Assignment _ Assign name [] value -> pure (toText name, Just value)
      _ -> (,Nothing) <$> literalName parent operand
    checkedName operand name
    rejectArrayScalar operand name
    when (not local && name == "IFS") (reject operand "export-ifs" "Exported IFS requires an explicit source environment alias")
    storage <- storageFor operand local name
    scalar <- traverse normalizeScalar value
    cfg <- gets nConfig
    initialized <- gets nVariables
    locals <- gets nLocals
    when
      (not local && entryMode cfg == Sourceable && isNothing scalar && not (S.member name initialized || S.member name locals))
      (reject operand "export-unset-caller" "Exporting an unset caller binding requires a persistent caller attribute contract")
    pure (name, storage, scalar)
  declarations <- forM values $ \(name, storage, value) -> do
    locals <- gets nLocals
    let freshLocal = local && not (S.member name locals)
    modify'
      ( \flow ->
          updateNormalization id (\currentFacts -> currentFacts {factLocals = if local then S.insert name (nLocals flow) else nLocals flow, factVariables = case value of Just _ -> S.insert name (nVariables flow); Nothing | freshLocal -> S.delete name (nVariables flow); _ -> nVariables flow, factNumeric = case value of Just scalar -> (if numericScalar scalar then S.insert else S.delete) name (nNumeric flow); Nothing | freshLocal -> S.insert name (nNumeric flow); _ -> nNumeric flow, factConstants = case value of Just scalar -> maybe (M.delete name (nConstants flow)) (\literal -> M.insert name literal (nConstants flow)) (scalarLiteral scalar); Nothing | freshLocal -> M.insert name "" (nConstants flow); _ -> nConstants flow, factResolutionStable = nResolutionStable flow && name `notElem` resolutionVariables}) id flow
      )
    pure (if local then P.DeclareLocal freshLocal name value else P.DeclareExport storage name value)
  pure (P.DeclarationCommand declarations)

-- A write is proved only after its RHS has established the current facts.
updateArray :: Token -> Text -> Either Int Natural -> Normalize scope ()
updateArray token name operation = do
  checkedName token name
  flow <- get
  case transitionDense prove flow of
    Left (code, message) -> reject token code message
    Right updated -> put updated
  where
    prove :: FlowFacts world -> Either (Text, Text) (DenseUpdate world)
    prove facts = do
      proof <- maybe (Left ("array-shape", "An indexed array requires a proved dense owned shape")) Right (lookupDense facts name)
      case operation of
        Left offset -> maybe (Left ("array-sparse-write", "Indexed assignment cannot create a sparse array")) Right (writeDenseAt facts proof offset)
        Right count -> Right (appendDenseBy facts proof count)

literalArrayIndex :: Token -> Normalize scope Int
literalArrayIndex token = case token of
  TA_Sequence _ [value] -> literalArrayIndex value
  TA_Expansion _ [value] -> literalArrayIndex value
  T_Literal _ value -> maybe invalid pure (decimalIndex (toText value))
  _ -> invalid
  where
    invalid = reject token "array-index" "Indexed array writes require a nonnegative decimal literal index"

normalizeAssignment :: Bool -> Token -> Normalize scope (P.Statement scope)
normalizeAssignment local token = do
  node <- case token of
    T_Assignment _ operation rawName [] (T_Array _ elements) -> do
      let name = toText rawName
      storage <- arrayStorage token local name
      wordsValue <- normalizeWords elements
      unless (all (\case P.OneField _ -> True; _ -> False) wordsValue) (reject token "array-construction-cardinality" "Dense array construction currently needs one field per element")
      if operation == Append
        then updateArray token name (Right (fromIntegral (length wordsValue)))
        else rememberArray name (KnownLength (fromIntegral (length wordsValue)))
      pure ((if operation == Append then P.AppendArray else P.AssignArray) storage name wordsValue)
    T_Assignment _ Assign rawName [index] value -> do
      let name = toText rawName
      storage <- arrayStorage token local name
      offset <- literalArrayIndex index
      scalar <- normalizeScalar value
      updateArray token name (Left offset)
      pure (P.AssignArrayElement storage name offset scalar)
    _ -> do
      (name, value, appendValue) <- case token of
        T_Assignment _ Assign name [] value -> pure (toText name, value, False)
        T_Assignment _ Append name [] value -> pure (toText name, value, True)
        _ | local, isJust (getLiteralString token) -> reject token "unset-local-declaration" "Bare local declarations require an unset binding with owned scope lifetime"
        _ -> reject token "assignment-shape" "Assignment has no proved scalar or dense indexed storage plan"
      checkedName token name
      arrays <- gets nArrays
      if M.member name arrays
        then do
          when appendValue (reject token "array-scalar-append" "Appending to element zero requires a separate array element update")
          storage <- arrayStorage token local name
          scalar <- normalizeScalar value
          updateArray token name (Left 0)
          pure (P.AssignArrayElement storage name 0 scalar)
        else do
          rhs <- normalizeScalar value
          when appendValue (readBinding token name)
          let valuePlan = if appendValue then P.AppendValue name rhs else rhs
          storage <- storageFor token local name
          modify'
            ( \flow ->
                updateNormalization id (\currentFacts -> currentFacts {factVariables = S.insert name (nVariables flow), factLocals = if local then S.insert name (nLocals flow) else nLocals flow, factResolutionStable = nResolutionStable flow && name `notElem` resolutionVariables, factNumeric = (if numericScalar valuePlan then S.insert else S.delete) name (nNumeric flow), factConstants = case scalarLiteral valuePlan of Just literal -> M.insert name literal (nConstants flow); Nothing -> M.delete name (nConstants flow)}) id flow
            )
          pure (P.Assign storage name valuePlan)
  range <- runtimeTokenRange token
  pure (P.Statement range node)

resolveHead :: Token -> Normalize scope Text
resolveHead token = case getLiteralString token of
  Just name -> checkedCommand token (toText name) >> pure (toText name)
  Nothing -> do
    scalar <- normalizeScalar token
    constants <- gets nConstants
    case scalar of
      P.Variable name | Just value <- M.lookup name constants -> checkedCommand token value >> pure value
      _ -> reject token "dynamic-command" "The command head has no single proven binding"

normalizeCondition :: Token -> Normalize scope (P.StatementNode scope)
normalizeCondition token = case token of
  TC_Nullary _ SingleBracket value -> normalizeFixedTest token "test" [value]
  TC_Unary _ SingleBracket "!" value -> P.Negate . P.Statement Nothing <$> normalizeCondition value
  TC_Unary _ SingleBracket operator value -> normalizeFixedTest token "test" [T_NormalWord (getId token) [T_Literal (getId token) operator], value]
  TC_Binary _ SingleBracket operator left right -> normalizeFixedTest token "test" [left, T_NormalWord (getId token) [T_Literal (getId token) operator], right]
  TC_Group _ DoubleBracket inner -> normalizeCondition inner
  TC_And _ DoubleBracket "&&" left right -> lazyCondition P.And left right
  TC_Or _ DoubleBracket "||" left right -> lazyCondition P.Or left right
  TC_Unary _ DoubleBracket "!" value -> P.Negate . P.Statement Nothing <$> normalizeCondition value
  TC_Nullary _ _ value -> P.Invoke (P.Builtin "test") . (P.OneField (P.Literal "-n") :) . (: []) . P.OneField <$> normalizeScalar value
  TC_Unary _ _ operator value
    | toText operator `elem` ["-n", "-z", "-e", "-f", "-d", "-r", "-w", "-x"] ->
        P.Invoke (P.Builtin "test") . (P.OneField (P.Literal (toText operator)) :) . (: []) . P.OneField <$> normalizeScalar value
  TC_Binary _ _ operator left right
    | toText operator `elem` ["=", "==", "!="] ->
        P.PatternCondition (operator == "!=") <$> normalizeScalar left <*> normalizePattern right
  TC_Binary _ _ operator left right | Just comparison <- Operator.parseNumericComparison (toText operator) -> do
    a <- normalizeScalar left
    b <- normalizeScalar right
    known <- gets nNumeric
    let valid = \case
          P.Literal value -> numericLiteral value
          P.Variable name -> S.member name known
          P.LastStatus -> True
          P.ArgumentCount -> True
          P.ArithmeticValue {} -> True
          _ -> False
    unless
      (valid a && valid b)
      (reject token "arithmetic-comparison" "Numeric comparison operands must be proven integer data, not runtime expression strings")
    constants <- gets nConstants
    pure (freezeComparison constants comparison a b)
  _ -> reject token "condition" "Condition operators require exact operand and pattern semantics"
  where
    lazyCondition constructor left right = do
      leftValue <- normalizeCondition left
      before <- get
      rightValue <- normalizeCondition right
      after <- get
      put (joinStates before after)
      pure (constructor (P.Statement Nothing leftValue) (P.Statement Nothing rightValue))

normalizeStatements :: [Token] -> Normalize scope [P.Statement scope]
normalizeStatements [] = pure []
normalizeStatements (token : remaining) = do
  statement <- normalizeStatement token
  rest <- if continues statement then normalizeStatements remaining else pure []
  pure (statement : rest)
  where
    continues (P.Statement _ node) = case node of
      P.Return {} -> False
      P.Exit {} -> False
      P.Break _ -> False
      P.Continue _ -> False
      P.Sequence statements -> all continues statements
      P.Conditional _ yes no -> any continues yes || null no || any continues no
      _ -> True

normalizeArithmeticAt :: Token -> Normalize scope (ArithmeticSource.ArithmeticSite, A.ArithmeticExpr, M.Map Text P.Storage)
normalizeArithmeticAt token = do
  (value, _, _) <- normalizeArithmeticRegion token
  pure value

normalizeArithmeticRegion :: Token -> Normalize scope ((ArithmeticSource.ArithmeticSite, A.ArithmeticExpr, M.Map Text P.Storage), (S.Set Text, S.Set Text), Bool)
normalizeArithmeticRegion token = do
  expression <- either (reject token "arithmetic-shape") pure (A.normalizeArithmetic token)
  source <- gets nDocument >>= maybe (reject token "arithmetic-source" "Exact arithmetic errors require the immutable original source document") pure
  origin <- gets nRuntimeOrigin
  positions <- gets (M.map (\(start, end) -> (start {posFile = toString origin}, end {posFile = toString origin})) . nPositions)
  commandLine <- gets nCommandLine
  site <- either (reject token "arithmetic-source") pure (ArithmeticSource.arithmeticSite commandLine source positions token expression)
  before <- get
  let constants = nConstants before
  validate expression
  success <- get
  let potentialFailure = mayFail (freezeConstants (M.withoutKeys constants (writes expression)) expression)
  when
    (potentialFailure && not (null (nEvaluatedPrograms before)))
    (reject token "eval-arithmetic-diagnostic" "Potential eval arithmetic errors need exact nested diagnostic source locations")
  when potentialFailure $ do
    after <- get
    -- A failed arithmetic command may continue its enclosing statement list.
    -- Writes after the failure are not definite; partial writes before it also
    -- prevent restoring an entry constant even when the final value agrees.
    put (updateNormalization id (\currentFacts -> currentFacts {factConstants = M.withoutKeys (nConstants (joinStates before after)) (writes expression)}) id (joinStates before after))
  bindings <- M.fromList <$> traverse (\name -> (name,) <$> storageFor token False name) (S.toList (writes expression))
  pure ((site, freezeConstants (M.withoutKeys constants (writes expression)) expression, bindings), (nNumeric success, nVariables success), potentialFailure)
  where
    mayFail expression | isJust (A.constantValue expression) = False
    mayFail expression = case expression of
      A.ArithmeticLocated _ inner -> mayFail inner
      A.ArithmeticUnary _ inner -> mayFail inner
      A.ArithmeticBinary operator left right -> failingOperator operator right || mayFail left || mayFail right
      A.ArithmeticAssign _ operator right -> maybe False (`failingOperator` right) operator || mayFail right
      A.ArithmeticConditional predicate yes no -> any mayFail [predicate, yes, no]
      A.ArithmeticSequence expressions -> any mayFail expressions
      _ -> False
    failingOperator operator right = case operator of
      A.Divide -> maybe True (== 0) (A.constantValue right)
      A.Remainder -> maybe True (== 0) (A.constantValue right)
      A.Power -> maybe True (< 0) (A.constantValue right)
      _ -> False
    -- Freeze only reads whose binding is not written anywhere in this region.
    -- Source validation and error origins use the original operation tree.
    freezeConstants constants = \case
      A.ArithmeticLocated location inner -> A.ArithmeticLocated location (freezeConstants constants inner)
      original@(A.ArithmeticVariable name) -> maybe original A.ArithmeticLiteral (M.lookup name constants >>= rightToMaybe . Integer.parseNumber . encodeUtf8)
      A.ArithmeticUnary operator inner -> A.ArithmeticUnary operator (freezeConstants constants inner)
      A.ArithmeticBinary operator left right -> A.ArithmeticBinary operator (freezeConstants constants left) (freezeConstants constants right)
      A.ArithmeticAssign name operator right -> A.ArithmeticAssign name operator (freezeConstants constants right)
      A.ArithmeticConditional predicate yes no -> A.ArithmeticConditional (freezeConstants constants predicate) (freezeConstants constants yes) (freezeConstants constants no)
      A.ArithmeticSequence expressions -> A.ArithmeticSequence (fmap (freezeConstants constants) expressions)
      other -> other
    readNumeric name = do
      rejectArrayScalar token name
      readBinding token name
      known <- gets nNumeric
      unless (name == "#" || S.member name known) (reject token "arithmetic-binding" "Arithmetic variable values must be proven numeric, not runtime expression strings")
    written name = do
      checkedName token name
      rejectArrayScalar token name
      modify' (\s -> updateNormalization id (\currentFacts -> currentFacts {factNumeric = S.insert name (nNumeric s), factVariables = S.insert name (nVariables s), factConstants = M.delete name (nConstants s), factResolutionStable = nResolutionStable s && name `notElem` resolutionVariables}) id s)
    validate = \case
      A.ArithmeticLocated _ expression -> validate expression
      A.ArithmeticLiteral _ -> pure ()
      A.ArithmeticVariable name -> readNumeric name
      A.ArithmeticUnary _ expression -> validate expression
      A.ArithmeticBinary operator left right -> do
        validate left
        before <- get
        validate right
        when (operator `elem` [A.LogicalAnd, A.LogicalOr]) (get >>= put . joinStates before)
      A.ArithmeticAssign name operator expression -> do
        when (isJust operator) (readNumeric name)
        validate expression
        written name
      A.ArithmeticUpdate _ _ name -> readNumeric name >> written name
      A.ArithmeticConditional predicate yes no -> do
        validate predicate
        before <- get
        validate yes
        firstBranch <- get
        put before
        validate no
        secondBranch <- get
        put (joinStates firstBranch secondBranch)
      A.ArithmeticSequence expressions -> traverse_ validate expressions
    writes = \case
      A.ArithmeticLocated _ expression -> writes expression
      A.ArithmeticLiteral _ -> mempty
      A.ArithmeticVariable _ -> mempty
      A.ArithmeticUnary _ expression -> writes expression
      A.ArithmeticBinary _ left right -> writes left <> writes right
      A.ArithmeticAssign name _ expression -> S.insert name (writes expression)
      A.ArithmeticUpdate _ _ name -> S.singleton name
      A.ArithmeticConditional predicate yes no -> writes predicate <> writes yes <> writes no
      A.ArithmeticSequence expressions -> foldMap writes expressions

wordNormalizer :: Words.WordNormalizer scope
wordNormalizer = Words.wordNormalizer (Words.WordCallbacks normalizeChild normalizeArithmeticAt)

normalizeWords :: [Token] -> Normalize scope [P.Word]
normalizeWords = Words.normalizedWords wordNormalizer

normalizeConsumerWords :: Text -> [Token] -> Normalize scope [P.Word]
normalizeConsumerWords = Words.normalizedConsumerWords wordNormalizer

normalizeScalar :: Token -> Normalize scope P.Scalar
normalizeScalar = Words.normalizedScalar wordNormalizer

normalizePattern :: Token -> Normalize scope P.Pattern
normalizePattern = Words.normalizedPattern wordNormalizer

normalizeSourceCall :: Token -> [Token] -> Normalize scope (P.StatementNode scope)
normalizeSourceCall = Sources.normalizeSourceCall (Sources.SourceCallbacks normalizeWords normalizeStatement)

normalizeRead :: Token -> [Token] -> Normalize scope (P.StatementNode scope)
normalizeRead = Commands.normalizeRead wordNormalizer

normalizeDirectory :: Token -> Text -> [Token] -> Normalize scope (P.StatementNode scope)
normalizeDirectory = Commands.normalizeDirectory wordNormalizer

normalizeFixedTest :: Token -> Text -> [Token] -> Normalize scope (P.StatementNode scope)
normalizeFixedTest = Commands.normalizeFixedTest wordNormalizer

normalizeSet :: Token -> [Token] -> Normalize scope (P.StatementNode scope)
normalizeSet = Commands.normalizeSet wordNormalizer

controlCallbacks :: Control.ControlCallbacks
controlCallbacks = Control.ControlCallbacks normalizeStatement normalizeStatements normalizeRedirected

normalizeLoop :: Token -> Bool -> [Token] -> [Token] -> Normalize scope (P.StatementNode scope)
normalizeLoop = Control.normalizeLoop controlCallbacks

normalizeFunction :: Token -> Text -> Token -> Normalize scope (P.StatementNode scope)
normalizeFunction = Control.normalizeFunction controlCallbacks

normalizeFunctionWith :: [Token] -> Token -> Text -> Token -> Normalize scope (P.StatementNode scope)
normalizeFunctionWith = Control.normalizeFunctionWith controlCallbacks

normalizeProcess :: Token -> Normalize scope P.Scalar
normalizeProcess = Words.normalizedProcess wordNormalizer

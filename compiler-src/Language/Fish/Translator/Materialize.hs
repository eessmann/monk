{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | Materialize one normalized lexical body. Root-indexed control, operand
-- emissions and complete request templates meet in this structural walker.
module Language.Fish.Translator.Materialize (lowerStatements) where

import Control.Monad.State.Strict (gets)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Language.Bash.Arithmetic.Plan qualified as A
import Language.Bash.Plan qualified as P
import Language.Bash.Plan.Control qualified as Control
import Language.Bash.Plan.Effects qualified as Effects
import Language.Bash.Plan.Operator qualified as Operator
import Language.Fish.DSL.Argument
import Language.Fish.DSL.Internal
import Language.Fish.Translator.ArithmeticDiagnostic (arithmeticDiagnostic)
import Language.Fish.Translator.ArithmeticPlan qualified as Arithmetic
import Language.Fish.Translator.Binding qualified as Binding
import Language.Fish.Translator.Boundary
import Language.Fish.Translator.Child qualified as Child
import Language.Fish.Translator.Context
import Language.Fish.Translator.Directory qualified as Directory
import Language.Fish.Translator.Emission (Emission, emit, renderEmission)
import Language.Fish.Translator.HelperRegistry qualified as Helpers
import Language.Fish.Translator.Identifier (compilerCommandName, compilerIdentifier)
import Language.Fish.Translator.Invocation qualified as Invocation
import Language.Fish.Translator.Native qualified as Native
import Language.Fish.Translator.NativeRuntime qualified as NativeRuntime
import Language.Fish.Translator.Pattern qualified as Pattern
import Language.Fish.Translator.Region qualified as Region
import Language.Fish.Translator.Session qualified as Session
import Language.Fish.Translator.Session.Request qualified as Request
import Language.Fish.Translator.Statement
import Language.Fish.Translator.Statistics (commandReferences)
import Language.Fish.Translator.Traps qualified as Traps
import Language.Fish.Translator.Words qualified as Words
import Monk.Runtime.Integer qualified as Integer
import Monk.Translation.Types
import Prelude hiding (exitFailure, first, force, gets, isPrefixOf, one, second)

-- Native session stages consume the same compiled child snapshots as bounded
-- helpers. External-only stages keep their real executable PID.
sessionStage :: P.ChildRegion -> Materialize scope (Emission (Request.SomeStage, [FishStatement]))
sessionStage region = P.withScopedBody (P.childBody region) $ \_ statements -> case concatMap flattenStatements statements of
  [P.Statement _ (P.Invoke (P.External name) wordsValue)] | all pureStageWord wordsValue -> do
    arguments <- Words.materializeWords wordMaterializer wordsValue
    needProgram (RequiresCommand name) "Owned external pipeline stage"
    let origin = maybe "<input>" (srcFile . rangeStart) (P.childRange region)
        line = maybe "1" (show . srcLine . rangeStart) (P.childRange region)
        stage fields = Request.SomeStage (Request.ExternalSiteStage (Request.Site (ExprLiteral origin) (ExprLiteral line)) (ExprLiteral name) fields)
    pure (fmap (\fields -> (stage fields, [])) (Region.fieldsEmission arguments))
  _ -> do
    invocation <- childMaterialization Child.IsolatedChild region
    pure (emit (Child.childSnapshotPrelude invocation) >> pure (Request.SomeStage (Child.childSessionStage invocation), childCleanup invocation))

pureStageWord :: P.Word -> Bool
pureStageWord (P.OneField value) = pureStageScalar value
pureStageWord (P.QuotedArguments before after _) = pureStageScalar before && pureStageScalar after
pureStageWord _ = False

pureStageScalar :: P.Scalar -> Bool
pureStageScalar = \case
  P.Literal _ -> True
  P.Variable _ -> True
  P.Positional _ -> True
  P.ArgumentCount -> True
  P.LastStatus -> True
  P.LastBackgroundPid -> True
  P.Concat values -> all pureStageScalar values
  _ -> False

flattenStatements :: P.Statement scope -> [P.Statement scope]
flattenStatements (P.Statement _ (P.Sequence body)) = concatMap flattenStatements body
flattenStatements statement = [statement]

data PipelineMode = RunPipeline | SpawnPipeline

lowerSessionPipeline :: Bool -> PipelineMode -> NonEmpty P.ChildRegion -> Materialize scope [FishStatement]
lowerSessionPipeline suppressed mode regions = do
  stages <- traverse sessionStage regions
  pipefail <- runtimeName "pipefail"
  saved <- forM stages $ \stage -> do
    name <- fresh "stage"
    pure $ do
      (body, cleanup) <- stage
      prepared <- Request.prepareStage name (pure body)
      pure (prepared, cleanup)
  captured <- captureStatus
  guards <- case mode of SpawnPipeline -> pure []; RunPipeline -> errexitGuard suppressed
  emitter <- projectedSessionRequest
  pure $ renderEmission $ do
    prepared <- sequence saved
    let body = Request.pipelineBody (scalarVar pipefail) (fmap fst prepared)
        invocation = case mode of RunPipeline -> emitSessionRequest emitter (Request.Run body); SpawnPipeline -> emitSessionRequest emitter (Request.Spawn body)
    pure ([invocation, captured] <> foldMap snd prepared <> guards)

lowerStatements :: Bool -> [P.Statement scope] -> Materialize scope [FishStatement]
lowerStatements _ [] = pure []
lowerStatements suppressed statements@(firstStatement : remaining) = do
  config <- gets materialConfig
  session <- gets materialSession
  traps <- gets materialTraps
  errexit <- gets materialErrexitRelevant
  prefix <- gets materialPrefix
  let eligible = entryMode config == Standalone && not session && not traps && not errexit
  root <- gets materialRoot
  case if eligible then Native.nativeRegionPrefix root prefix (prefix <> "status") statements else Nothing of
    Just (native, rest, usesWriter) -> do
      when usesWriter $ do
        needNative NativeWrite "Bash output errno and signal semantics"
        registerHelpers [NativeRuntime.nativeWriterDefinition prefix]
      capture <- captureStatus
      after <- lowerStatements suppressed rest
      pure (native <> [capture] <> after)
    Nothing -> do
      before <- lowerStatement suppressed firstStatement
      after <- lowerStatements suppressed remaining
      pure (before <> after)

lowerStatement :: Bool -> P.Statement scope -> Materialize scope [FishStatement]
lowerStatement suppressed statement@(P.Statement range _) = do
  previousRange <- gets materialRange
  previous <- gets materialSuppressed
  modify' (\s -> s {materialSuppressed = suppressed, materialRange = range <|> previousRange})
  result <- lowerStatementNode suppressed statement
  modify' (\s -> s {materialSuppressed = previous, materialRange = previousRange})
  pure result

lowerStatementNode :: Bool -> P.Statement scope -> Materialize scope [FishStatement]
lowerStatementNode suppressed (P.Statement _ node) = case node of
  P.Sequence values -> lowerStatements suppressed values
  P.Approximate approximation values -> do
    cfg <- gets materialConfig
    unless
      (allowsApproximation cfg approximation)
      (lift (Left (planDiagnostic "approximation-policy" "The normalized approximation is not selected" :| [])))
    range <- gets materialRange
    let warning =
          MkDiagnostic
            (MkDiagnosticCode ("monk.approximation." <> approximationName approximation))
            PhaseTranslate
            DiagnosticWarning
            Review
            "Readonly assignment is translated without enforcing the readonly attribute"
            range
    modify' (\s -> s {materialDiagnostics = materialDiagnostics s <> [warning]})
    lowerStatements suppressed values
  P.PatternCondition inverted subject patternValue -> do
    subjectEmission <- Words.materializeScalar wordMaterializer subject
    saved <- fresh "pattern_subject"
    patternEmission <- Words.materializePatternParts wordMaterializer patternValue
    when (Pattern.sourceRequiresRuntime patternValue) (needNative NativePattern "Quote-aware conditional byte pattern")
    native <- runtimeName "native"
    capture <- captureStatus
    status <- runtimeName "status"
    zero <- setSourceStatus (ExprLiteral "0")
    one <- setSourceStatus (ExprLiteral "1")
    guardStatements <- errexitGuard suppressed
    pure $ renderEmission $ do
      subjectValue <- subjectEmission
      emit [assign [SetLocal] saved subjectValue]
      parts <- patternEmission
      pure ([Pattern.matchPattern (identifierText native) (scalarVar saved) parts, capture] <> [ifStatements [testEquals (scalarVar status) "0"] [one] [zero] | inverted] <> guardStatements)
  P.NumericCondition operator (P.Literal left) (P.Literal right) -> do
    result <- either (\message -> lift (Left (planDiagnostic "numeric-constant" (decodeUtf8 message) :| []))) pure (Integer.integerValue (encodeUtf8 (Operator.comparisonName operator)) [encodeUtf8 left, encodeUtf8 right])
    status <- setSourceStatus (ExprLiteral (if result == 0 then "1" else "0"))
    guards <- errexitGuard suppressed
    pure (status : guards)
  P.NumericCondition operator left right -> do
    leftEmission <- Words.materializeScalar wordMaterializer left
    leftName <- fresh "numeric_left"
    rightEmission <- Words.materializeScalar wordMaterializer right
    rightName <- fresh "numeric_right"
    let operation = Operator.comparisonArithmetic operator
    result <- arithmeticMaterialization (A.ArithmeticBinary operation (A.ArithmeticVariable (identifierText leftName)) (A.ArithmeticVariable (identifierText rightName))) mempty
    status <- setSourceStatus (Arithmetic.arithmeticStatus result)
    guardStatements <- errexitGuard suppressed
    pure $ renderEmission $ do
      leftValue <- leftEmission
      emit [assign [SetLocal] leftName leftValue]
      rightValue <- rightEmission
      emit [assign [SetLocal] rightName rightValue]
      pure (Arithmetic.arithmeticStatements result <> [status] <> guardStatements)
  P.AssignmentCommand _ values | not (Effects.effectSubstitution (foldMap Effects.statementEffects values)) -> do
    previous <- gets materialAssignment
    modify' (\material -> material {materialAssignment = True})
    body <- lowerStatements suppressed values
    modify' (\material -> material {materialAssignment = previous})
    zero <- setSourceStatus (ExprLiteral "0")
    pure (body <> [zero])
  P.AssignmentCommand maskStatus values -> do
    previous <- gets materialAssignment
    modify' (\s -> s {materialAssignment = True})
    body <- lowerStatements suppressed values
    modify' (\s -> s {materialAssignment = previous})
    marker <- runtimeName "substitution_executed"
    result <- runtimeName "substitution_status"
    zero <- setSourceStatus (ExprLiteral "0")
    substitution <- setSourceStatus (scalarVar result)
    guardStatements <- errexitGuard suppressed
    pure
      ( [assign [] marker (ExprLiteral "0")]
          <> body
          <> [if maskStatus then zero else ifStatements [testEquals (scalarVar marker) "1"] [substitution] [zero]]
          <> guardStatements
      )
  P.Invoke target wordsValue -> renderEmission <$> Invocation.materializeInvokeWords invocationMaterializer suppressed target wordsValue
  P.PrefixedInvoke assignments target wordsValue -> renderEmission <$> Invocation.materializePrefixedInvoke invocationMaterializer suppressed assignments target wordsValue
  P.Redirected redirects statement -> do
    supervised <- gets materialSession
    if supervised then lowerOwnedRedirects suppressed redirects statement else lowerDirectRedirects suppressed redirects statement
  P.Read options target -> lowerRead suppressed options target
  P.PrefixedRead assignments options target -> do
    bindings <- fmap concat $ forM assignments $ \(_, name, scalar) -> do
      valueEmission <- Words.materializeScalar wordMaterializer scalar
      actual <- bindingName name
      pure (renderEmission (fmap (\value -> [assign [SetLocal, SetUnexport, SetUnpath] actual value]) valueEmission))
    body <- lowerRead suppressed options target
    pure [Stmt (Begin (bodyNE (bindings <> body)) [])]
  P.DeclarationCommand declarations -> Words.materializeDeclarations wordMaterializer declarations
  P.Assign storage name scalar -> do
    valueEmission <- Words.materializeScalar wordMaterializer scalar
    target <- bindingName name
    status <- setSourceStatus (ExprLiteral "0")
    grouped <- gets materialAssignment
    prefix <- gets materialPrefix
    pure (renderEmission (fmap (\value -> Binding.writeBinding (Binding.bindingRuntime prefix (identifierText target)) storage value <> [status | not grouped]) valueEmission))
  P.AssignArray storage name values -> Words.materializeArrayWrite wordMaterializer storage name values False
  P.AppendArray storage name values -> Words.materializeArrayWrite wordMaterializer storage name values True
  P.AssignArrayElement storage name index scalar -> do
    valueEmission <- Words.materializeScalar wordMaterializer scalar
    target <- bindingName name
    status <- setSourceStatus (ExprLiteral "0")
    grouped <- gets materialAssignment
    pure (renderEmission (fmap (\value -> arrayWrite storage (identifierText target) (identifierText target <> "[" <> show (index + 1) <> "]") [arg value] <> [status | not grouped]) valueEmission))
  P.Erase name -> do
    target <- bindingName name
    status <- setSourceStatus (ExprLiteral "0")
    prefix <- gets materialPrefix
    pure (Binding.eraseBinding (Binding.bindingRuntime prefix (identifierText target)) <> [status])
  P.SetArguments target wordsValue -> do
    root <- gets materialRoot
    Control.consumeSetArguments root target `seq` pure ()
    argumentEmission <- Region.fieldsEmission <$> Words.materializeWords wordMaterializer wordsValue
    status <- setSourceStatus (ExprLiteral "0")
    pure (renderEmission (fmap (\arguments -> [builtin "set" (arg (ExprLiteral "argv") : map argumentExpression arguments), status]) argumentEmission))
  P.DirectoryOperation operation -> do
    needNative NativeDirectory "Parent directory transition and actual diagnostics"
    prefix <- fresh "directory_"
    runtime <- gets materialPrefix
    status <- runtimeName "status"
    range <- gets materialRange
    guardStatements <- errexitGuard suppressed
    mode <- gets (entryMode . materialConfig)
    supervised <- gets materialSession
    let origin = maybe "<input>" (srcFile . rangeStart) range
        line = maybe "1" (show . srcLine . rangeStart) range
    traps <- gets materialTraps
    incoming <- fresh "directory_incoming"
    let terminate = if traps then [assign [SetGlobal] (compilerIdentifier runtime <> "pending_signal") (ExprLiteral "13"), Traps.exitWithStatusAt runtime (ExprLiteral origin) (scalarVar incoming)] else [Session.request runtime Request.FinishBrokenPipe, builtin "exit" [arg (ExprLiteral "141")]]
    pure ([assign [SetLocal] incoming (scalarVar status) | supervised && traps] <> Directory.directoryStatements supervised mode (identifierText prefix) runtime (identifierText status) origin line operation <> [ifStatements [testEquals (scalarVar status) "141"] terminate [] | supervised] <> guardStatements)
  P.SetOption option enabled -> do
    name <- runtimeName (case option of P.Errexit -> "errexit"; P.Pipefail -> "pipefail")
    status <- setSourceStatus (ExprLiteral "0")
    pure [assign [] name (ExprLiteral (if enabled then "1" else "0")), status]
  P.And left right -> lowerConjunction True left right
  P.Or left right -> lowerConjunction False left right
  P.Negate statement -> do
    body <- lowerStatement True statement
    status <- runtimeName "status"
    zero <- setSourceStatus (ExprLiteral "0")
    one <- setSourceStatus (ExprLiteral "1")
    pure (body <> [ifStatements [testEquals (scalarVar status) "0"] [one] [zero]])
  P.Conditional predicate yes no -> do
    predicateBody <- lowerStatements True predicate
    yesBody <- lowerStatements suppressed yes
    noBody <- lowerStatements suppressed no
    status <- runtimeName "status"
    zero <- setSourceStatus (ExprLiteral "0")
    pure (predicateBody <> [ifStatements [testEquals (scalarVar status) "0"] (if null yesBody then [zero] else yesBody) (if null noBody then [zero] else noBody)])
  P.ShiftArguments target count -> do
    root <- gets materialRoot
    Control.consumeShift root target `seq` pure ()
    zero <- setSourceStatus (ExprLiteral "0")
    one <- setSourceStatus (ExprLiteral "1")
    guards <- errexitGuard suppressed
    let enough = builtin "test" [arg (ExprQuotedCommandSubst (builtin "count" [arg (ExprVariable (VarAll "argv"))] :| [])), arg (ExprLiteral "-ge"), arg (ExprLiteral (show count))]
        remove = [builtin "set" [arg (ExprLiteral "--erase"), arg (ExprLiteral ("argv[1.." <> show count <> "]"))] | count > 0]
    pure ([ifStatements [enough] (remove <> [zero]) [one]] <> guards)
  P.ArithmeticFor initial loopBody -> do
    failed <- fresh "arithmetic_loop_failed"
    result <- fresh "arithmetic_loop_status"
    initialEmission <- lowerHeader failed initial
    let initialBody = renderEmission (fmap (const []) initialEmission)
    P.withArithmeticBody loopBody $ \root target predicate increment body -> withControlScope root $ do
      predicateEmission <- lowerHeader failed predicate
      incrementEmission <- lowerHeader failed increment
      let incrementBody = renderEmission (fmap (const []) incrementEmission)
      previous <- gets materialLoops
      loopDepth <- gets materialDescriptorDepth
      let incrementActions = incrementBody <> [ifStatements [testEquals (scalarVar failed) "1"] [Stmt Break] []]
      modify' (\material -> material {materialLoops = LoopFrame target result incrementActions loopDepth : previous})
      bodyValue <- lowerStatements suppressed body
      modify' (\material -> material {materialLoops = previous})
      status <- runtimeName "status"
      finalStatus <- setSourceStatus (scalarVar result)
      failedStatus <- setSourceStatus (ExprLiteral "1")
      guards <- errexitGuard suppressed
      let predicateBody = renderEmission $ fmap (\predicateValue -> [ifStatements [testEquals (scalarVar failed) "0"] [builtin "test" [arg predicateValue, arg (ExprLiteral "!="), arg (ExprLiteral "0")]] [builtin "false" []]]) predicateEmission
          loop = Stmt (While (condition predicateBody) (bodyNE (bodyValue <> [assign [] result (scalarVar status)] <> incrementActions)) [])
      pure ([assign [SetLocal] failed (ExprLiteral "0"), assign [SetLocal] result (ExprLiteral "0")] <> initialBody <> [ifStatements [testEquals (scalarVar failed) "0"] [loop] [], ifStatements [testEquals (scalarVar failed) "1"] [failedStatus] [finalStatus]] <> guards)
  P.WhileLoop inverted loopBody -> P.withWhileBody loopBody $ \root target predicate body -> withControlScope root $ do
    result <- fresh "loop_status"
    previous <- gets materialLoops
    loopDepth <- gets materialDescriptorDepth
    modify' (\material -> material {materialLoops = LoopFrame target result [] loopDepth : previous})
    predicateBody <- lowerStatements True predicate
    bodyValue <- lowerStatements suppressed body
    modify' (\material -> material {materialLoops = previous})
    status <- runtimeName "status"
    let check = if inverted then Stmt (Not (Command "test" [arg (scalarVar status), arg (ExprLiteral "="), arg (ExprLiteral "0")])) else testEquals (scalarVar status) "0"
    resultStatus <- setSourceStatus (scalarVar result)
    pure [assign [SetLocal] result (ExprLiteral "0"), Stmt (While (condition (predicateBody <> [check])) (bodyNE (bodyValue <> [assign [] result (scalarVar status)])) []), resultStatus]
  P.ForLoop storage name wordsValue loopBody -> do
    target <- bindingName name
    prefix <- gets materialPrefix
    argumentEmission <- Region.fieldsEmission <$> Words.materializeWords wordMaterializer wordsValue
    P.withForBody loopBody $ \root loopTarget body -> withControlScope root $ do
      result <- fresh "loop_status"
      previous <- gets materialLoops
      loopDepth <- gets materialDescriptorDepth
      modify' (\material -> material {materialLoops = LoopFrame loopTarget result [] loopDepth : previous})
      bodyValue <- lowerStatements suppressed body
      modify' (\material -> material {materialLoops = previous})
      values <- fresh "for_values"
      iteration <- fresh "iteration"
      status <- runtimeName "status"
      resultStatus <- setSourceStatus (scalarVar result)
      pure $
        renderEmission $
          fmap
            ( \arguments ->
                [ builtin "set" (arg (ExprLiteral (identifierText values)) : map argumentExpression arguments),
                  assign [SetLocal] result (ExprLiteral "0"),
                  Stmt (For iteration (ExprVariable (VarAll values)) (bodyNE ([statement | name /= "_", statement <- Binding.writeBinding (Binding.bindingRuntime prefix (identifierText target)) storage (scalarVar iteration)] <> bodyValue <> [assign [] result (scalarVar status)])) []),
                  resultStatus
                ]
            )
            argumentEmission
  P.Case scalar arms -> lowerCase suppressed scalar arms
  P.DefineFunction name body -> do
    needProgram (RequiresFishFeature FunctionScopeSharing) "Bash dynamic function scope"
    bodyValue <- P.withScopedBody body $ \root statements -> withDescriptorRoot root (lowerStatements False statements)
    status <- runtimeName "status"
    zero <- setSourceStatus (ExprLiteral "0")
    cfg <- gets materialConfig
    prefix <- gets materialPrefix
    identityTag <- gets materialIdentity
    helpers <- gets materialHelpers
    wrapper <- fresh "function_entry"
    nativeOperations <- gets (foldMap (\case RequiresNativeRuntime _ _ operations -> operations; _ -> mempty) . M.keys . materialRequirements)
    traps <- gets materialTraps
    let directBody = [statement | traps, statement <- Traps.functionEntry prefix] <> bodyValue <> [Stmt (ReturnScalar (if null bodyValue then ExprLiteral "0" else scalarVar status))]
        framedBody = sourceableEntry cfg nativeOperations prefix identityTag (scalarVar (compilerIdentifier prefix <> "incoming")) (identifierText wrapper) [] (Helpers.definitions helpers) bodyValue
        sharedBody =
          [ ifStatements
              [testEquals (scalarVar (compilerIdentifier prefix <> "active")) identityTag]
              directBody
              (boundaryFailure "incompatible active translation frame")
          ]
        completeBody =
          if entryMode cfg == Sourceable
            then [assign [SetLocal] (compilerIdentifier prefix <> "incoming") (scalarVar "status"), ifStatements [builtin "set" [arg (ExprLiteral "--query"), arg (ExprLiteral (prefix <> "active"))]] sharedBody framedBody]
            else directBody
        definition = Stmt (Function (MkFishFunction name ([FuncUnknownFlag "--no-scope-shadowing"] <> [FuncCaptureVariable (compilerIdentifier (NativeRuntime.runtimePathName prefix)) | entryMode cfg == Sourceable && not (S.null nativeOperations)]) [] (bodyNE completeBody)))
    pure [definition, zero]
  P.SourceBody request body -> do
    needProgram (RequiresFishFeature FunctionScopeSharing) "Owned source return and caller scope"
    wrapper <- fresh "source"
    previousReturn <- gets materialReturnDepth
    deferred <- gets materialDeferCompletion
    currentDepth <- gets materialDescriptorDepth
    modify' (\material -> material {materialReturnDepth = currentDepth, materialDeferCompletion = False})
    bodyValue <- P.withScopedBody body $ \root statements -> withControlScope root (lowerStatements suppressed statements)
    modify' (\material -> material {materialReturnDepth = previousReturn, materialDeferCompletion = deferred})
    status <- runtimeName "status"
    argumentEmission <-
      if null (P.sourceRequestArguments request)
        then pure (pure [SomeArgument (ListArgument (ExprVariable (VarAll "argv")))])
        else Region.fieldsEmission <$> Words.materializeWords wordMaterializer (P.sourceRequestArguments request)
    effective <- fresh "source_arguments"
    let mayInherit = not (null (P.sourceRequestArguments request)) && not (any P.guaranteesField (P.sourceRequestArguments request))
        invocation fields =
          let arguments = map argumentExpression fields
           in if mayInherit
                then
                  [ builtin "set" (map (arg . ExprLiteral) ["--local", "--unexport", "--unpath", "--", identifierText effective] <> arguments),
                    ifStatements
                      [builtin "set" [arg (ExprLiteral "--query"), arg (ExprLiteral (identifierText effective <> "[1]"))]]
                      [Stmt (Command (compilerCommandName (identifierText wrapper)) [arg (ExprVariable (VarAll effective))])]
                      [Stmt (Command (compilerCommandName (identifierText wrapper)) [arg (ExprVariable (VarAll "argv"))])]
                  ]
                else [Stmt (Command (compilerCommandName (identifierText wrapper)) arguments)]
    capture <- captureStatus
    guardStatements <- errexitGuard suppressed
    let result = if null bodyValue then ExprLiteral "0" else scalarVar status
        definition = Stmt (Function (MkFishFunction (identifierText wrapper) [FuncUnknownFlag "--no-scope-shadowing"] [] (bodyNE (bodyValue <> [Stmt (ReturnScalar result)]))))
    separate <- gets materialSeparate
    when separate $ modify' (\material -> material {materialModules = M.insert (toString (identifierText wrapper <> ".fish")) definition (materialModules material)})
    pure $ renderEmission $ fmap (\arguments -> [definition | not separate] <> invocation arguments <> [capture] <> [builtin "functions" [arg (ExprLiteral "--erase"), arg (ExprLiteral (identifierText wrapper))] | not separate] <> guardStatements) argumentEmission
  P.Subshell region -> do
    invocation <- childMaterialization Child.IsolatedChild region
    status <- captureStatus
    guardStatements <- errexitGuard suppressed
    pure (Child.childPrelude invocation <> [Child.childCommand invocation, status] <> childCleanup invocation <> guardStatements)
  P.Pipeline regions -> do
    supervised <- gets materialSession
    if supervised
      then lowerSessionPipeline suppressed RunPipeline regions
      else do
        invocations <- traverse (childMaterialization Child.IsolatedChild) regions
        prefix <- fresh "pipeline"
        pipefail <- runtimeName "pipefail"
        let result = Child.materializePipeline (identifierText prefix) (scalarVar pipefail) invocations
        traverse_ mergeRequirement (Child.pipelineRequirements result)
        status <- setSourceStatus (Child.pipelineStatus result)
        guardStatements <- errexitGuard suppressed
        pure (Child.pipelineStatements result <> [status] <> concatMap childCleanup invocations <> guardStatements)
  P.SupervisedPipeline regions -> lowerSessionPipeline suppressed RunPipeline regions
  P.Background region -> P.withScopedBody (P.childBody region) $ \_ statements ->
    case concatMap flattenStatements statements of
      [P.Statement _ (P.Pipeline regions)] -> lowerSessionPipeline True SpawnPipeline regions
      [P.Statement _ (P.SupervisedPipeline regions)] -> lowerSessionPipeline True SpawnPipeline regions
      _ -> do
        stage <- sessionStage region
        emitter <- projectedSessionRequest
        captured <- captureStatus
        pure $ renderEmission $ do
          (Request.SomeStage body, cleanup) <- stage
          pure ([emitSessionRequest emitter (Request.Spawn (Request.singleBody body)), captured] <> cleanup)
  P.Wait wordsValue -> do
    arguments <- Region.fieldsEmission <$> Words.materializeWords wordMaterializer wordsValue
    prefix <- gets materialPrefix
    (origin, line) <- diagnosticOrigin
    captured <- captureStatus
    guards <- errexitGuard suppressed
    pure $ renderEmission $ fmap (\fields -> [Session.request prefix (Request.Wait (Request.Site (ExprLiteral origin) (ExprLiteral line)) fields), captured] <> guards) arguments
  P.SetTrap kind handler -> do
    prefix <- gets materialPrefix
    zero <- setSourceStatus (ExprLiteral "0")
    case handler of
      Nothing -> pure (Traps.install prefix kind Nothing <> [zero])
      Just statements -> do
        name <- fresh "trap_body"
        previousCallback <- gets materialCallback
        modify' (\material -> material {materialCallback = True})
        body <- P.withScopedBody statements $ \root bodyStatements -> withDescriptorRoot root (lowerStatements False bodyStatements)
        modify' (\material -> material {materialCallback = previousCallback})
        status <- runtimeName "status"
        let definition = Stmt (Function (MkFishFunction (identifierText name) [FuncUnknownFlag "--no-scope-shadowing"] [] (bodyNE (body <> [Stmt (ReturnScalar (scalarVar status))]))))
        pure ([definition] <> Traps.install prefix kind (Just (identifierText name)) <> [zero])
  P.ArithmeticCommand site expression bindings -> do
    result <- arithmeticMaterialization expression bindings
    success <- setSourceStatus (Arithmetic.arithmeticStatus result)
    failure <- setSourceStatus (ExprLiteral "1")
    guardStatements <- errexitGuard suppressed
    pure
      ( Arithmetic.arithmeticStatements result
          <> [ ifStatements
                 [testEquals (Arithmetic.arithmeticError result) ""]
                 [success]
                 (arithmeticDiagnostic True site result <> [failure])
             ]
          <> guardStatements
      )
  P.Return target value -> do
    root <- gets materialRoot
    Control.consumeReturn root target `seq` lowerExit "return" value
  P.Exit value -> lowerExit "exit" value
  P.Break target -> lowerLoopJump target [] (Stmt Break)
  P.Continue target -> do
    actions <- gets (maybe [] loopContinue . viaNonEmpty head . materialLoops)
    lowerLoopJump target actions (Stmt Continue)
  where
    lowerConjunction onSuccess left right = do
      first <- lowerStatement True left
      second <- lowerStatement suppressed right
      status <- runtimeName "status"
      let branch = if onSuccess then ifStatements [testEquals (scalarVar status) "0"] second [] else ifStatements [testEquals (scalarVar status) "0"] [] second
      pure (first <> [branch])
    lowerExit name value = do
      resultEmission <- maybe (pure . scalarVar <$> runtimeName "status") (Words.materializeScalar wordMaterializer) value
      remaining <- gets materialReturnDepth
      unwind <- if name == "return" then unwindDescriptors remaining else pure []
      traps <- gets materialTraps
      prefix <- gets materialPrefix
      origin <- diagnosticOriginExpression
      pure (renderEmission (fmap (\result -> unwind <> [if traps && name == "exit" then Traps.exitWithStatusAt prefix origin result else if name == "return" then Stmt (ReturnScalar result) else builtin name [arg result]]) resultEmission))

lowerOwnedRedirects :: Bool -> [P.Redirection] -> P.Statement scope -> Materialize scope [FishStatement]
lowerOwnedRedirects suppressed redirects statement = do
  prefix <- gets materialPrefix
  depth <- gets materialDescriptorDepth
  failed <- fresh "redirect_status"
  operations <- traverse lowerOwnedRedirect redirects
  previousDefer <- gets materialDeferCompletion
  let simple = case statement of P.Statement _ (P.Sequence _) -> False; P.Statement _ (P.Conditional {}) -> False; P.Statement _ (P.WhileLoop {}) -> False; P.Statement _ (P.ForLoop {}) -> False; P.Statement _ (P.ArithmeticFor {}) -> False; P.Statement _ (P.Case {}) -> False; P.Statement _ (P.And {}) -> False; P.Statement _ (P.Or {}) -> False; _ -> True
  modify' (\material -> material {materialDescriptorDepth = depth + 1, materialDeferCompletion = previousDefer || simple})
  invocation <- case statement of
    P.Statement _ (P.Invoke target wordsValue) -> Invocation.materializeInvokeWords invocationMaterializer suppressed target wordsValue
    P.Statement _ (P.PrefixedInvoke assignments target wordsValue) -> Invocation.materializePrefixedInvoke invocationMaterializer suppressed assignments target wordsValue
    _ -> pure <$> lowerStatement suppressed statement
  modify' (\material -> material {materialDescriptorDepth = depth, materialDeferCompletion = previousDefer})
  failureStatus <- setSourceStatus (scalarVar failed)
  guards <- errexitGuard suppressed
  let apply statements = ifStatements [testEquals (scalarVar failed) "0"] (statements <> [assign [] failed (scalarVar "status")]) []
  pure (renderEmission (fmap (\body -> [Session.request prefix Request.FdPush, assign [SetLocal] failed (scalarVar "status")] <> map apply operations <> [ifStatements [testEquals (scalarVar failed) "0"] (body <> [Session.request prefix Request.FdPop] <> [guardStatement | simple, guardStatement <- guards]) ([failureStatus, Session.request prefix Request.FdPop] <> guards)]) invocation))

lowerOwnedRedirect :: P.Redirection -> Materialize scope [FishStatement]
lowerOwnedRedirect redirect = do
  prefix <- gets materialPrefix
  site <- diagnosticSite
  let open fd mode path = do
        value <- Words.materializeScalar wordMaterializer path
        pure (renderEmission (fmap (\expression -> [Session.request prefix (Request.FdOpen site (fromIntegral fd) mode expression)]) value))
  case redirect of
    P.OpenDescriptor fd _ endpoint@P.ProcessSubstitution {} -> do
      value <- Words.materializeScalar wordMaterializer endpoint
      pure (renderEmission (fmap (const [Session.request prefix (Request.FdEndpoint site (fromIntegral fd) (Session.endpointLease prefix))]) value))
    P.OpenDescriptor fd mode path -> open fd (case mode of P.ReadFile -> Request.ReadFile; P.WriteFile -> Request.WriteFile; P.AppendFile -> Request.AppendFile; P.ReadWriteFile -> Request.ReadWriteFile) path
    P.NullDescriptor fd input -> open fd (if input then Request.ReadFile else Request.WriteFile) (P.Literal "/dev/null")
    P.DuplicateDescriptor target source _ -> pure [Session.request prefix (Request.FdDup site (fromIntegral target) (fromIntegral source))]
    P.CloseDescriptor fd _ -> pure [Session.request prefix (Request.FdClose (fromIntegral fd))]
    P.InputDescriptor fd scalar newline -> do
      value <- Words.materializeScalar wordMaterializer scalar
      pure (renderEmission (fmap (\expression -> [Session.request prefix (Request.FdData (fromIntegral fd) (if newline then ExprStringConcat expression (ExprLiteral "\n") else expression))]) value))

lowerRead :: Bool -> P.ReadOptions -> P.ReadTarget -> Materialize scope [FishStatement]
lowerRead suppressed options target = do
  prefix <- gets materialPrefix
  ifs <- runtimeName "ifs"
  site <- diagnosticSite
  captured <- captureStatus
  guards <- errexitGuard suppressed
  destination <- case target of
    P.ReadReply _ -> pure Request.ReplyVariable
    P.ReadScalars names -> maybe (lift (Left (planDiagnostic "read-target" "A scalar read needs at least one destination" :| []))) (pure . Request.ScalarVariables . fmap snd) (NE.nonEmpty names)
    P.ReadArray {} -> pure Request.ArrayVariable
  let request = Request.Read site (fromIntegral (P.readDescriptor options)) (P.readRaw options) (P.readDelimiter options) (fromIntegral <$> P.readCount options) (scalarVar ifs) destination
      value index = ExprQuotedVariable (VarIndex (compilerIdentifier prefix <> "session_fields") (IndexSingle (ExprNumLiteral (index + 1))))
      write (index, (storage, name)) = do
        actual <- bindingName name
        pure (Binding.writeBinding (Binding.bindingRuntime prefix (identifierText actual)) storage (value index))
  assignments <- case target of
    P.ReadReply storage -> write (1, (storage, "REPLY"))
    P.ReadScalars names -> concat <$> traverse write (zip [1 ..] names)
    P.ReadArray storage name -> do
      actual <- bindingName name
      pure (arrayWrite storage (identifierText actual) (identifierText actual) [arg (Request.replyExpression request prefix)])
  pure ([Session.request prefix request, captured, ifStatements [testEquals (value 0) "1"] assignments []] <> guards)

lowerDirectRedirects :: Bool -> [P.Redirection] -> P.Statement scope -> Materialize scope [FishStatement]
lowerDirectRedirects suppressed redirects statement = do
  needProgram (RequiresPlatformCapability PosixOwnedDescriptors) "Standard descriptor and stable null device operations"
  invocation <- case statement of
    P.Statement _ (P.Invoke target wordsValue) -> Invocation.materializeInvokeWords invocationMaterializer suppressed target wordsValue
    P.Statement _ (P.PrefixedInvoke assignments target wordsValue) -> Invocation.materializePrefixedInvoke invocationMaterializer suppressed assignments target wordsValue
    P.Statement _ P.DeclarationCommand {} -> lift (Left (planDiagnostic "redirect-declaration" "Redirected declarations require their own expansion and local-slot scope" :| []))
    _ -> pure <$> lowerStatement suppressed statement
  lowered <- either (\message -> lift (Left (planDiagnostic "redirect-materialization" message :| []))) pure (traverse lowerRedirect redirects)
  pure (renderEmission (fmap (\body -> [Stmt (Begin (bodyNE body) lowered)]) invocation))

lowerHeader :: Identifier -> P.Statement scope -> Materialize scope (Emission (FishExpr TStr))
lowerHeader failed (P.Statement _ (P.ArithmeticCommand site expression bindings)) = do
  result <- arithmeticMaterialization expression bindings
  pure (emit (Arithmetic.arithmeticStatements result <> [ifStatements [testEquals (Arithmetic.arithmeticError result) ""] [] (arithmeticDiagnostic True site result <> [assign [] failed (ExprLiteral "1")])]) >> pure (Arithmetic.arithmeticValue result))
lowerHeader _ _ = lift (Left (planDiagnostic "arithmetic-header" "Arithmetic loop header must own an integer operation" :| []))

lowerLoopJump :: Control.LoopTarget scope -> [FishStatement] -> FishStatement -> Materialize scope [FishStatement]
lowerLoopJump target actions jump = do
  root <- gets materialRoot
  let key = Control.consumeLoop root target
  found <- gets (find (\candidate -> Control.consumeLoop root (loopTarget candidate) == key) . materialLoops)
  frame <- maybe (lift (Left (planDiagnostic "loop-control" "Loop control has no owned materialization target" :| []))) pure found
  zero <- setSourceStatus (ExprLiteral "0")
  unwind <- unwindDescriptors (loopDescriptorDepth frame)
  pure (unwind <> actions <> [zero, assign [] (loopResult frame) (ExprLiteral "0"), jump])

lowerRedirect :: P.Redirection -> Either Text Redirect
lowerRedirect = \case
  P.DuplicateDescriptor source target input -> pure (DuplicateRedirect (fromIntegral source) (mode input) (fromIntegral target))
  P.CloseDescriptor source input -> pure (CloseRedirect (fromIntegral source) (mode input))
  P.NullDescriptor source input -> pure (FileRedirect (fromIntegral source) (if input then InputFile else OverwriteFile) (ExprLiteral "/dev/null"))
  P.OpenDescriptor {} -> Left "File opens require an owned descriptor session"
  P.InputDescriptor {} -> Left "Input data requires an owned descriptor session"
  where
    mode input = if input then ReadFrom else WriteTo

-- Dependencies are allocated Command identities in the structural DSL, never
-- words searched inside rendered helper or child source text.
helperClosure :: [FishStatement] -> [FishStatement] -> [FishStatement]
helperClosure available roots =
  let byName = M.fromList [(name, definition) | definition <- available, name <- helperNames [definition]]
      close retained pending = case S.minView pending of
        Nothing -> retained
        Just (name, rest)
          | S.member name retained -> close retained rest
          | otherwise -> case M.lookup name byName of
              Nothing -> close retained rest
              Just definition -> close (S.insert name retained) (rest <> commandReferences [definition])
      names = close mempty (commandReferences roots)
   in [definition | definition <- available, any (`S.member` names) (helperNames [definition])]

childCleanup :: Child.ChildInvocation -> [FishStatement]
childCleanup invocation = case helperNames (Child.childPrelude invocation) of
  [] -> []
  names -> [builtin "functions" (map (arg . ExprLiteral) ("--erase" : names))]

childMaterialization :: Child.ChildMode -> P.ChildRegion -> Materialize scope Child.ChildInvocation
childMaterialization mode region = do
  needNative NativeChildRun "Owned child launch"
  prefix <- fresh "child"
  runtimePrefix <- gets materialPrefix
  suppressed <- gets materialSuppressed
  supervised <- gets materialSession
  traps <- gets materialTraps
  previousHelpers <- gets materialHelpers
  definitions <- fmap concat $ forM (M.toAscList (P.childFunctions region)) $ \(name, body) ->
    lowerStatement False (P.Statement (P.childRange region) (P.DefineFunction name body))
  bodyValue <- P.withScopedBody (P.childBody region) $ \root statements -> withDescriptorRoot root (lowerStatements suppressed statements)
  let body = [assign [SetGlobal] (compilerIdentifier runtimePrefix <> "source_origin") (ExprLiteral (maybe "<input>" (srcFile . rangeStart) (P.childRange region))) | traps] <> bodyValue
  helpers <- gets materialHelpers
  modify' (\s -> s {materialHelpers = previousHelpers})
  ownedBindings <- gets materialBindings
  actualBindings <- S.fromList . map identifierText <$> traverse bindingName (S.toAscList (P.childVariables region <> (if P.childNeedsEnvironment region then ownedBindings else mempty)))
  ownerPrefix <- gets materialPrefix
  actualArrays <- S.fromList . map identifierText <$> traverse bindingName (S.toAscList (P.childArrays region))
  let bindings = (actualBindings S.\\ actualArrays) <> foldMap (Binding.bindingRuntimeNames . Binding.bindingRuntime ownerPrefix) actualBindings
  let runtime =
        Child.MkChildRuntime
          runtimePrefix
          (runtimePrefix <> "status")
          (runtimePrefix <> "errexit")
          (runtimePrefix <> "suppress")
          (S.fromList ([runtimePrefix <> role | role <- ["status", "errexit", "pipefail", "suppress", "ifs", "active", "substitution_executed", "substitution_status", "native_path", "last_pid"]] <> [runtimePrefix <> "source_origin" | traps]))
          (NativeRuntime.runtimeHelperName runtimePrefix)
          suppressed
          supervised
          traps
  result <-
    either
      (\message -> lift (Left (planDiagnostic "child-materialization" message :| [])))
      pure
      (Child.materializeChild (identifierText prefix) mode runtime bindings actualArrays (helperClosure (Helpers.definitions helpers) (body <> definitions <> [Traps.exitWithStatus runtimePrefix (scalarVar (compilerIdentifier runtimePrefix <> "status")) | traps]) <> definitions) body)
  traverse_ mergeRequirement (Child.childRequirements result)
  needProgram (RequiresFishFeature FunctionScopeSharing) "Owned child body and dynamic function closure"
  pure result

lowerCase :: Bool -> P.Scalar -> [P.CaseArm scope] -> Materialize scope [FishStatement]
lowerCase suppressed scalar arms = do
  valueEmission <- Words.materializeScalar wordMaterializer scalar
  input <- fresh "case_value"
  mode <- fresh "case_mode"
  result <- fresh "case_status"
  status <- runtimeName "status"
  armBodies <- forM arms $ \(P.CaseArm patterns body ending) -> do
    matches <- Words.materializePatterns wordMaterializer input patterns
    bodyValue <- lowerStatements suppressed body
    let afterMode = case ending of P.StopCase -> "2"; P.FallThrough -> "1"; P.Retest -> "0"
        execute = bodyValue <> [assign [] result (if null bodyValue then ExprLiteral "0" else scalarVar status), assign [] mode (ExprLiteral afterMode)]
        attempt = ifStatements [testEquals (scalarVar mode) "1"] execute [ifStatements matches execute []]
    pure (ifStatements [testEquals (scalarVar mode) "2"] [] [attempt])
  resultStatus <- setSourceStatus (scalarVar result)
  pure (renderEmission (fmap (\value -> [assign [SetLocal] input value, assign [SetLocal] mode (ExprLiteral "0"), assign [SetLocal] result (ExprLiteral "0")] <> armBodies <> [resultStatus]) valueEmission))

-- The recursive knot contains only child/session statement construction;
-- operand sequencing and invocation ownership stay in their dedicated modules.
wordMaterializer :: Words.WordMaterializer scope
wordMaterializer = Words.wordMaterializer (Words.Recursion childMaterialization sessionStage projectedSessionRequest childCleanup)

invocationMaterializer :: Invocation.InvocationMaterializer scope
invocationMaterializer = Invocation.invocationMaterializer wordMaterializer

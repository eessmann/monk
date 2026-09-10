{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Language.Fish.Translator.Plan
  ( PlannedTranslation,
    compilePlannedTranslation,
    compilePlannedDocument,
    compileSourcePlan,
    PlannedBundle,
    compileSourceBundle,
    compileBundleLoader,
    plannedBundleEntry,
    plannedBundleModules,
    plannedBundleModuleStatistics,
    plannedScript,
    plannedDiagnostics,
    plannedRequirements,
    plannedStatistics,
  )
where

import Control.Monad.State.Strict (gets)
import Data.ByteString qualified as BS
import Data.List (lookup)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Language.Bash.Arithmetic.Plan (ArithmeticExpr)
import Language.Bash.Arithmetic.Plan qualified as A
import Language.Bash.Plan qualified as P
import Language.Bash.Plan.Effects qualified as Effects
import Language.Bash.Plan.Normalize (normalizeDocument, normalizeSource)
import Language.Fish.DSL.Internal
import Language.Fish.Translator.ArithmeticDiagnostic (arithmeticDiagnostic)
import Language.Fish.Translator.ArithmeticPlan qualified as Arithmetic
import Language.Fish.Translator.Binding qualified as Binding
import Language.Fish.Translator.Child qualified as Child
import Language.Fish.Translator.Directory qualified as Directory
import Language.Fish.Translator.NativeRuntime qualified as NativeRuntime
import Language.Fish.Translator.Pattern qualified as Pattern
import Language.Fish.Translator.Statistics (commandReferences, materializationStatistics)
import Monk.Runtime.Integer qualified as Integer
import Monk.Translation.Types
import Numeric (showHex, showOct)
import ShellCheck.Interface (ParseResult)
import System.FilePath (isAbsolute)
import Prelude hiding (exitFailure, first, force, gets, isPrefixOf, one, second)

data PlannedTranslation = MkPlannedTranslation
  { plannedScript :: Script,
    plannedDiagnostics :: [Diagnostic],
    plannedRequirements :: [RuntimeRequirement],
    plannedStatistics :: TranslationStatistics
  }
  deriving stock (Show, Eq)

data PlannedBundle = MkPlannedBundle
  { plannedBundleEntry :: PlannedTranslation,
    plannedBundleModules :: M.Map FilePath Script,
    plannedBundleModuleStatistics :: M.Map FilePath TranslationStatistics
  }
  deriving stock (Show, Eq)

data Materialization = MkMaterialization
  { materialPrefix :: Text,
    materialNext :: Int,
    materialRequirements :: M.Map RuntimeProgram (NonEmpty RequirementUse),
    materialHelpers :: [FishStatement],
    materialConfig :: TranslateConfig,
    materialIdentity :: Text,
    materialSuppressed :: Bool,
    materialAssignment :: Bool,
    materialRange :: Maybe SourceRange,
    materialDiagnostics :: [Diagnostic],
    materialBindings :: S.Set Text,
    materialSeparate :: Bool,
    materialModules :: M.Map FilePath FishStatement,
    materialLoops :: [Text],
    materialContinueActions :: [[FishStatement]],
    materialErrexitRelevant :: Bool
  }

type Materialize = StateT Materialization (Either (NonEmpty Diagnostic))

compilePlannedTranslation :: TranslateConfig -> ParseResult -> Either (NonEmpty Diagnostic) PlannedTranslation
compilePlannedTranslation cfg parsed = normalizeSource cfg parsed >>= compileSourcePlan

compilePlannedDocument :: TranslateConfig -> Text -> ParseResult -> Either (NonEmpty Diagnostic) PlannedTranslation
compilePlannedDocument cfg source parsed = normalizeDocument cfg source parsed >>= compileSourcePlan

compileSourcePlan :: P.SourcePlan -> Either (NonEmpty Diagnostic) PlannedTranslation
compileSourcePlan plan = plannedBundleEntry <$> compileMaterialization False plan

compileSourceBundle :: P.SourcePlan -> Either (NonEmpty Diagnostic) PlannedBundle
compileSourceBundle = compileMaterialization True

-- The publication planner supplies the absolute immutable generation entry.
-- This constructor owns the complete loader; no wrapper is appended to an
-- already admitted script by output planning.
compileBundleLoader :: FilePath -> PlannedBundle -> Either (NonEmpty Diagnostic) PlannedTranslation
compileBundleLoader entry bundle = do
  unless
    (isAbsolute entry && not (T.any (== '\0') (toText entry)))
    (Left (planDiagnostic "bundle-loader-path" "A bundle loader needs an absolute NUL-free generation entry" :| []))
  let admitted = plannedBundleEntry bundle
      script = MkScript [builtin "source" [arg (ExprLiteral (toText entry)), arg (ExprVariable (VarAll "argv"))]]
  pure (MkPlannedTranslation script (plannedDiagnostics admitted) (plannedRequirements admitted) (materializationStatistics "" "" script))

compileMaterialization :: Bool -> P.SourcePlan -> Either (NonEmpty Diagnostic) PlannedBundle
compileMaterialization separate (P.SourcePlan cfg statements reserved) = do
  let prefix = choosePrefix reserved 0
      identityTag = show (cfg, statements)
      effects = foldMap Effects.statementEffects statements
      bindings = S.delete "#" (Effects.effectReads effects <> Effects.effectWrites effects <> M.keysSet (callerVariables (callerContract cfg)))
      initial = MkMaterialization prefix 0 mempty [] cfg identityTag False False Nothing [] bindings separate mempty [] [] (Effects.effectMayEnableErrexit effects)
  (body, final) <-
    runStateT
      ( do
          needProgram (RequiresFishFeature Fish46) "Structural Fish execution profile"
          when (stableDirectoryEnabled cfg && not (S.null (bindings `S.intersection` S.fromList ["PWD", "OLDPWD", "dirstack"]))) $
            needNative NativeDirectory "Directory binding boundary obligations"
          when (entryMode cfg == Sourceable) $ do
            needProgram (RequiresCommand "fish") "Caller boundary failure status restoration"
            needProgram (RequiresFishFeature FunctionScopeSharing) "Sourceable caller frame"
          lowerStatements False statements
      )
      initial
  let statusName = prefix <> "status"
      initialization =
        [assign [SetGlobal] statusName (ExprLiteral "0")]
          <> [assign [SetGlobal] (prefix <> role) (ExprLiteral "0") | Effects.effectMayEnableErrexit effects, role <- ["errexit", "suppress"]]
          <> [assign [SetGlobal] (prefix <> "pipefail") (ExprLiteral "0") | Effects.effectPipefail effects]
          <> [assign [SetGlobal] (prefix <> "ifs") (ExprLiteral " \t\n") | S.member "IFS" bindings]
          <> [assign [SetGlobal] (prefix <> role) (ExprLiteral "0") | Effects.effectSubstitution effects, role <- ["substitution_executed", "substitution_status"]]
      moduleDefinitions = M.elems (materialModules final)
      moduleFunctions = helperNames moduleDefinitions
      modules = fmap (MkScript . (: [])) (materialModules final)
      moduleRoot = prefix <> "module_root"
      loaders =
        [assign [SetLocal] moduleRoot (ExprQuotedCommandSubst (builtin "status" [arg (ExprLiteral "dirname")] :| [])) | not (M.null modules)]
          <> concatMap (loadModule (entryMode cfg == Sourceable) moduleRoot) (M.keys modules)
      helpers = materialHelpers final <> loaders
      nativeOperations = foldMap (\case RequiresNativeRuntime _ _ operations -> operations; _ -> mempty) (M.keys (materialRequirements final))
      runtimeSetup = NativeRuntime.nativeRuntimeSetup cfg prefix nativeOperations
      complete =
        if entryMode cfg == Sourceable
          then [asCommand (sourceableEntry cfg nativeOperations prefix identityTag (scalarVar "status") (prefix <> "entry") moduleFunctions helpers body)]
          else standaloneGuards prefix bindings (runtimeSetup <> initialization <> helpers <> (if S.member NativeDirectory nativeOperations then Directory.directorySetup cfg prefix else []) <> body <> [builtin "exit" [arg (scalarVar statusName)]])
      statistics = materializationStatistics prefix (NativeRuntime.runtimeHelperName prefix) (MkScript complete)
      requirements = [MkRuntimeRequirement program uses | (program, uses) <- M.toAscList (materialRequirements final)]
  statistics `seq` pure ()
  forM_ (M.keys (materialRequirements final)) $ \case
    RequiresFishFeature feature ->
      unless
        (profileSupportsFishFeature (targetProfile cfg) feature)
        (Left (planDiagnostic "target-capability" ("Target profile does not support " <> fishFeatureName feature) :| []))
    RequiresPlatformCapability capability ->
      unless
        (profileSupportsPlatformCapability (targetProfile cfg) capability)
        (Left (planDiagnostic "target-capability" ("Target profile does not support " <> platformCapabilityName capability) :| []))
    RequiresCommand _ -> pure ()
    RequiresNativeRuntime abi profile _ -> unless (abi == 1 && profile == targetProfile cfg) (Left (planDiagnostic "native-runtime-capability" "Native runtime ABI/profile is incompatible" :| []))
  -- The owned complete script includes every inserted initialization and final
  -- control operation; no subsequent pass changes its semantics.
  pure (MkPlannedBundle (MkPlannedTranslation (MkScript complete) (materialDiagnostics final) requirements statistics) modules (fmap (materializationStatistics prefix (NativeRuntime.runtimeHelperName prefix)) modules))

loadModule :: Bool -> Text -> FilePath -> [FishStatement]
loadModule sourceable root path =
  [ builtin "source" [arg (ExprStringConcat (scalarVar root) (ExprLiteral ("/" <> toText path)))],
    ifStatements [testEquals (scalarVar "status") "0"] [] [builtin (if sourceable then "return" else "exit") [arg (ExprLiteral "125")]]
  ]

planDiagnostic :: Text -> Text -> Diagnostic
planDiagnostic code message = MkDiagnostic (MkDiagnosticCode ("monk.semantic." <> code)) PhaseTranslate DiagnosticError Unsafe message Nothing

choosePrefix :: S.Set Text -> Int -> Text
choosePrefix names index =
  let prefix = "__monk_plan_" <> show index <> "_"
   in if any (prefix `isPrefixOf`) (S.toList names) then choosePrefix names (index + 1) else prefix
  where
    isPrefixOf prefix value = take (length (toString prefix)) (toString value) == toString prefix

fresh :: Text -> Materialize Text
fresh role = do
  prefix <- gets materialPrefix
  index <- gets materialNext
  modify' (\s -> s {materialNext = index + 1})
  pure (prefix <> role <> "_" <> show index)

runtimeName :: Text -> Materialize Text
runtimeName role = (<> role) <$> gets materialPrefix

bindingName :: Text -> Materialize Text
bindingName "IFS" = runtimeName "ifs"
bindingName name = pure name

needProgram :: RuntimeProgram -> Text -> Materialize ()
needProgram program reason = do
  range <- gets materialRange
  mergeRequirement (MkRuntimeRequirement program (MkRequirementUse reason range :| []))

mergeRequirement :: RuntimeRequirement -> Materialize ()
mergeRequirement (MkRuntimeRequirement program uses) = do
  range <- gets materialRange
  let located use = use {requirementRange = requirementRange use <|> range}
  modify' (\s -> s {materialRequirements = M.insertWith (flip (<>)) program (fmap located uses) (materialRequirements s)})

needNative :: NativeOperation -> Text -> Materialize ()
needNative operation reason = do
  mergeRequirement (nativeRuntimeRequirement operation reason)
  needProgram (RequiresFishFeature FunctionScopeSharing) "Private native runtime environment isolation"
  needProgram (RequiresFishFeature NulDelimitedCapture) "Native provider pathname byte preservation"
  name <- runtimeName "native"
  helpers <- gets materialHelpers
  unless (name `elem` helperNames helpers) $ do
    bindings <- gets materialBindings
    prefix <- gets materialPrefix
    modify' (\s -> s {materialHelpers = materialHelpers s <> [NativeRuntime.nativeRuntimeDefinition prefix bindings]})

arg :: (Typeable t) => FishExpr t -> ExprOrRedirect
arg = ExprVal

scalarVar :: Text -> FishExpr TStr
scalarVar = ExprQuotedVariable . VarScalar

builtin :: Text -> [ExprOrRedirect] -> FishStatement
builtin name arguments = Stmt (Decorated DecBuiltin (Command name arguments))

external :: Text -> [ExprOrRedirect] -> FishStatement
external name arguments = Stmt (Decorated DecCommand (Command name arguments))

assign :: [SetFlag] -> Text -> FishExpr TStr -> FishStatement
assign flags name value = Stmt (Decorated DecBuiltin (Set flags name (ExprListLiteral [value])))

assignList :: [SetFlag] -> Text -> FishExpr (TList TStr) -> FishStatement
assignList flags name value = Stmt (Decorated DecBuiltin (Set flags name value))

bodyNE :: [FishStatement] -> NonEmpty FishStatement
bodyNE = fromMaybe (builtin "true" [] :| []) . NE.nonEmpty

jobOf :: FishStatement -> FishJobPipeline
jobOf statement = MkFishJobPipeline False [] statement [] False

condition :: [FishStatement] -> FishJobList
condition statements = MkFishJobList (MkFishJobConjunction Nothing (jobOf (asCommand statements)) [] :| [])

asCommand :: [FishStatement] -> FishStatement
asCommand [statement] = statement
asCommand statements = Stmt (Begin (bodyNE statements) [])

ifStatements :: [FishStatement] -> [FishStatement] -> [FishStatement] -> FishStatement
ifStatements predicate yes no = Stmt (If (condition predicate) (bodyNE yes) no [])

testEquals :: FishExpr TStr -> Text -> FishStatement
testEquals value expected = builtin "test" [arg value, arg (ExprLiteral "="), arg (ExprLiteral expected)]

setSourceStatus :: FishExpr TStr -> Materialize FishStatement
setSourceStatus value = do
  name <- runtimeName "status"
  pure (assign [] name value)

captureStatus :: Materialize FishStatement
captureStatus = setSourceStatus (scalarVar "status")

errexitGuard :: Bool -> Materialize [FishStatement]
errexitGuard suppressed
  | suppressed = pure []
  | otherwise = do
      relevant <- gets materialErrexitRelevant
      if relevant then materializeGuard else pure []
  where
    materializeGuard = do
      enabled <- runtimeName "errexit"
      suppression <- runtimeName "suppress"
      status <- runtimeName "status"
      let exitFailure = ifStatements [testEquals (scalarVar status) "0"] [] [builtin "exit" [arg (scalarVar status)]]
      pure [ifStatements [testEquals (scalarVar enabled) "1"] [ifStatements [testEquals (scalarVar suppression) "0"] [exitFailure] []] []]

lowerStatements :: Bool -> [P.Statement] -> Materialize [FishStatement]
lowerStatements suppressed = fmap concat . traverse (lowerStatement suppressed)

lowerStatement :: Bool -> P.Statement -> Materialize [FishStatement]
lowerStatement suppressed statement@(P.Statement range _) = do
  previousRange <- gets materialRange
  previous <- gets materialSuppressed
  modify' (\s -> s {materialSuppressed = suppressed, materialRange = range <|> previousRange})
  result <- lowerStatementNode suppressed statement
  modify' (\s -> s {materialSuppressed = previous, materialRange = previousRange})
  pure result

lowerStatementNode :: Bool -> P.Statement -> Materialize [FishStatement]
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
    (subjectPrelude, subjectValue) <- lowerScalar subject
    saved <- fresh "pattern_subject"
    (patternPrelude, parts) <- lowerPatternParts patternValue
    when (Pattern.requiresRuntime parts) (needNative NativePattern "Quote-aware conditional byte pattern")
    native <- runtimeName "native"
    capture <- captureStatus
    status <- runtimeName "status"
    zero <- setSourceStatus (ExprLiteral "0")
    one <- setSourceStatus (ExprLiteral "1")
    guardStatements <- errexitGuard suppressed
    pure
      ( subjectPrelude
          <> [assign [SetLocal] saved subjectValue]
          <> patternPrelude
          <> [Stmt (Pattern.matchPattern native (scalarVar saved) parts), capture]
          <> [ifStatements [testEquals (scalarVar status) "0"] [one] [zero] | inverted]
          <> guardStatements
      )
  P.NumericCondition operator (P.Literal left) (P.Literal right) -> do
    result <- either (\message -> lift (Left (planDiagnostic "numeric-constant" (decodeUtf8 message) :| []))) pure (Integer.integerValue (encodeUtf8 (T.drop 1 operator)) [encodeUtf8 left, encodeUtf8 right])
    status <- setSourceStatus (ExprLiteral (if result == 0 then "1" else "0"))
    guards <- errexitGuard suppressed
    pure (status : guards)
  P.NumericCondition operator left right -> do
    (leftPrelude, leftValue) <- lowerScalar left
    leftName <- fresh "numeric_left"
    (rightPrelude, rightValue) <- lowerScalar right
    rightName <- fresh "numeric_right"
    operation <-
      maybe
        (lift (Left (planDiagnostic "numeric-condition-operator" "Unknown numeric comparison operation" :| [])))
        pure
        (lookup operator [("-eq", A.Equal), ("-ne", A.NotEqual), ("-lt", A.LessThan), ("-le", A.LessEqual), ("-gt", A.GreaterThan), ("-ge", A.GreaterEqual)])
    result <- arithmeticMaterialization (A.ArithmeticBinary operation (A.ArithmeticVariable leftName) (A.ArithmeticVariable rightName)) mempty
    status <- setSourceStatus (Arithmetic.arithmeticStatus result)
    guardStatements <- errexitGuard suppressed
    pure
      ( leftPrelude
          <> [assign [SetLocal] leftName leftValue]
          <> rightPrelude
          <> [assign [SetLocal] rightName rightValue]
          <> Arithmetic.arithmeticStatements result
          <> [status]
          <> guardStatements
      )
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
  P.Invoke (P.Builtin "echo") wordsValue
    | Just values <- traverse literalWord wordsValue,
      Just (newline, output) <- nativeEcho values -> do
        capture <- captureStatus
        guardStatements <- errexitGuard suppressed
        pure ([builtin "printf" [arg (ExprLiteral (if newline then "%s\n" else "%s")), arg (ExprLiteral (T.intercalate " " output))], capture] <> guardStatements)
  P.Invoke target wordsValue -> do
    (prelude, arguments) <- lowerWords wordsValue
    body <- lowerInvocation suppressed target arguments
    pure (prelude <> body)
  P.Redirected redirects statement -> do
    needProgram (RequiresPlatformCapability Linux64DescriptorFilesystem) "Standard descriptor and stable null device operations"
    (prelude, body) <- case statement of
      P.Statement _ (P.Invoke target wordsValue) -> do
        (expansions, arguments) <- lowerWords wordsValue
        invocation <- lowerInvocation suppressed target arguments
        pure (expansions, invocation)
      P.Statement _ P.DeclarationCommand {} -> lift (Left (planDiagnostic "redirect-declaration" "Redirected declarations require their own expansion and local-slot scope" :| []))
      _ -> ([],) <$> lowerStatement suppressed statement
    pure (prelude <> [Stmt (Begin (bodyNE body) (map (RedirectVal . lowerRedirect) redirects))])
  P.DeclarationCommand declarations -> lowerDeclarations declarations
  P.Assign storage name scalar -> do
    (prelude, value) <- lowerScalar scalar
    target <- bindingName name
    status <- setSourceStatus (ExprLiteral "0")
    grouped <- gets materialAssignment
    prefix <- gets materialPrefix
    pure (prelude <> Binding.writeBinding (Binding.bindingRuntime prefix target) storage value <> [status | not grouped])
  P.Erase name -> do
    target <- bindingName name
    status <- setSourceStatus (ExprLiteral "0")
    prefix <- gets materialPrefix
    pure (Binding.eraseBinding (Binding.bindingRuntime prefix target) <> [status])
  P.SetArguments wordsValue -> do
    (prelude, arguments) <- lowerWords wordsValue
    status <- setSourceStatus (ExprLiteral "0")
    pure (prelude <> [builtin "set" (arg (ExprLiteral "argv") : arguments), status])
  P.DirectoryOperation operation -> do
    needNative NativeDirectory "Parent directory transition and actual diagnostics"
    prefix <- fresh "directory_"
    runtime <- gets materialPrefix
    status <- runtimeName "status"
    range <- gets materialRange
    guardStatements <- errexitGuard suppressed
    mode <- gets (entryMode . materialConfig)
    let origin = maybe "<input>" (srcFile . rangeStart) range
        line = maybe "1" (show . srcLine . rangeStart) range
    pure (Directory.directoryStatements mode prefix runtime status origin line operation <> guardStatements)
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
  P.ShiftArguments count -> do
    zero <- setSourceStatus (ExprLiteral "0")
    one <- setSourceStatus (ExprLiteral "1")
    guards <- errexitGuard suppressed
    let enough = builtin "test" [arg (ExprQuotedCommandSubst (builtin "count" [arg (ExprVariable (VarAll "argv"))] :| [])), arg (ExprLiteral "-ge"), arg (ExprLiteral (show count))]
        remove = [builtin "set" [arg (ExprLiteral "--erase"), arg (ExprLiteral ("argv[1.." <> show count <> "]"))] | count > 0]
    pure ([ifStatements [enough] (remove <> [zero]) [one]] <> guards)
  P.ArithmeticFor initial predicate increment body -> do
    failed <- fresh "arithmetic_loop_failed"
    result <- fresh "arithmetic_loop_status"
    (initialBody, _) <- lowerHeader failed initial
    (predicateBody, predicateValue) <- lowerHeader failed predicate
    (incrementBody, _) <- lowerHeader failed increment
    previous <- gets materialLoops
    previousActions <- gets materialContinueActions
    let incrementActions = incrementBody <> [ifStatements [testEquals (scalarVar failed) "1"] [Stmt Break] []]
    modify' (\material -> material {materialLoops = result : previous, materialContinueActions = incrementActions : previousActions})
    bodyValue <- lowerStatements suppressed body
    modify' (\material -> material {materialLoops = previous, materialContinueActions = previousActions})
    status <- runtimeName "status"
    finalStatus <- setSourceStatus (scalarVar result)
    failedStatus <- setSourceStatus (ExprLiteral "1")
    guards <- errexitGuard suppressed
    let nonzero = builtin "test" [arg predicateValue, arg (ExprLiteral "!="), arg (ExprLiteral "0")]
        predicateCheck = ifStatements [testEquals (scalarVar failed) "0"] [nonzero] [builtin "false" []]
        loop = Stmt (While (condition (predicateBody <> [predicateCheck])) (bodyNE (bodyValue <> [assign [] result (scalarVar status)] <> incrementActions)) [])
    pure ([assign [SetLocal] failed (ExprLiteral "0"), assign [SetLocal] result (ExprLiteral "0")] <> initialBody <> [ifStatements [testEquals (scalarVar failed) "0"] [loop] [], ifStatements [testEquals (scalarVar failed) "1"] [failedStatus] [finalStatus]] <> guards)
  P.WhileLoop inverted predicate body -> do
    result <- fresh "loop_status"
    previous <- gets materialLoops
    previousActions <- gets materialContinueActions
    modify' (\material -> material {materialLoops = result : previous, materialContinueActions = [] : previousActions})
    predicateBody <- lowerStatements True predicate
    bodyValue <- lowerStatements suppressed body
    modify' (\material -> material {materialLoops = previous, materialContinueActions = previousActions})
    status <- runtimeName "status"
    let check = if inverted then Stmt (Not (Command "test" [arg (scalarVar status), arg (ExprLiteral "="), arg (ExprLiteral "0")])) else testEquals (scalarVar status) "0"
    resultStatus <- setSourceStatus (scalarVar result)
    pure [assign [SetLocal] result (ExprLiteral "0"), Stmt (While (condition (predicateBody <> [check])) (bodyNE (bodyValue <> [assign [] result (scalarVar status)])) []), resultStatus]
  P.ForLoop storage name wordsValue body -> do
    target <- bindingName name
    prefix <- gets materialPrefix
    (prelude, arguments) <- lowerWords wordsValue
    result <- fresh "loop_status"
    previous <- gets materialLoops
    previousActions <- gets materialContinueActions
    modify' (\material -> material {materialLoops = result : previous, materialContinueActions = [] : previousActions})
    bodyValue <- lowerStatements suppressed body
    modify' (\material -> material {materialLoops = previous, materialContinueActions = previousActions})
    values <- fresh "for_values"
    iteration <- fresh "iteration"
    status <- runtimeName "status"
    resultStatus <- setSourceStatus (scalarVar result)
    pure
      ( prelude
          <> [ builtin "set" (arg (ExprLiteral values) : arguments),
               assign [SetLocal] result (ExprLiteral "0"),
               Stmt (For iteration (ExprVariable (VarAll values)) (bodyNE (Binding.writeBinding (Binding.bindingRuntime prefix target) storage (scalarVar iteration) <> bodyValue <> [assign [] result (scalarVar status)])) []),
               resultStatus
             ]
      )
  P.Case scalar arms -> lowerCase suppressed scalar arms
  P.DefineFunction name body -> do
    needProgram (RequiresFishFeature FunctionScopeSharing) "Bash dynamic function scope"
    bodyValue <- lowerStatements False body
    status <- runtimeName "status"
    zero <- setSourceStatus (ExprLiteral "0")
    cfg <- gets materialConfig
    prefix <- gets materialPrefix
    identityTag <- gets materialIdentity
    helpers <- gets materialHelpers
    wrapper <- fresh "function_entry"
    nativeOperations <- gets (foldMap (\case RequiresNativeRuntime _ _ operations -> operations; _ -> mempty) . M.keys . materialRequirements)
    let directBody = bodyValue <> [builtin "return" [arg (if null bodyValue then ExprLiteral "0" else scalarVar status)]]
        framedBody = sourceableEntry cfg nativeOperations prefix identityTag (scalarVar (prefix <> "incoming")) wrapper [] helpers bodyValue
        sharedBody =
          [ ifStatements
              [testEquals (scalarVar (prefix <> "active")) identityTag]
              directBody
              (boundaryFailure "incompatible active translation frame")
          ]
        completeBody =
          if entryMode cfg == Sourceable
            then [assign [SetLocal] (prefix <> "incoming") (scalarVar "status"), ifStatements [builtin "set" [arg (ExprLiteral "--query"), arg (ExprLiteral (prefix <> "active"))]] sharedBody framedBody]
            else directBody
        definition = Stmt (Function (MkFishFunction name ([FuncUnknownFlag "--no-scope-shadowing"] <> [FuncCaptureVariable (NativeRuntime.runtimePathName prefix) | entryMode cfg == Sourceable && not (S.null nativeOperations)]) [] (bodyNE completeBody)))
    pure [definition, zero]
  P.SourceBody request body -> do
    needProgram (RequiresFishFeature FunctionScopeSharing) "Owned source return and caller scope"
    wrapper <- fresh "source"
    bodyValue <- lowerStatements suppressed body
    status <- runtimeName "status"
    (prelude, arguments) <-
      if null (P.sourceRequestArguments request)
        then pure ([], [arg (ExprVariable (VarAll "argv"))])
        else lowerWords (P.sourceRequestArguments request)
    effective <- fresh "source_arguments"
    let mayInherit = not (null (P.sourceRequestArguments request)) && not (any P.guaranteesField (P.sourceRequestArguments request))
        invocation =
          if mayInherit
            then
              [ builtin "set" (map (arg . ExprLiteral) ["--local", "--unexport", "--unpath", "--", effective] <> arguments),
                ifStatements
                  [builtin "set" [arg (ExprLiteral "--query"), arg (ExprLiteral (effective <> "[1]"))]]
                  [Stmt (Command wrapper [arg (ExprVariable (VarAll effective))])]
                  [Stmt (Command wrapper [arg (ExprVariable (VarAll "argv"))])]
              ]
            else [Stmt (Command wrapper arguments)]
    capture <- captureStatus
    guardStatements <- errexitGuard suppressed
    let result = if null bodyValue then ExprLiteral "0" else scalarVar status
        definition = Stmt (Function (MkFishFunction wrapper [FuncUnknownFlag "--no-scope-shadowing"] [] (bodyNE (bodyValue <> [builtin "return" [arg result]]))))
    separate <- gets materialSeparate
    when separate $ modify' (\material -> material {materialModules = M.insert (toString (wrapper <> ".fish")) definition (materialModules material)})
    pure
      ( prelude
          <> [definition | not separate]
          <> invocation
          <> [capture]
          <> [builtin "functions" [arg (ExprLiteral "--erase"), arg (ExprLiteral wrapper)] | not separate]
          <> guardStatements
      )
  P.Subshell region -> do
    invocation <- childMaterialization Child.IsolatedChild region
    status <- captureStatus
    guardStatements <- errexitGuard suppressed
    pure (Child.childPrelude invocation <> [Child.childCommand invocation, status] <> childCleanup invocation <> guardStatements)
  P.Pipeline regions -> do
    invocations <- traverse (childMaterialization Child.IsolatedChild) regions
    prefix <- fresh "pipeline"
    pipefail <- runtimeName "pipefail"
    let result = Child.materializePipeline prefix (scalarVar pipefail) invocations
    traverse_ mergeRequirement (Child.pipelineRequirements result)
    status <- setSourceStatus (Child.pipelineStatus result)
    guardStatements <- errexitGuard suppressed
    pure (Child.pipelineStatements result <> [status] <> concatMap childCleanup invocations <> guardStatements)
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
  P.Return value -> lowerExit "return" value
  P.Exit value -> lowerExit "exit" value
  P.Break -> lowerLoopJump (Stmt Break)
  P.Continue -> do
    actions <- gets (fromMaybe [] . viaNonEmpty head . materialContinueActions)
    jump <- lowerLoopJump (Stmt Continue)
    pure (actions <> jump)
  where
    lowerConjunction onSuccess left right = do
      first <- lowerStatement True left
      second <- lowerStatement suppressed right
      status <- runtimeName "status"
      let branch = if onSuccess then ifStatements [testEquals (scalarVar status) "0"] second [] else ifStatements [testEquals (scalarVar status) "0"] [] second
      pure (first <> [branch])
    lowerExit name value = do
      (prelude, result) <- maybe (([],) . scalarVar <$> runtimeName "status") lowerScalar value
      pure (prelude <> [builtin name [arg result]])

lowerHeader :: Text -> P.Statement -> Materialize ([FishStatement], FishExpr TStr)
lowerHeader failed (P.Statement _ (P.ArithmeticCommand site expression bindings)) = do
  result <- arithmeticMaterialization expression bindings
  pure (Arithmetic.arithmeticStatements result <> [ifStatements [testEquals (Arithmetic.arithmeticError result) ""] [] (arithmeticDiagnostic True site result <> [assign [] failed (ExprLiteral "1")])], Arithmetic.arithmeticValue result)
lowerHeader _ _ = lift (Left (planDiagnostic "arithmetic-header" "Arithmetic loop header must own an integer operation" :| []))

lowerLoopJump :: FishStatement -> Materialize [FishStatement]
lowerLoopJump jump = do
  target <- gets (viaNonEmpty head . materialLoops)
  result <- maybe (lift (Left (planDiagnostic "loop-control" "Loop control has no owned materialization target" :| []))) pure target
  zero <- setSourceStatus (ExprLiteral "0")
  pure [zero, assign [] result (ExprLiteral "0"), jump]

lowerRedirect :: P.Redirection -> Redirect
lowerRedirect = \case
  P.DuplicateDescriptor source target input -> MkRedirect (RedirectFD source) (mode input) (RedirectTargetFD target)
  P.CloseDescriptor source input -> MkRedirect (RedirectFD source) (mode input) RedirectClose
  P.NullDescriptor source input -> MkRedirect (RedirectFD source) (mode input) (RedirectFile (ExprLiteral "/dev/null"))
  where
    mode input = if input then RedirectIn else RedirectOut

lowerInvocation :: Bool -> P.CallTarget -> [ExprOrRedirect] -> Materialize [FishStatement]
lowerInvocation suppressed target arguments = do
  capture <- captureStatus
  guardStatements <- errexitGuard suppressed
  case target of
    P.Function name -> do
      suppression <- runtimeName "suppress"
      saved <- fresh "caller_suppression"
      let call = Stmt (Command name arguments)
      pure
        ( [assign [SetLocal] saved (scalarVar suppression)]
            <> [assign [] suppression (ExprLiteral "1") | suppressed]
            <> [call, capture, assign [] suppression (scalarVar saved)]
            <> guardStatements
        )
    P.Builtin "echo" -> do
      needNative NativeEcho "Bash echo option and byte semantics"
      native <- runtimeName "native"
      pure ([framedPrimitive native "echo" arguments, capture] <> guardStatements)
    P.Builtin name -> pure ([builtin (if name == ":" then "true" else name) arguments, capture] <> guardStatements)
    P.External name -> do
      needProgram (RequiresCommand name) "Explicit external command dispatch"
      helper <- externalHelper name
      pure ([Stmt (Command helper arguments), capture] <> guardStatements)

lowerWords :: [P.Word] -> Materialize ([FishStatement], [ExprOrRedirect])
lowerWords values = do
  results <- traverse lowerWord values
  pure (concatMap fst results, concatMap snd results)

lowerWord :: P.Word -> Materialize ([FishStatement], [ExprOrRedirect])
lowerWord = \case
  P.PathnameFields patternValue -> do
    (prelude, parts) <- lowerPatternParts patternValue
    needNative NativeGlob "Quote-aware pathname expansion"
    native <- runtimeName "native"
    needProgram (RequiresFishFeature NulDelimitedCapture) "Pathname byte transport"
    temporary <- fresh "pathname_fields"
    pure (prelude <> captureList temporary (Pattern.expandPathname native parts), [arg (ExprVariable (VarAll temporary))])
  P.OneField (P.Literal value) -> pure ([], [arg (ExprLiteral value)])
  P.OneField scalar -> do
    (prelude, value) <- lowerScalar scalar
    temporary <- fresh "field"
    pure (prelude <> [assign [SetLocal] temporary value], [arg (scalarVar temporary)])
  P.SplitFields scalar -> do
    (prelude, value) <- lowerScalar scalar
    ifs <- runtimeName "ifs"
    needNative NativeSplit "Bash IFS field splitting"
    native <- runtimeName "native"
    needProgram (RequiresFishFeature NulDelimitedCapture) "IFS field transport"
    temporary <- fresh "fields"
    pure (prelude <> captureList temporary (nulCapture native "split" [scalarVar ifs, value]), [arg (ExprVariable (VarAll temporary))])
  P.QuotedArguments (P.Literal "") (P.Literal "") False -> pure ([], [arg (ExprVariable (VarAll "argv"))])
  P.QuotedArguments before after force -> do
    (preA, prefix) <- lowerScalar before
    savedPrefix <- fresh "argv_prefix"
    (preB, suffix) <- lowerScalar after
    needNative NativeArgv "Quoted argument prefix and suffix cardinality"
    native <- runtimeName "native"
    needProgram (RequiresFishFeature NulDelimitedCapture) "Quoted argument field transport"
    temporary <- fresh "argv_fields"
    let producer = framedPrimitive native "argv" [arg (scalarVar savedPrefix), arg suffix, arg (ExprLiteral (if force then "1" else "0")), arg (ExprVariable (VarAll "argv"))]
    pure (preA <> [assign [SetLocal] savedPrefix prefix] <> preB <> captureList temporary (nulCaptureStatement producer), [arg (ExprVariable (VarAll temporary))])

captureList :: Text -> FishExpr (TList TStr) -> [FishStatement]
captureList name expression =
  [ assignList [SetLocal] name (ExprListLiteral []),
    Stmt (Begin (assign [SetLocal] "fish_read_limit" (ExprLiteral "0") :| [assignList [] name expression]) [])
  ]

nulCapture :: Text -> Text -> [FishExpr TStr] -> FishExpr (TList TStr)
nulCapture native program values = nulCaptureStatement (framedPrimitive native program (map arg values))

framedPrimitive :: Text -> Text -> [ExprOrRedirect] -> FishStatement
framedPrimitive native program values =
  let producer = builtin "printf" (arg (ExprLiteral "%s\\0") : values)
      consumer = Stmt (Command native [arg (ExprLiteral "--abi"), arg (ExprLiteral "1"), arg (ExprLiteral program)])
   in Stmt (Pipeline (MkFishJobPipeline False [] producer [PipeTo [] consumer] False))

nulCaptureStatement :: FishStatement -> FishExpr (TList TStr)
nulCaptureStatement producer = ExprCommandSubst (Stmt (Pipeline (MkFishJobPipeline False [] producer [PipeTo [] (builtin "string" [arg (ExprLiteral "split0")])] False)) :| [])

lowerScalar :: P.Scalar -> Materialize ([FishStatement], FishExpr TStr)
lowerScalar = \case
  P.Literal value -> pure ([], ExprLiteral value)
  P.ByteLiteral bytes -> do
    temporary <- fresh "byte_literal"
    needProgram (RequiresFishFeature NulDelimitedCapture) "ANSI quoted byte transport"
    let escaped = mconcat ["\\" <> toText (showOct byte "") | byte <- BS.unpack bytes]
        producer = builtin "printf" [arg (ExprLiteral "%b\\0"), arg (ExprLiteral escaped)]
    pure (captureList temporary (nulCaptureStatement producer), scalarVar temporary)
  P.AppendValue name value -> do
    (prelude, rhs) <- lowerScalar value
    saved <- fresh "append_rhs"
    target <- bindingName name
    pure (prelude <> [assign [SetLocal] saved rhs], ExprStringConcat (scalarVar target) (scalarVar saved))
  P.ParameterTransform operation scalar patternValue replacement -> do
    (prelude, value) <- lowerScalar scalar
    needNative NativePattern "Bounded byte parameter operation"
    helper <- runtimeName "native"
    temporary <- fresh "parameter_transform"
    needProgram (RequiresFishFeature NulDelimitedCapture) "Parameter byte transport"
    let fields = [ExprLiteral operation, value, ExprLiteral patternValue] <> [ExprLiteral replacement | operation `elem` ["replace-first", "replace-all"]]
    pure (prelude <> captureList temporary (nulCapture helper "pattern" fields), scalarVar temporary)
  P.AlternateValue name nullSensitive alternative -> do
    target <- bindingName name
    temporary <- fresh "parameter_alternate"
    (prelude, value) <- lowerScalar alternative
    let exists = builtin "set" [arg (ExprLiteral "--query"), arg (ExprLiteral (target <> "[1]"))]
        nonempty = builtin "test" [arg (ExprLiteral "-n"), arg (scalarVar target)]
        predicate = if nullSensitive then [Stmt (JobConj (MkFishJobConjunction Nothing (jobOf exists) [JCAnd (jobOf nonempty)]))] else [exists]
    pure ([assign [SetLocal] temporary (ExprLiteral ""), ifStatements predicate (prelude <> [assign [] temporary value]) []], scalarVar temporary)
  P.Variable name -> ([],) . scalarVar <$> bindingName name
  P.PositionalAlternate index nullSensitive alternative -> do
    temporary <- fresh "positional_alternate"
    (prelude, value) <- lowerScalar alternative
    let current = ExprQuotedVariable (VarIndex "argv" (IndexSingle (ExprNumLiteral index)))
        exists = builtin "set" [arg (ExprLiteral "--query"), arg (ExprLiteral ("argv[" <> show index <> "]"))]
        nonempty = builtin "test" [arg (ExprLiteral "-n"), arg current]
        predicate = if nullSensitive then [Stmt (JobConj (MkFishJobConjunction Nothing (jobOf exists) [JCAnd (jobOf nonempty)]))] else [exists]
    pure ([assign [SetLocal] temporary (ExprLiteral ""), ifStatements predicate (prelude <> [assign [] temporary value]) []], scalarVar temporary)
  P.PositionalDefault index nullSensitive alternative -> do
    temporary <- fresh "positional_default"
    (prelude, fallback) <- lowerScalar alternative
    let current = ExprQuotedVariable (VarIndex "argv" (IndexSingle (ExprNumLiteral index)))
        exists = builtin "set" [arg (ExprLiteral "--query"), arg (ExprLiteral ("argv[" <> show index <> "]"))]
        nonempty = builtin "test" [arg (ExprLiteral "-n"), arg current]
        predicate = if nullSensitive then [Stmt (JobConj (MkFishJobConjunction Nothing (jobOf exists) [JCAnd (jobOf nonempty)]))] else [exists]
    pure ([assign [SetLocal] temporary (ExprLiteral ""), ifStatements predicate [assign [] temporary current] (prelude <> [assign [] temporary fallback])], scalarVar temporary)
  P.Positional index -> pure ([], ExprQuotedVariable (VarIndex "argv" (IndexSingle (ExprNumLiteral index))))
  P.ArgumentCount -> pure ([], ExprQuotedCommandSubst (builtin "count" [arg (ExprVariable (VarAll "argv"))] :| []))
  P.LastStatus -> ([],) . scalarVar <$> runtimeName "status"
  P.Concat values | Just literals <- traverse (\case P.Literal value -> Just value; _ -> Nothing) values -> pure ([], ExprLiteral (mconcat literals))
  P.Concat values -> do
    parts <- forM values $ \value -> do
      (prelude, expression) <- lowerScalar value
      temporary <- fresh "scalar_part"
      pure (prelude <> [assign [SetLocal] temporary expression], scalarVar temporary)
    pure (concatMap fst parts, foldl' ExprStringConcat (ExprLiteral "") (map snd parts))
  P.Substitute region -> do
    invocation <- childMaterialization Child.SubstitutionChild region
    prefix <- fresh "capture"
    result <-
      either
        (\message -> lift (Left (planDiagnostic "capture-materialization" message :| [])))
        pure
        (Child.materializeCapture prefix (P.childRange region) invocation)
    traverse_ mergeRequirement (Child.captureRequirements result)
    needProgram (RequiresFishFeature NulDelimitedCapture) "Command substitution byte and status transport"
    marker <- runtimeName "substitution_executed"
    lastStatus <- runtimeName "substitution_status"
    status <- setSourceStatus (Child.captureStatus result)
    let finish = [status, assign [] marker (ExprLiteral "1"), assign [] lastStatus (Child.captureStatus result)]
        check =
          ifStatements
            [testEquals (Child.captureError result) ""]
            finish
            [builtin "printf" [arg (ExprLiteral "monk: child transport failed\n"), RedirectVal (MkRedirect RedirectStdout RedirectOut (RedirectTargetFD 2))], builtin "exit" [arg (ExprLiteral "125")]]
    pure (Child.captureStatements result <> childCleanup invocation <> [check], Child.captureValue result)
  P.DefaultValue storage name nullSensitive assignValue alternative -> do
    target <- bindingName name
    temporary <- fresh "parameter"
    (prelude, value) <- lowerScalar alternative
    prefix <- gets materialPrefix
    let exists = builtin "set" [arg (ExprLiteral "--query"), arg (ExprLiteral (target <> "[1]"))]
        nonempty = builtin "test" [arg (ExprLiteral "-n"), arg (scalarVar target)]
        predicate = if nullSensitive then [Stmt (JobConj (MkFishJobConjunction Nothing (jobOf exists) [JCAnd (jobOf nonempty)]))] else [exists]
        otherwiseBody = prelude <> [assign [] temporary value] <> (if assignValue then Binding.writeBinding (Binding.bindingRuntime prefix target) storage (scalarVar temporary) else [])
    pure ([assign [SetLocal] temporary (ExprLiteral ""), ifStatements predicate [assign [] temporary (scalarVar target)] otherwiseBody], scalarVar temporary)
  P.ArithmeticValue site expression bindings -> do
    result <- arithmeticMaterialization expression bindings
    pure
      ( Arithmetic.arithmeticStatements result
          <> [ ifStatements
                 [testEquals (Arithmetic.arithmeticError result) ""]
                 []
                 (arithmeticDiagnostic False site result <> [builtin "exit" [arg (ExprLiteral "1")]])
             ],
        Arithmetic.arithmeticValue result
      )

lowerDeclarations :: [P.Declaration] -> Materialize [FishStatement]
lowerDeclarations declarations = do
  frozen <- forM declarations $ \declaration -> do
    let value = case declaration of P.DeclareLocal _ _ scalar -> scalar; P.DeclareExport _ _ scalar -> scalar
    case value of
      Nothing -> pure ([], declaration, Nothing)
      Just scalar -> do
        (prelude, result) <- lowerScalar scalar
        name <- fresh "declaration_value"
        pure (prelude <> [assign [SetLocal] name result], declaration, Just (scalarVar name))
  prefix <- gets materialPrefix
  installed <- forM frozen $ \(_, declaration, value) -> case declaration of
    P.DeclareLocal freshBinding name _ -> do
      target <- bindingName name
      temporary <- fresh "binding"
      pure (Binding.declareLocal temporary (Binding.bindingRuntime prefix target) freshBinding value)
    P.DeclareExport storage name _ -> do
      target <- bindingName name
      pure (Binding.declareExport (Binding.bindingRuntime prefix target) storage value)
  status <- setSourceStatus (ExprLiteral "0")
  pure (concatMap (\(prelude, _, _) -> prelude) frozen <> concat installed <> [status])

arithmeticMaterialization :: ArithmeticExpr -> M.Map Text P.Storage -> Materialize Arithmetic.ArithmeticMaterialization
arithmeticMaterialization expression bindings = do
  native <- runtimeName "native"
  prefix <- fresh "arithmetic"
  ownerPrefix <- gets materialPrefix
  ifs <- runtimeName "ifs"
  let actualName name = if name == "IFS" then ifs else name
      getter "#" = ExprQuotedCommandSubst (builtin "count" [arg (ExprVariable (VarAll "argv"))] :| [])
      getter name = scalarVar (actualName name)
      setter name = Binding.writeBinding (Binding.bindingRuntime ownerPrefix (actualName name)) (M.findWithDefault P.Global name bindings)
  result <- either (\message -> lift (Left (planDiagnostic "arithmetic-materialization" message :| []))) pure (Arithmetic.materializeArithmetic native prefix getter setter expression)
  unless (null (Arithmetic.arithmeticRequirements result)) (needNative NativeInteger "Bash signed-64-bit integer primitives")
  existing <- gets (helperNames . materialHelpers)
  let additions = [helper | helper <- Arithmetic.arithmeticHelpers result, all (`notElem` existing) (helperNames [helper])]
  modify' (\s -> s {materialHelpers = materialHelpers s <> additions})
  traverse_ mergeRequirement (Arithmetic.arithmeticRequirements result)
  pure result

externalHelper :: Text -> Materialize Text
externalHelper executable = do
  needProgram (RequiresFishFeature FunctionScopeSharing) "Owned external environment projection"
  helper <- runtimeName ("external_" <> T.intercalate "_" [toText (showHex (ord character) "") | character <- toString executable])
  existing <- gets (helperNames . materialHelpers)
  unless (helper `elem` existing) $ do
    temporary <- fresh "environment"
    prefix <- gets materialPrefix
    names <- gets materialBindings
    bindings <- traverse (fmap (Binding.bindingRuntime prefix) . bindingName) (S.toAscList (S.delete "IFS" names))
    let body = Binding.environmentShadows temporary bindings <> [external executable [arg (ExprVariable (VarAll "argv"))]]
        definition = Stmt (Function (MkFishFunction helper [FuncUnknownFlag "--no-scope-shadowing"] [] (bodyNE body)))
    modify' (\material -> material {materialHelpers = materialHelpers material <> [definition]})
  pure helper

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

childMaterialization :: Child.ChildMode -> P.ChildRegion -> Materialize Child.ChildInvocation
childMaterialization mode region = do
  needNative NativeChildRun "Owned child launch"
  prefix <- fresh "child"
  runtimePrefix <- gets materialPrefix
  suppressed <- gets materialSuppressed
  previousHelpers <- gets materialHelpers
  definitions <- fmap concat $ forM (M.toAscList (P.childFunctions region)) $ \(name, body) ->
    lowerStatement False (P.Statement (P.childRange region) (P.DefineFunction name body))
  body <- lowerStatements suppressed (P.childStatements region)
  helpers <- gets materialHelpers
  modify' (\s -> s {materialHelpers = previousHelpers})
  ownedBindings <- gets materialBindings
  actualBindings <- S.fromList <$> traverse bindingName (S.toAscList (P.childVariables region <> (if P.childNeedsEnvironment region then ownedBindings else mempty)))
  ownerPrefix <- gets materialPrefix
  let bindings = actualBindings <> foldMap (Binding.bindingRuntimeNames . Binding.bindingRuntime ownerPrefix) actualBindings
  let runtime =
        Child.MkChildRuntime
          runtimePrefix
          (runtimePrefix <> "status")
          (runtimePrefix <> "errexit")
          (runtimePrefix <> "suppress")
          (S.fromList [runtimePrefix <> role | role <- ["status", "errexit", "pipefail", "suppress", "ifs", "active", "substitution_executed", "substitution_status", "native_path"]])
          (NativeRuntime.runtimeHelperName runtimePrefix)
          suppressed
  result <-
    either
      (\message -> lift (Left (planDiagnostic "child-materialization" message :| [])))
      pure
      (Child.materializeChild prefix mode runtime bindings (helperClosure helpers (body <> definitions) <> definitions) body)
  traverse_ mergeRequirement (Child.childRequirements result)
  needProgram (RequiresFishFeature FunctionScopeSharing) "Owned child body and dynamic function closure"
  pure result

lowerCase :: Bool -> P.Scalar -> [P.CaseArm] -> Materialize [FishStatement]
lowerCase suppressed scalar arms = do
  (prelude, value) <- lowerScalar scalar
  input <- fresh "case_value"
  mode <- fresh "case_mode"
  result <- fresh "case_status"
  status <- runtimeName "status"
  armBodies <- forM arms $ \(P.CaseArm patterns body ending) -> do
    matches <- lowerPatterns input patterns
    bodyValue <- lowerStatements suppressed body
    let afterMode = case ending of P.StopCase -> "2"; P.FallThrough -> "1"; P.Retest -> "0"
        execute = bodyValue <> [assign [] result (if null bodyValue then ExprLiteral "0" else scalarVar status), assign [] mode (ExprLiteral afterMode)]
        attempt = ifStatements [testEquals (scalarVar mode) "1"] execute [ifStatements matches execute []]
    pure (ifStatements [testEquals (scalarVar mode) "2"] [] [attempt])
  resultStatus <- setSourceStatus (scalarVar result)
  pure (prelude <> [assign [SetLocal] input value, assign [SetLocal] mode (ExprLiteral "0"), assign [SetLocal] result (ExprLiteral "0")] <> armBodies <> [resultStatus])

lowerPatternParts :: P.Pattern -> Materialize ([FishStatement], [(Bool, FishExpr TStr)])
lowerPatternParts (P.MkPattern parts) = do
  values <- forM parts $ \part -> do
    let (active, scalar) = case part of P.LiteralPattern value -> (False, value); P.ActivePattern value -> (True, value)
    (prelude, value) <- lowerScalar scalar
    case scalar of
      P.Literal _ -> pure (prelude, (active, value))
      _ -> do
        temporary <- fresh "pattern_part"
        pure (prelude <> [assign [SetLocal] temporary value], (active, scalarVar temporary))
  pure (concatMap fst values, map snd values)

lowerPatterns :: Text -> [P.Pattern] -> Materialize [FishStatement]
lowerPatterns _ [] = pure [builtin "false" []]
lowerPatterns input (patternValue : remaining) = do
  (prelude, parts) <- lowerPatternParts patternValue
  when (Pattern.requiresRuntime parts) (needNative NativePattern "Lazy quote-aware case pattern")
  native <- runtimeName "native"
  tailValue <- lowerPatterns input remaining
  let matched = Stmt (Pattern.matchPattern native (scalarVar input) parts)
  pure (prelude <> [ifStatements [matched] [builtin "true" []] tailValue])

-- Source entry and exported functions own their entire temporary lifetime.
-- The first block-local capture preserves incoming status without overwriting a
-- caller binding; all observable guards precede helper/global/user effects.
sourceableEntry :: TranslateConfig -> S.Set NativeOperation -> Text -> Text -> FishExpr TStr -> Text -> [Text] -> [FishStatement] -> [FishStatement] -> [FishStatement]
sourceableEntry cfg nativeOperations prefix identityTag incomingValue wrapper moduleFunctions helpers body =
  [assign [SetLocal] incoming incomingValue]
    <> privateGuardsWithCaptured guardFailure captured prefix incoming (callerGuards (callerContract cfg) execution)
  where
    captured = [NativeRuntime.runtimePathName prefix | wrapper /= prefix <> "entry" && not (S.null nativeOperations)]
    incoming = prefix <> "incoming"
    result = prefix <> "result"
    finish = prefix <> "finish"
    execution =
      [ Stmt (Function (MkFishFunction wrapper [FuncUnknownFlag "--no-scope-shadowing"] [] (bodyNE bodyExecution))),
        Stmt (Command wrapper [arg (ExprVariable (VarAll "argv"))]),
        assign [SetLocal] result (scalarVar "status"),
        builtin "functions" (map (arg . ExprLiteral) ("--erase" : wrapper : (helperNames helpers <> moduleFunctions))),
        Stmt
          ( Function
              ( MkFishFunction
                  finish
                  []
                  []
                  ( builtin "functions" [arg (ExprLiteral "--erase"), arg (ExprLiteral finish)]
                      :| [builtin "return" [arg (ExprQuotedVariable (VarIndex "argv" (IndexSingle (ExprNumLiteral 1))))]]
                  )
              )
          ),
        Stmt (Command finish [arg (scalarVar result)])
      ]
    bodyExecution =
      NativeRuntime.nativeRuntimeSetup cfg prefix nativeOperations
        <> [ assign [SetLocal] (prefix <> "active") (ExprLiteral identityTag),
             assign [SetLocal] (prefix <> "status") (scalarVar incoming),
             assign [SetLocal] (prefix <> "errexit") (ExprLiteral "0"),
             assign [SetLocal] (prefix <> "pipefail") (ExprLiteral "0"),
             assign [SetLocal] (prefix <> "suppress") (ExprLiteral "0"),
             assign [SetLocal] (prefix <> "ifs") (ExprLiteral " \t\n"),
             assign [SetLocal] (prefix <> "substitution_executed") (ExprLiteral "0"),
             assign [SetLocal] (prefix <> "substitution_status") (ExprLiteral "0")
           ]
        <> helpers
        <> (if S.member NativeDirectory nativeOperations then Directory.directorySetup cfg prefix else [])
        <> body
        <> [builtin "return" [arg (if null body then ExprLiteral "0" else scalarVar (prefix <> "status"))]]

helperNames :: [FishStatement] -> [Text]
helperNames statements = [funcName function | Stmt (Function function) <- statements]

boundaryFailure :: Text -> [FishStatement]
boundaryFailure message =
  [ builtin "printf" [arg (ExprLiteral "%s\n"), arg (ExprLiteral ("monk: caller contract failed: " <> message)), RedirectVal (MkRedirect RedirectStdout RedirectOut (RedirectTargetFD 2))],
    builtin "return" [arg (ExprLiteral "125")]
  ]

-- A failing source guard reaches EOF with its status. Native `return` here
-- would escape a Fish function which happened to source this file.
guardFailure :: Text -> [FishStatement]
guardFailure message =
  take 1 (boundaryFailure message)
    <> [external "fish" [arg (ExprLiteral "--no-config"), arg (ExprLiteral "-c"), arg (ExprLiteral "exit 125")]]

guardWhen :: FishStatement -> Text -> [FishStatement] -> [FishStatement]
guardWhen predicate message next = [ifStatements [predicate] (guardFailure message) next]

guardUnless :: FishStatement -> Text -> [FishStatement] -> [FishStatement]
guardUnless predicate message next = [ifStatements [predicate] next (guardFailure message)]

privateGuardsWith :: (Text -> [FishStatement]) -> Text -> Text -> [FishStatement] -> [FishStatement]
privateGuardsWith failure = privateGuardsWithCaptured failure []

privateGuardsWithCaptured :: (Text -> [FishStatement]) -> [Text] -> Text -> Text -> [FishStatement] -> [FishStatement]
privateGuardsWithCaptured failure captured prefix incoming next =
  rejectWhen
    (namesMatch (builtin "functions" [arg (ExprLiteral "--all"), arg (ExprLiteral "--names")]) [])
    "private function namespace is occupied"
    $ rejectWhen
      ( namesMatch
          (builtin "set" [arg (ExprLiteral "--names")])
          [PipeTo [] (builtin "string" [arg (ExprLiteral "match"), arg (ExprLiteral "--invert"), arg (ExprLiteral "--regex"), arg (ExprLiteral "--"), arg (ExprLiteral ("^(" <> T.intercalate "|" (incoming : captured) <> ")$"))])]
      )
      "private variable namespace is occupied"
      next
  where
    rejectWhen predicate message continuation = [ifStatements [predicate] (failure message) continuation]
    namesMatch producer middle =
      Stmt
        ( Pipeline
            ( MkFishJobPipeline
                False
                []
                producer
                (middle <> [PipeTo [] (builtin "string" [arg (ExprLiteral "match"), arg (ExprLiteral "--quiet"), arg (ExprLiteral "--"), arg (ExprLiteral (prefix <> "*"))])])
                False
            )
        )

-- Standalone execution admits only represented environment scalars for
-- relevant source bindings. Unrelated interactive state is immaterial.
standaloneGuards :: Text -> S.Set Text -> [FishStatement] -> [FishStatement]
standaloneGuards prefix bindings next =
  privateGuardsWith failure prefix "" (guardSequence [bindingGuard name [builtin "true" []] | name <- S.toAscList (S.delete "IFS" bindings)] next)
  where
    failure message =
      [ builtin "printf" [arg (ExprLiteral "%s\n"), arg (ExprLiteral ("monk: runtime contract failed: " <> message)), RedirectVal (MkRedirect RedirectStdout RedirectOut (RedirectTargetFD 2))],
        builtin "exit" [arg (ExprLiteral "125")]
      ]
    rejectWhen predicate message continuation = [ifStatements [predicate] (failure message) continuation]
    require predicate message continuation = [ifStatements [predicate] continuation (failure message)]
    query flags name = builtin "set" (map (arg . ExprLiteral) ("--query" : flags <> [name]))
    bindingGuard name continuation =
      rejectWhen (query ["--universal"] name) ("universal binding " <> name) $
        rejectWhen
          (query ["--path"] name)
          ("path binding " <> name)
          [ifStatements [query [] name] (shape name continuation) continuation]
    shape name continuation =
      let count = ExprQuotedCommandSubst (builtin "count" [arg (ExprVariable (VarAll name))] :| [])
       in require (testEquals count "1") ("non-scalar binding " <> name)
            $ rejectWhen (query ["--local"] name) ("local binding " <> name)
            $ require (query ["--export"] name) ("non-environment binding " <> name) continuation

-- Each check has a bounded body and succeeds with status zero. The conjunction
-- stops at the first failed check, preserving its status, and owns one copy of
-- the admitted continuation. Nesting the continuation in optional-binding
-- branches would duplicate the entire remaining program for every binding.
guardSequence :: [[FishStatement]] -> [FishStatement] -> [FishStatement]
guardSequence [] next = next
guardSequence (first : rest) next =
  [ Stmt
      ( JobConj
          ( MkFishJobConjunction
              Nothing
              (jobOf (asCommand first))
              [JCAnd (jobOf (asCommand statements)) | statements <- rest <> [next]]
          )
      )
  ]

callerGuards :: CallerContract -> [FishStatement] -> [FishStatement]
callerGuards contract =
  guardSequence
    ( [variableGuard binding [builtin "true" []] | binding <- M.toList (callerVariables contract)]
        <> [functionGuard imported [builtin "true" []] | imported <- M.elems (callerFunctions contract)]
    )
  where
    query flags name = builtin "set" (map (arg . ExprLiteral) ("--query" : flags <> [name]))
    functionGuard imported =
      guardUnless
        (builtin "functions" [arg (ExprLiteral "--query"), arg (ExprLiteral (functionTarget imported))])
        ("missing imported function " <> functionTarget imported)
    variableGuard (name, ScalarBinding access scope exported) remaining =
      guardWhen (query ["--universal"] name) ("universal binding " <> name) $
        (if scope == GlobalBinding then guardWhen (query ["--local"] name) ("local shadow of global " <> name) else id)
          [ ifStatements
              [query scopeFlags name]
              shape
              (if access == OutputBinding && scope == GlobalBinding && exported == UnexportedBinding then remaining else guardFailure ("missing scalar " <> name))
          ]
      where
        scopeFlags = ["--global" | scope == GlobalBinding]
        cardinality = ExprQuotedCommandSubst (builtin "count" [arg (ExprVariable (VarAll name))] :| [])
        shape =
          guardUnless (testEquals cardinality "1") ("non-scalar binding " <> name)
            $ guardWhen (query (scopeFlags <> ["--path"]) name) ("path binding " <> name)
            $ (if exported == ExportedBinding then guardUnless else guardWhen)
              (query (scopeFlags <> ["--export"]) name)
              ("export attribute mismatch for " <> name)
              remaining

literalWord :: P.Word -> Maybe Text
literalWord (P.OneField (P.Literal value)) = Just value
literalWord _ = Nothing

nativeEcho :: [Text] -> Maybe (Bool, [Text])
nativeEcho = options True False
  where
    options newline escapes (value : rest)
      | Just flags <- T.stripPrefix "-" value,
        not (T.null flags),
        T.all (`elem` ['n', 'e', 'E']) flags =
          let step (n, e) flag = case flag of 'n' -> (False, e); 'e' -> (n, True); _ -> (n, False)
              (nextNewline, nextEscapes) = T.foldl' step (newline, escapes) flags
           in options nextNewline nextEscapes rest
    options newline escapes values
      | not escapes || not (any (T.any (== '\\')) values) = Just (newline, values)
      | otherwise = Nothing

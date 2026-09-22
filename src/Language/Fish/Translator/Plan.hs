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
import Language.Fish.Translator.Native qualified as Native
import Language.Fish.Translator.NativeRuntime qualified as NativeRuntime
import Language.Fish.Translator.Pattern qualified as Pattern
import Language.Fish.Translator.Session qualified as Session
import Language.Fish.Translator.Statistics (commandReferences, materializationStatistics)
import Language.Fish.Translator.Traps qualified as Traps
import Monk.Runtime.Integer qualified as Integer
import Monk.Runtime.NativeTarget (runtimeABI)
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
    materialErrexitRelevant :: Bool,
    materialSession :: Bool,
    materialDescriptorDepth :: Int,
    materialReturnDepth :: Int,
    materialLoopDescriptorDepths :: [Int],
    materialTraps :: Bool,
    materialDeferCompletion :: Bool,
    materialCallback :: Bool
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
compileMaterialization _ (P.SourcePlan _ _ reserved)
  | any (`S.member` reserved) ["MONK_LAUNCH_ORIGINAL", "MONK_LAUNCH_WRAPPER"] =
      Left (planDiagnostic "launch-binding" "Source bindings may not overlap private standalone launch metadata" :| [])
compileMaterialization _ (P.SourcePlan cfg statements reserved)
  | entryMode cfg == Standalone,
    profileSupportsFishFeature (targetProfile cfg) Fish46,
    let prefix = choosePrefix reserved 0,
    Just (body, commands, writesOutput) <- Native.nativeStatements prefix statements =
      let effects = foldMap Effects.statementEffects statements
          bindings = S.delete "#" (Effects.effectReads effects <> Effects.effectWrites effects)
          operations = S.fromList ([NativeExec | not (S.null commands)] <> [NativeWrite | writesOutput])
          script = MkScript (if S.null operations then standaloneBindingGuards bindings <> body else standaloneGuards prefix bindings (NativeRuntime.nativeRuntimeSetup cfg prefix operations <> [NativeRuntime.nativeWriterDefinition prefix | writesOutput] <> body))
          requirement program reason = MkRuntimeRequirement program (MkRequirementUse reason Nothing :| [])
          requirements = nativeRuntimeRequirement NativeLaunch "Preserve streams before Fish startup" : requirement (RequiresFishFeature Fish46) "Structural Fish execution profile" : [requirement (RequiresCommand name) "Explicit external command dispatch" | name <- S.toAscList commands] <> [nativeRuntimeRequirement NativeExec "Source-located external exec failures" | not (S.null commands)] <> [nativeRuntimeRequirement NativeWrite "Bash output errno and signal semantics" | writesOutput]
          translation = MkPlannedTranslation script [] requirements (materializationStatistics prefix (NativeRuntime.runtimeHelperName prefix) script)
       in Right (MkPlannedBundle translation mempty mempty)
compileMaterialization separate (P.SourcePlan cfg statements reserved) = do
  let prefix = choosePrefix reserved 0
      identityTag = show (cfg, statements)
      effects = foldMap Effects.statementEffects statements
      bindings = S.delete "#" (Effects.effectReads effects <> Effects.effectWrites effects <> M.keysSet (callerVariables (callerContract cfg)))
      initial = MkMaterialization prefix 0 mempty [] cfg identityTag False False Nothing [] bindings (separate && not (Effects.effectSession effects)) mempty [] [] (Effects.effectMayEnableErrexit effects) (Effects.effectSession effects) 0 0 [] (Effects.effectTraps effects) False False
  when (not (S.null (Effects.effectArrays effects)) && entryMode cfg == Sourceable) (Left (planDiagnostic "array-entry" "Owned arrays currently require standalone execution; caller contracts describe scalar bindings" :| []))
  when (Effects.effectTraps effects && Effects.effectDirectory effects) (Left (planDiagnostic "directory-trap-signal" "Directory operations combined with EXIT/ERR traps require shared stdio error-state ownership across signal callbacks" :| []))
  when (Effects.effectSession effects && entryMode cfg == Sourceable) (Left (planDiagnostic "session-entry" "Session effects require standalone execution" :| []))
  when (Effects.effectSession effects && any (`S.member` reserved) ["MONK_SESSION_SOCKET", "MONK_SESSION_TOKEN", "MONK_SESSION_REPLY", "MONK_SESSION_FDS"]) (Left (planDiagnostic "session-binding" "Source bindings may not overlap the private session transport" :| []))
  (body, final) <-
    runStateT
      ( do
          needProgram (RequiresFishFeature Fish46) "Structural Fish execution profile"
          when (entryMode cfg == Standalone) (mergeRequirement (nativeRuntimeRequirement NativeLaunch "Preserve streams before Fish startup"))
          when (Effects.effectSession effects) $ do
            needNative NativeSession "Owned process and descriptor session"
            needNative NativeDescriptorState "Observe user streams before session control transport"
            needProgram (RequiresCommand "fish") "Private generated Fish evaluator"
            modify' (\material -> material {materialHelpers = materialHelpers material <> [Session.requestDefinition prefix]})
          when (Effects.effectTraps effects) $
            modify' (\material -> material {materialHelpers = materialHelpers material <> Traps.definitions prefix})
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
          <> [assign [SetGlobal] (prefix <> "source_origin") (ExprLiteral (fromMaybe "<input>" (listToMaybe [srcFile (rangeStart range) | P.Statement (Just range) _ <- statements]))) | Effects.effectTraps effects]
          <> [assign [SetGlobal] (prefix <> role) (ExprLiteral "0") | Effects.effectMayEnableErrexit effects, role <- ["errexit", "suppress"]]
          <> [assign [SetGlobal] (prefix <> "last_pid") (ExprLiteral "") | Effects.effectSession effects]
          <> [assign [SetGlobal] (prefix <> "pipefail") (ExprLiteral "0") | Effects.effectPipefail effects]
          <> [assign [SetGlobal] (prefix <> "ifs") (ExprLiteral " \t\n") | S.member "IFS" bindings]
          <> [assign [SetGlobal] (prefix <> role) (ExprLiteral "0") | Effects.effectSubstitution effects, role <- ["substitution_executed", "substitution_status"]]
      moduleDefinitions = M.elems (materialModules final)
      moduleFunctions = helperNames moduleDefinitions
      modules = fmap (MkScript . (: [])) (materialModules final)
      moduleRoot = prefix <> "module_root"
      loaders =
        [assign [SetLocal] moduleRoot (NativeRuntime.entryDirectory prefix) | not (M.null modules)]
          <> concatMap (loadModule (entryMode cfg == Sourceable) moduleRoot) (M.keys modules)
      helpers = materialHelpers final <> loaders
      nativeOperations = foldMap (\case RequiresNativeRuntime _ _ operations -> operations; _ -> mempty) (M.keys (materialRequirements final))
      runtimeSetup = NativeRuntime.nativeRuntimeSetup cfg prefix nativeOperations
      programBody = initialization <> [statement | Effects.effectTraps effects, statement <- Traps.initialize prefix] <> helpers <> (if S.member NativeDirectory nativeOperations then Directory.directorySetup cfg prefix else []) <> body <> [if Effects.effectTraps effects then Traps.exitWithStatus prefix (scalarVar statusName) else builtin "exit" [arg (scalarVar statusName)]]
      complete =
        if entryMode cfg == Sourceable
          then [asCommand (sourceableEntry cfg nativeOperations prefix identityTag (scalarVar "status") (prefix <> "entry") moduleFunctions helpers body)]
          else standaloneGuards prefix (bindings S.\\ Effects.effectArrays effects) (standaloneArrayGuards (Effects.effectArrays effects) <> [statement | Effects.effectSession effects, statement <- sessionEnvironmentGuards] <> runtimeSetup <> if Effects.effectSession effects then Session.launchSession prefix programBody else programBody)
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
    RequiresNativeRuntime abi profile _ -> unless (abi == runtimeABI && profile == targetProfile cfg) (Left (planDiagnostic "native-runtime-capability" "Native runtime ABI/profile is incompatible" :| []))
  -- The owned complete script includes every inserted initialization and final
  -- control operation; no subsequent pass changes its semantics.
  pure (MkPlannedBundle (MkPlannedTranslation (MkScript complete) (materialDiagnostics final) requirements statistics) modules (fmap (materializationStatistics prefix (NativeRuntime.runtimeHelperName prefix)) modules))

-- Native session stages consume the same compiled child snapshots as bounded
-- helpers. External-only stages keep their real executable PID.
sessionStage :: P.ChildRegion -> Materialize ([FishStatement], [ExprOrRedirect], [FishStatement])
sessionStage region = case concatMap flattenStatements (P.childStatements region) of
  [P.Statement _ (P.Invoke (P.External name) wordsValue)] | all pureStageWord wordsValue -> do
    (prelude, arguments) <- lowerWords wordsValue
    needProgram (RequiresCommand name) "Owned external pipeline stage"
    let origin = maybe "<input>" (srcFile . rangeStart) (P.childRange region)
        line = maybe "1" (show . srcLine . rangeStart) (P.childRange region)
    pure (prelude, map (arg . ExprLiteral) ["external-site", origin, line, name] <> arguments, [])
  _ -> do
    invocation <- childMaterialization Child.IsolatedChild region
    pure (Child.childSnapshotPrelude invocation, arg (ExprLiteral "snapshot") : Child.childSessionFrames invocation, childCleanup invocation)

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

projectedSessionRequest :: Text -> [ExprOrRedirect] -> Materialize FishStatement
projectedSessionRequest operation frames = do
  temporary <- fresh "session_environment"
  prefix <- gets materialPrefix
  names <- gets materialBindings
  bindings <- traverse (fmap (Binding.bindingRuntime prefix) . bindingName) (S.toAscList (S.delete "IFS" names))
  pure (Stmt (Begin (bodyNE (Binding.environmentShadows temporary bindings <> [Session.request prefix operation frames])) []))

flattenStatements :: P.Statement -> [P.Statement]
flattenStatements (P.Statement _ (P.Sequence body)) = concatMap flattenStatements body
flattenStatements statement = [statement]

lowerSessionPipeline :: Bool -> Text -> NonEmpty P.ChildRegion -> Materialize [FishStatement]
lowerSessionPipeline suppressed operation regions = do
  stages <- traverse sessionStage regions
  pipefail <- runtimeName "pipefail"
  saved <- forM stages $ \(prelude, frames, cleanup) -> do
    name <- fresh "stage"
    let remaining = ExprVariable (VarIndex name (IndexRange (Just (ExprNumLiteral 2)) Nothing))
        kind = ExprQuotedVariable (VarIndex name (IndexSingle (ExprNumLiteral 1)))
    pure (prelude <> [builtin "set" (map (arg . ExprLiteral) ["--local", "--unexport", "--unpath", name] <> frames)], [arg kind, arg (ExprQuotedCommandSubst (builtin "count" [arg remaining] :| [])), arg remaining], cleanup)
  captured <- captureStatus
  guards <- if operation == "spawn" then pure [] else errexitGuard suppressed
  let prefixFrames = [arg (ExprLiteral "pipeline"), arg (scalarVar pipefail), arg (ExprLiteral (show (length regions)))]
  invocation <- projectedSessionRequest operation (prefixFrames <> concatMap (\(_, frames, _) -> frames) saved)
  pure (concatMap (\(before, _, _) -> before) saved <> [invocation, captured] <> concatMap (\(_, _, after) -> after) saved <> guards)

diagnosticOrigin :: Materialize (Text, Text)
diagnosticOrigin = do
  range <- gets materialRange
  pure (maybe "<input>" (srcFile . rangeStart) range, maybe "1" (show . srcLine . rangeStart) range)

-- Callback diagnostics belong to the execution site, not the trap declaration.
-- Handler line offsets are bounded compiler metadata, not Bash arithmetic.
diagnosticArguments :: Materialize [ExprOrRedirect]
diagnosticArguments = do
  callback <- gets materialCallback
  prefix <- gets materialPrefix
  (origin, line) <- diagnosticOrigin
  range <- gets materialRange
  let offset = maybe 0 (subtract 1 . srcLine . rangeStart) range
  pure $
    if callback
      then [arg (scalarVar (prefix <> "callback_origin")), if offset == 0 then arg (scalarVar (prefix <> "callback_line")) else arg (ExprMath (scalarVar (prefix <> "callback_line") :| [ExprLiteral "+", ExprLiteral (show offset)]))]
      else map (arg . ExprLiteral) [origin, line]

diagnosticOriginExpression :: Materialize (FishExpr TStr)
diagnosticOriginExpression = do
  callback <- gets materialCallback
  prefix <- gets materialPrefix
  (origin, _) <- diagnosticOrigin
  pure (if callback then scalarVar (prefix <> "callback_origin") else ExprLiteral origin)

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
  unless (operation `elem` [NativeExec, NativeWrite] || name `elem` helperNames helpers) $ do
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
      deferred <- gets materialDeferCompletion
      if relevant && not deferred then materializeGuard else pure []
  where
    materializeGuard = do
      enabled <- runtimeName "errexit"
      suppression <- runtimeName "suppress"
      status <- runtimeName "status"
      traps <- gets materialTraps
      prefix <- gets materialPrefix
      origin <- diagnosticOriginExpression
      location <- diagnosticArguments
      let terminate = if traps then Traps.exitWithStatusAt prefix origin (scalarVar status) else builtin "exit" [arg (scalarVar status)]
          exitFailure = ifStatements [testEquals (scalarVar status) "0"] [] ([Traps.errorHook prefix location | traps] <> [ifStatements [testEquals (scalarVar enabled) "1"] [terminate] []])
      pure [ifStatements [testEquals (scalarVar suppression) "0"] [exitFailure] []]

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
  P.Invoke target wordsValue -> do
    (prelude, body) <- lowerInvokeWords suppressed target wordsValue
    pure (prelude <> body)
  P.PrefixedInvoke assignments target wordsValue -> do
    (prelude, body) <- lowerPrefixedInvoke suppressed assignments target wordsValue
    pure (prelude <> body)
  P.Redirected redirects statement -> do
    supervised <- gets materialSession
    if supervised then lowerOwnedRedirects suppressed redirects statement else lowerDirectRedirects suppressed redirects statement
  P.Read options target -> lowerRead suppressed options target
  P.PrefixedRead assignments options target -> do
    bindings <- fmap concat $ forM assignments $ \(_, name, scalar) -> do
      (prelude, value) <- lowerScalar scalar
      actual <- bindingName name
      pure (prelude <> [assign [SetLocal, SetUnexport, SetUnpath] actual value])
    body <- lowerRead suppressed options target
    pure [Stmt (Begin (bodyNE (bindings <> body)) [])]
  P.DeclarationCommand declarations -> lowerDeclarations declarations
  P.Assign storage name scalar -> do
    (prelude, value) <- lowerScalar scalar
    target <- bindingName name
    status <- setSourceStatus (ExprLiteral "0")
    grouped <- gets materialAssignment
    prefix <- gets materialPrefix
    pure (prelude <> Binding.writeBinding (Binding.bindingRuntime prefix target) storage value <> [status | not grouped])
  P.AssignArray storage name values -> lowerArrayWrite storage name values False
  P.AppendArray storage name values -> lowerArrayWrite storage name values True
  P.AssignArrayElement storage name index scalar -> do
    (prelude, value) <- lowerScalar scalar
    target <- bindingName name
    status <- setSourceStatus (ExprLiteral "0")
    grouped <- gets materialAssignment
    pure (prelude <> arrayWrite storage target (target <> "[" <> show (index + 1) <> "]") [arg value] <> [status | not grouped])
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
    supervised <- gets materialSession
    let origin = maybe "<input>" (srcFile . rangeStart) range
        line = maybe "1" (show . srcLine . rangeStart) range
    traps <- gets materialTraps
    incoming <- fresh "directory_incoming"
    let terminate = if traps then [assign [SetGlobal] (runtime <> "pending_signal") (ExprLiteral "13"), Traps.exitWithStatusAt runtime (ExprLiteral origin) (scalarVar incoming)] else [Session.request runtime "finish-signal" [arg (ExprLiteral "13")], builtin "exit" [arg (ExprLiteral "141")]]
    pure ([assign [SetLocal] incoming (scalarVar status) | supervised && traps] <> Directory.directoryStatements supervised mode prefix runtime status origin line operation <> [ifStatements [testEquals (scalarVar status) "141"] terminate [] | supervised] <> guardStatements)
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
    previousDepths <- gets materialLoopDescriptorDepths
    loopDepth <- gets materialDescriptorDepth
    let incrementActions = incrementBody <> [ifStatements [testEquals (scalarVar failed) "1"] [Stmt Break] []]
    modify' (\material -> material {materialLoops = result : previous, materialContinueActions = incrementActions : previousActions, materialLoopDescriptorDepths = loopDepth : previousDepths})
    bodyValue <- lowerStatements suppressed body
    modify' (\material -> material {materialLoops = previous, materialContinueActions = previousActions, materialLoopDescriptorDepths = previousDepths})
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
    previousDepths <- gets materialLoopDescriptorDepths
    loopDepth <- gets materialDescriptorDepth
    modify' (\material -> material {materialLoops = result : previous, materialContinueActions = [] : previousActions, materialLoopDescriptorDepths = loopDepth : previousDepths})
    predicateBody <- lowerStatements True predicate
    bodyValue <- lowerStatements suppressed body
    modify' (\material -> material {materialLoops = previous, materialContinueActions = previousActions, materialLoopDescriptorDepths = previousDepths})
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
    previousDepths <- gets materialLoopDescriptorDepths
    loopDepth <- gets materialDescriptorDepth
    modify' (\material -> material {materialLoops = result : previous, materialContinueActions = [] : previousActions, materialLoopDescriptorDepths = loopDepth : previousDepths})
    bodyValue <- lowerStatements suppressed body
    modify' (\material -> material {materialLoops = previous, materialContinueActions = previousActions, materialLoopDescriptorDepths = previousDepths})
    values <- fresh "for_values"
    iteration <- fresh "iteration"
    status <- runtimeName "status"
    resultStatus <- setSourceStatus (scalarVar result)
    pure
      ( prelude
          <> [ builtin "set" (arg (ExprLiteral values) : arguments),
               assign [SetLocal] result (ExprLiteral "0"),
               Stmt (For iteration (ExprVariable (VarAll values)) (bodyNE ([statement | name /= "_", statement <- Binding.writeBinding (Binding.bindingRuntime prefix target) storage (scalarVar iteration)] <> bodyValue <> [assign [] result (scalarVar status)])) []),
               resultStatus
             ]
      )
  P.Case scalar arms -> lowerCase suppressed scalar arms
  P.DefineFunction name body -> do
    needProgram (RequiresFishFeature FunctionScopeSharing) "Bash dynamic function scope"
    bodyValue <- withDescriptorRoot (lowerStatements False body)
    status <- runtimeName "status"
    zero <- setSourceStatus (ExprLiteral "0")
    cfg <- gets materialConfig
    prefix <- gets materialPrefix
    identityTag <- gets materialIdentity
    helpers <- gets materialHelpers
    wrapper <- fresh "function_entry"
    nativeOperations <- gets (foldMap (\case RequiresNativeRuntime _ _ operations -> operations; _ -> mempty) . M.keys . materialRequirements)
    traps <- gets materialTraps
    let directBody = [statement | traps, statement <- Traps.functionEntry prefix] <> bodyValue <> [builtin "return" [arg (if null bodyValue then ExprLiteral "0" else scalarVar status)]]
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
    previousReturn <- gets materialReturnDepth
    deferred <- gets materialDeferCompletion
    currentDepth <- gets materialDescriptorDepth
    modify' (\material -> material {materialReturnDepth = currentDepth, materialDeferCompletion = False})
    bodyValue <- lowerStatements suppressed body
    modify' (\material -> material {materialReturnDepth = previousReturn, materialDeferCompletion = deferred})
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
    supervised <- gets materialSession
    if supervised
      then lowerSessionPipeline suppressed "run" regions
      else do
        invocations <- traverse (childMaterialization Child.IsolatedChild) regions
        prefix <- fresh "pipeline"
        pipefail <- runtimeName "pipefail"
        let result = Child.materializePipeline prefix (scalarVar pipefail) invocations
        traverse_ mergeRequirement (Child.pipelineRequirements result)
        status <- setSourceStatus (Child.pipelineStatus result)
        guardStatements <- errexitGuard suppressed
        pure (Child.pipelineStatements result <> [status] <> concatMap childCleanup invocations <> guardStatements)
  P.SupervisedPipeline regions -> lowerSessionPipeline suppressed "run" regions
  P.Background region -> do
    case concatMap flattenStatements (P.childStatements region) of
      [P.Statement _ (P.Pipeline regions)] -> lowerSessionPipeline True "spawn" regions
      [P.Statement _ (P.SupervisedPipeline regions)] -> lowerSessionPipeline True "spawn" regions
      _ -> do
        (prelude, frames, cleanup) <- sessionStage region
        invocation <- projectedSessionRequest "spawn" frames
        captured <- captureStatus
        pure (prelude <> [invocation, captured] <> cleanup)
  P.Wait wordsValue -> do
    (prelude, arguments) <- lowerWords wordsValue
    prefix <- gets materialPrefix
    (origin, line) <- diagnosticOrigin
    captured <- captureStatus
    guards <- errexitGuard suppressed
    pure (prelude <> [Session.request prefix "wait" (map (arg . ExprLiteral) [origin, line] <> arguments), captured] <> guards)
  P.SetTrap kind handler -> do
    prefix <- gets materialPrefix
    zero <- setSourceStatus (ExprLiteral "0")
    case handler of
      Nothing -> pure (Traps.install prefix kind Nothing <> [zero])
      Just statements -> do
        name <- fresh "trap_body"
        previousCallback <- gets materialCallback
        modify' (\material -> material {materialCallback = True})
        body <- withDescriptorRoot (lowerStatements False statements)
        modify' (\material -> material {materialCallback = previousCallback})
        status <- runtimeName "status"
        let definition = Stmt (Function (MkFishFunction name [FuncUnknownFlag "--no-scope-shadowing"] [] (bodyNE (body <> [builtin "return" [arg (scalarVar status)]]))))
        pure ([definition] <> Traps.install prefix kind (Just name) <> [zero])
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
  P.Break -> lowerLoopJump [] (Stmt Break)
  P.Continue -> do
    actions <- gets (fromMaybe [] . viaNonEmpty head . materialContinueActions)
    lowerLoopJump actions (Stmt Continue)
  where
    lowerConjunction onSuccess left right = do
      first <- lowerStatement True left
      second <- lowerStatement suppressed right
      status <- runtimeName "status"
      let branch = if onSuccess then ifStatements [testEquals (scalarVar status) "0"] second [] else ifStatements [testEquals (scalarVar status) "0"] [] second
      pure (first <> [branch])
    lowerExit name value = do
      (prelude, result) <- maybe (([],) . scalarVar <$> runtimeName "status") lowerScalar value
      remaining <- gets materialReturnDepth
      unwind <- if name == "return" then unwindDescriptors remaining else pure []
      traps <- gets materialTraps
      prefix <- gets materialPrefix
      origin <- diagnosticOriginExpression
      pure (prelude <> unwind <> [if traps && name == "exit" then Traps.exitWithStatusAt prefix origin result else builtin name [arg result]])

withDescriptorRoot :: Materialize a -> Materialize a
withDescriptorRoot action = do
  depth <- gets materialDescriptorDepth
  returnDepth <- gets materialReturnDepth
  loopDepths <- gets materialLoopDescriptorDepths
  deferred <- gets materialDeferCompletion
  modify' (\material -> material {materialDescriptorDepth = 0, materialReturnDepth = 0, materialLoopDescriptorDepths = [], materialDeferCompletion = False})
  result <- action
  modify' (\material -> material {materialDescriptorDepth = depth, materialReturnDepth = returnDepth, materialLoopDescriptorDepths = loopDepths, materialDeferCompletion = deferred})
  pure result

unwindDescriptors :: Int -> Materialize [FishStatement]
unwindDescriptors remaining = do
  depth <- gets materialDescriptorDepth
  prefix <- gets materialPrefix
  pure (replicate (max 0 (depth - remaining)) (Session.request prefix "fd-pop" []))

lowerOwnedRedirects :: Bool -> [P.Redirection] -> P.Statement -> Materialize [FishStatement]
lowerOwnedRedirects suppressed redirects statement = do
  prefix <- gets materialPrefix
  depth <- gets materialDescriptorDepth
  failed <- fresh "redirect_status"
  operations <- traverse lowerOwnedRedirect redirects
  previousDefer <- gets materialDeferCompletion
  let simple = case statement of P.Statement _ (P.Sequence _) -> False; P.Statement _ (P.Conditional {}) -> False; P.Statement _ (P.WhileLoop {}) -> False; P.Statement _ (P.ForLoop {}) -> False; P.Statement _ (P.ArithmeticFor {}) -> False; P.Statement _ (P.Case {}) -> False; P.Statement _ (P.And {}) -> False; P.Statement _ (P.Or {}) -> False; _ -> True
  modify' (\material -> material {materialDescriptorDepth = depth + 1, materialDeferCompletion = previousDefer || simple})
  (prelude, body) <- case statement of
    P.Statement _ (P.Invoke target wordsValue) -> lowerInvokeWords suppressed target wordsValue
    P.Statement _ (P.PrefixedInvoke assignments target wordsValue) -> lowerPrefixedInvoke suppressed assignments target wordsValue
    _ -> ([],) <$> lowerStatement suppressed statement
  modify' (\material -> material {materialDescriptorDepth = depth, materialDeferCompletion = previousDefer})
  failureStatus <- setSourceStatus (scalarVar failed)
  guards <- errexitGuard suppressed
  let apply statements = ifStatements [testEquals (scalarVar failed) "0"] (statements <> [assign [] failed (scalarVar "status")]) []
  pure (prelude <> [Session.request prefix "fd-push" [], assign [SetLocal] failed (scalarVar "status")] <> map apply operations <> [ifStatements [testEquals (scalarVar failed) "0"] (body <> [Session.request prefix "fd-pop" []] <> [guardStatement | simple, guardStatement <- guards]) ([failureStatus, Session.request prefix "fd-pop" []] <> guards)])

lowerOwnedRedirect :: P.Redirection -> Materialize [FishStatement]
lowerOwnedRedirect redirect = do
  prefix <- gets materialPrefix
  (origin, line) <- diagnosticOrigin
  let request operation fields = Session.request prefix operation (map arg fields)
      open fd mode path = do
        (prelude, value) <- lowerScalar path
        pure (prelude <> [request "fd-open" [ExprLiteral origin, ExprLiteral line, ExprLiteral (show fd), ExprLiteral mode, value]])
  case redirect of
    P.OpenDescriptor fd _ endpoint@P.ProcessSubstitution {} -> do
      (prelude, _) <- lowerScalar endpoint
      pure (prelude <> [request "fd-endpoint" [ExprLiteral origin, ExprLiteral line, ExprLiteral (show fd), Session.endpointLease prefix]])
    P.OpenDescriptor fd mode path -> open fd (case mode of P.ReadFile -> "read"; P.WriteFile -> "write"; P.AppendFile -> "append"; P.ReadWriteFile -> "read-write") path
    P.NullDescriptor fd input -> open fd (if input then "read" else "write") (P.Literal "/dev/null")
    P.DuplicateDescriptor target source _ -> pure [request "fd-dup" (map ExprLiteral [origin, line, show target, show source])]
    P.CloseDescriptor fd _ -> pure [request "fd-close" [ExprLiteral (show fd)]]
    P.InputDescriptor fd scalar newline -> do
      (prelude, value) <- lowerScalar scalar
      pure (prelude <> [request "fd-data" [ExprLiteral (show fd), if newline then ExprStringConcat value (ExprLiteral "\n") else value]])

lowerRead :: Bool -> P.ReadOptions -> P.ReadTarget -> Materialize [FishStatement]
lowerRead suppressed options target = do
  prefix <- gets materialPrefix
  ifs <- runtimeName "ifs"
  (origin, line) <- diagnosticOrigin
  captured <- captureStatus
  guards <- errexitGuard suppressed
  let (mode, count) = case target of P.ReadReply _ -> ("reply", 1 :: Int); P.ReadScalars names -> ("scalar", length names); P.ReadArray {} -> ("array", 0)
      fields = map ExprLiteral [origin, line, show (P.readDescriptor options), if P.readRaw options then "1" else "0", P.readDelimiter options, maybe "-1" show (P.readCount options)] <> [scalarVar ifs] <> map ExprLiteral [mode, show count]
      value index = ExprQuotedVariable (VarIndex (prefix <> "session_fields") (IndexSingle (ExprNumLiteral (index + 1))))
      write (index, (storage, name)) = do
        actual <- bindingName name
        pure (Binding.writeBinding (Binding.bindingRuntime prefix actual) storage (value index))
  assignments <- case target of
    P.ReadReply storage -> write (1, (storage, "REPLY"))
    P.ReadScalars names -> concat <$> traverse write (zip [1 ..] names)
    P.ReadArray storage name -> do
      actual <- bindingName name
      pure (arrayWrite storage actual actual [arg (Session.responseValues prefix)])
  pure ([Session.request prefix "read" (map arg fields), captured, ifStatements [testEquals (value 0) "1"] assignments []] <> guards)

lowerDirectRedirects :: Bool -> [P.Redirection] -> P.Statement -> Materialize [FishStatement]
lowerDirectRedirects suppressed redirects statement = do
  needProgram (RequiresPlatformCapability PosixOwnedDescriptors) "Standard descriptor and stable null device operations"
  (prelude, body) <- case statement of
    P.Statement _ (P.Invoke target wordsValue) -> lowerInvokeWords suppressed target wordsValue
    P.Statement _ (P.PrefixedInvoke assignments target wordsValue) -> lowerPrefixedInvoke suppressed assignments target wordsValue
    P.Statement _ P.DeclarationCommand {} -> lift (Left (planDiagnostic "redirect-declaration" "Redirected declarations require their own expansion and local-slot scope" :| []))
    _ -> ([],) <$> lowerStatement suppressed statement
  lowered <- either (\message -> lift (Left (planDiagnostic "redirect-materialization" message :| []))) pure (traverse lowerRedirect redirects)
  pure (prelude <> [Stmt (Begin (bodyNE body) (map RedirectVal lowered))])

lowerHeader :: Text -> P.Statement -> Materialize ([FishStatement], FishExpr TStr)
lowerHeader failed (P.Statement _ (P.ArithmeticCommand site expression bindings)) = do
  result <- arithmeticMaterialization expression bindings
  pure (Arithmetic.arithmeticStatements result <> [ifStatements [testEquals (Arithmetic.arithmeticError result) ""] [] (arithmeticDiagnostic True site result <> [assign [] failed (ExprLiteral "1")])], Arithmetic.arithmeticValue result)
lowerHeader _ _ = lift (Left (planDiagnostic "arithmetic-header" "Arithmetic loop header must own an integer operation" :| []))

lowerLoopJump :: [FishStatement] -> FishStatement -> Materialize [FishStatement]
lowerLoopJump actions jump = do
  target <- gets (viaNonEmpty head . materialLoops)
  result <- maybe (lift (Left (planDiagnostic "loop-control" "Loop control has no owned materialization target" :| []))) pure target
  zero <- setSourceStatus (ExprLiteral "0")
  remaining <- gets (fromMaybe 0 . viaNonEmpty head . materialLoopDescriptorDepths)
  unwind <- unwindDescriptors remaining
  pure (unwind <> actions <> [zero, assign [] result (ExprLiteral "0"), jump])

lowerRedirect :: P.Redirection -> Either Text Redirect
lowerRedirect = \case
  P.DuplicateDescriptor source target input -> pure (MkRedirect (RedirectFD source) (mode input) (RedirectTargetFD target))
  P.CloseDescriptor source input -> pure (MkRedirect (RedirectFD source) (mode input) RedirectClose)
  P.NullDescriptor source input -> pure (MkRedirect (RedirectFD source) (mode input) (RedirectFile (ExprLiteral "/dev/null")))
  P.OpenDescriptor {} -> Left "File opens require an owned descriptor session"
  P.InputDescriptor {} -> Left "Input data requires an owned descriptor session"
  where
    mode input = if input then RedirectIn else RedirectOut

lowerPrefixedInvoke :: Bool -> [(P.Storage, Text, P.Scalar)] -> P.CallTarget -> [P.Word] -> Materialize ([FishStatement], [FishStatement])
lowerPrefixedInvoke suppressed assignments target wordsValue = do
  (prelude, arguments) <- lowerWords wordsValue
  prefix <- gets materialPrefix
  frozen <- forM assignments $ \(_, name, scalar) -> do
    (setup, value) <- lowerScalar scalar
    actual <- bindingName name
    saved <- fresh "prefix_value"
    let installValue operand = [assign [SetLocal, SetExport, SetUnpath] actual operand, assign [SetLocal] (prefix <> "binding_export_" <> actual) (ExprLiteral "--export"), assignList [SetLocal] (prefix <> "binding_environment_" <> actual) (ExprListLiteral [])]
    pure (assign [SetLocal] saved (ExprLiteral ""), setup <> installValue value <> [assign [] saved (scalarVar actual)], installValue (scalarVar saved))
  body <- lowerInvocation suppressed target arguments
  let declarations = [declaration | (declaration, _, _) <- frozen]
      expansion = concat [statements | (_, statements, _) <- frozen]
      installation = concat [statements | (_, _, statements) <- frozen]
  pure (prelude <> declarations <> [Stmt (Begin (bodyNE expansion) [])], [Stmt (Begin (bodyNE (installation <> body)) [])])

lowerInvokeWords :: Bool -> P.CallTarget -> [P.Word] -> Materialize ([FishStatement], [FishStatement])
lowerInvokeWords suppressed target wordsValue = do
  supervised <- gets materialSession
  case (supervised, target, traverse literalWord wordsValue >>= Native.nativeEcho) of
    (False, P.Builtin "echo", Just (newline, output)) -> do
      writer <- directWriter "echo-bytes" [arg (ExprLiteral (T.intercalate " " output <> if newline then "\n" else ""))]
      captured <- captureStatus
      guards <- errexitGuard suppressed
      pure ([], [writer, captured] <> guards)
    _ -> do
      (prelude, arguments) <- lowerWords wordsValue
      body <- lowerInvocation suppressed target arguments
      prefix <- gets materialPrefix
      let endpoint = \case P.OneField P.ProcessSubstitution {} -> True; _ -> False
      pure (prelude, body <> [Session.request prefix "substitution-release" [] | any endpoint wordsValue])

lowerInvocation :: Bool -> P.CallTarget -> [ExprOrRedirect] -> Materialize [FishStatement]
lowerInvocation suppressed target arguments = do
  capture <- captureStatus
  guardStatements <- errexitGuard suppressed
  supervised <- gets materialSession
  prefix <- gets materialPrefix
  case target of
    P.Builtin name | supervised && name `elem` ["printf", "echo"] -> do
      location <- diagnosticArguments
      origin <- diagnosticOriginExpression
      sourceStatus <- runtimeName "status"
      traps <- gets materialTraps
      incoming <- fresh "writer_incoming"
      let terminate = if traps then [assign [SetGlobal] (prefix <> "pending_signal") (ExprLiteral "13"), Traps.exitWithStatusAt prefix origin (scalarVar incoming)] else [Session.request prefix "finish-signal" [arg (ExprLiteral "13")], builtin "exit" [arg (ExprLiteral "141")]]
      pure ([assign [SetLocal] incoming (scalarVar sourceStatus) | traps] <> [Session.request prefix "run" ([arg (ExprLiteral "builtin")] <> location <> [arg (ExprLiteral name)] <> arguments), capture, ifStatements [testEquals (scalarVar sourceStatus) "141"] terminate []] <> guardStatements)
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
    P.Builtin name | name `elem` ["printf", "echo"] -> do
      writer <- directWriter name arguments
      pure ([writer, capture] <> guardStatements)
    P.Builtin name -> pure ([builtin (if name == ":" then "true" else name) arguments, capture] <> guardStatements)
    P.External name -> do
      needProgram (RequiresCommand name) "Explicit external command dispatch"
      helper <- externalHelper name
      (origin, line) <- diagnosticOrigin
      pure ([Stmt (Command helper (map (arg . ExprLiteral) [origin, line] <> arguments)), capture] <> guardStatements)

directWriter :: Text -> [ExprOrRedirect] -> Materialize FishStatement
directWriter name arguments = do
  needNative NativeWrite "Bash output errno and signal semantics"
  prefix <- gets materialPrefix
  helpers <- gets materialHelpers
  unless (NativeRuntime.nativeWriterName prefix `elem` helperNames helpers) $
    modify' (\material -> material {materialHelpers = materialHelpers material <> [NativeRuntime.nativeWriterDefinition prefix]})
  range <- gets materialRange
  pure (NativeRuntime.nativeWriterInvocation prefix range name arguments)

lowerWords :: [P.Word] -> Materialize ([FishStatement], [ExprOrRedirect])
lowerWords values = do
  results <- traverse lowerWord values
  pure (concatMap fst results, concatMap snd results)

lowerWord :: P.Word -> Materialize ([FishStatement], [ExprOrRedirect])
lowerWord = \case
  P.ExpandedWord parts -> do
    frozen <- forM parts $ \part -> do
      let (mode, scalar) = case part of P.QuotedExpansion value -> ("q", value); P.LiteralExpansion value -> ("l", value); P.SplitExpansion value -> ("e", value)
      (prelude, value) <- lowerScalar scalar
      temporary <- fresh "expansion_part"
      pure (prelude <> [assign [SetLocal] temporary value], [arg (ExprLiteral mode), arg (scalarVar temporary)])
    ifs <- runtimeName "ifs"
    needNative NativeExpansion "Composed quote-aware splitting and pathname expansion"
    native <- runtimeName "native"
    temporary <- fresh "expanded_fields"
    pure (concatMap fst frozen <> captureList temporary (nulCaptureStatement (framedPrimitive native "expansion" (arg (scalarVar ifs) : concatMap snd frozen))), [arg (ExprVariable (VarAll temporary))])
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
  P.QuotedArray name (P.Literal "") (P.Literal "") False -> do
    target <- bindingName name
    temporary <- fresh "array_fields"
    pure ([assignList [SetLocal] temporary (ExprVariable (VarAll target))], [arg (ExprVariable (VarAll temporary))])
  P.QuotedArray name before after force -> do
    target <- bindingName name
    (preA, prefix) <- lowerScalar before
    savedPrefix <- fresh "array_prefix"
    (preB, suffix) <- lowerScalar after
    needNative NativeArgv "Quoted array prefix and suffix cardinality"
    native <- runtimeName "native"
    needProgram (RequiresFishFeature NulDelimitedCapture) "Quoted array field transport"
    temporary <- fresh "array_fields"
    let producer = framedPrimitive native "argv" [arg (scalarVar savedPrefix), arg suffix, arg (ExprLiteral (if force then "1" else "0")), arg (ExprVariable (VarAll target))]
    pure (preA <> [assign [SetLocal] savedPrefix prefix] <> preB <> captureList temporary (nulCaptureStatement producer), [arg (ExprVariable (VarAll temporary))])

lowerArrayWrite :: P.Storage -> Text -> [P.Word] -> Bool -> Materialize [FishStatement]
lowerArrayWrite storage name values append = do
  (prelude, arguments) <- lowerWords values
  target <- bindingName name
  status <- setSourceStatus (ExprLiteral "0")
  grouped <- gets materialAssignment
  let operands = [arg (ExprVariable (VarAll target)) | append] <> arguments
  pure (prelude <> arrayWrite storage target target operands <> [status | not grouped])

arrayWrite :: P.Storage -> Text -> Text -> [ExprOrRedirect] -> [FishStatement]
arrayWrite storage name target values =
  let write flags = builtin "set" (map (arg . ExprLiteral) (flags <> ["--unexport", "--unpath", target]) <> values)
   in case storage of
        P.Global -> [write ["--global"]]
        P.CallerGlobal _ -> [write ["--global"]]
        P.Local -> [write []]
        _ -> [ifStatements [builtin "set" [arg (ExprLiteral "--query"), arg (ExprLiteral name)]] [write []] [write ["--global"]]]

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
      consumer = Stmt (Command native [arg (ExprLiteral "--abi"), arg (ExprLiteral (show runtimeABI)), arg (ExprLiteral program)])
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
  P.PlatformBytes darwinBytes linuxBytes -> do
    temporary <- fresh "platform_bytes"
    needNative NativePlatformBytes "Platform-dependent Bash ANSI quoted bytes"
    helper <- runtimeName "native"
    let hexBytes = T.concat . map (\byte -> let digits = toText (showHex byte "") in T.justifyRight 2 '0' digits) . BS.unpack
    pure (captureList temporary (nulCapture helper "bytes-platform" (map (ExprLiteral . hexBytes) [darwinBytes, linuxBytes])), scalarVar temporary)
  P.AppendValue name value -> do
    (prelude, rhs) <- lowerScalar value
    saved <- fresh "append_rhs"
    target <- bindingName name
    pure (prelude <> [assign [SetLocal] saved rhs], ExprStringConcat (scalarVar target) (scalarVar saved))
  P.ParameterPatternTransform operation scalar patternValue -> do
    (prelude, value) <- lowerScalar scalar
    subject <- fresh "parameter_subject"
    (patternPrelude, parts) <- lowerPatternParts patternValue
    needNative NativePatternParts "Quote-aware byte parameter pattern"
    helper <- runtimeName "native"
    temporary <- fresh "parameter_transform"
    needProgram (RequiresFishFeature NulDelimitedCapture) "Parameter byte transport"
    let fields = [ExprLiteral operation, scalarVar subject] <> concatMap (\(active, part) -> [ExprLiteral (if active then "1" else "0"), part]) parts
    pure (prelude <> [assign [SetLocal] subject value] <> patternPrelude <> captureList temporary (nulCapture helper "pattern-parts" fields), scalarVar temporary)
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
  P.ArrayElement name index -> do
    target <- bindingName name
    pure ([], ExprQuotedVariable (VarIndex target (IndexSingle (ExprNumLiteral (index + 1)))))
  P.ArrayLength name -> do
    target <- bindingName name
    pure ([], ExprQuotedCommandSubst (builtin "count" [arg (ExprVariable (VarAll target))] :| []))
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
  P.LastBackgroundPid -> do name <- runtimeName "last_pid"; pure ([], scalarVar name)
  P.LastStatus -> ([],) . scalarVar <$> runtimeName "status"
  P.Concat values | Just literals <- traverse (\case P.Literal value -> Just value; _ -> Nothing) values -> pure ([], ExprLiteral (mconcat literals))
  P.Concat values -> do
    parts <- forM values $ \value -> do
      (prelude, expression) <- lowerScalar value
      temporary <- fresh "scalar_part"
      pure (prelude <> [assign [SetLocal] temporary expression], scalarVar temporary)
    pure (concatMap fst parts, foldl' ExprStringConcat (ExprLiteral "") (map snd parts))
  P.ProcessSubstitution direction region -> do
    prefix <- gets materialPrefix
    needNative NativePipePaths "Inherited pipe pathname endpoints"
    needProgram (RequiresPlatformCapability PipeDescriptorPaths) "Owned process substitution pipe descriptors"
    (prelude, frames, cleanup) <- sessionStage region
    invocation <- projectedSessionRequest "substitution" (arg (ExprLiteral (case direction of P.ProcessInput -> "input"; P.ProcessOutput -> "output")) : frames)
    temporary <- fresh "process_endpoint"
    pure (prelude <> [invocation, assign [SetLocal] temporary (Session.endpointPath prefix)] <> cleanup, scalarVar temporary)
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
    traps <- gets materialTraps
    prefix <- gets materialPrefix
    origin <- diagnosticOriginExpression
    let terminate = if traps then Traps.exitWithStatusAt prefix origin (ExprLiteral "1") else builtin "exit" [arg (ExprLiteral "1")]
    pure
      ( Arithmetic.arithmeticStatements result
          <> [ ifStatements
                 [testEquals (Arithmetic.arithmeticError result) ""]
                 []
                 (arithmeticDiagnostic False site result <> [terminate])
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
  supervised <- gets materialSession
  unless supervised (needNative NativeExec "Source-located external exec failures")
  helper <- runtimeName ("external_" <> T.intercalate "_" [toText (showHex (ord character) "") | character <- toString executable])
  existing <- gets (helperNames . materialHelpers)
  unless (helper `elem` existing) $ do
    temporary <- fresh "environment"
    prefix <- gets materialPrefix
    names <- gets materialBindings
    bindings <- traverse (fmap (Binding.bindingRuntime prefix) . bindingName) (S.toAscList (S.delete "IFS" names))
    let invocationArguments =
          [ arg (ExprQuotedVariable (VarIndex "argv" (IndexSingle (ExprNumLiteral 1)))),
            arg (ExprQuotedVariable (VarIndex "argv" (IndexSingle (ExprNumLiteral 2)))),
            arg (ExprLiteral executable),
            arg (ExprVariable (VarIndex "argv" (IndexRange (Just (ExprNumLiteral 3)) Nothing)))
          ]
        invocation =
          if supervised
            then Session.request prefix "run" (arg (ExprLiteral "external-site") : invocationArguments)
            else Stmt (Decorated DecCommand (CommandExpr (scalarVar (NativeRuntime.runtimePathName prefix)) (map (arg . ExprLiteral) ["--abi", "2", "exec-site"] <> invocationArguments)))
        body = Binding.environmentShadows temporary bindings <> [invocation]
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
  supervised <- gets materialSession
  traps <- gets materialTraps
  previousHelpers <- gets materialHelpers
  definitions <- fmap concat $ forM (M.toAscList (P.childFunctions region)) $ \(name, body) ->
    lowerStatement False (P.Statement (P.childRange region) (P.DefineFunction name body))
  bodyValue <- withDescriptorRoot (lowerStatements suppressed (P.childStatements region))
  let body = [assign [SetGlobal] (runtimePrefix <> "source_origin") (ExprLiteral (maybe "<input>" (srcFile . rangeStart) (P.childRange region))) | traps] <> bodyValue
  helpers <- gets materialHelpers
  modify' (\s -> s {materialHelpers = previousHelpers})
  ownedBindings <- gets materialBindings
  actualBindings <- S.fromList <$> traverse bindingName (S.toAscList (P.childVariables region <> (if P.childNeedsEnvironment region then ownedBindings else mempty)))
  ownerPrefix <- gets materialPrefix
  actualArrays <- S.fromList <$> traverse bindingName (S.toAscList (P.childArrays region))
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
      (Child.materializeChild prefix mode runtime bindings actualArrays (helperClosure helpers (body <> definitions <> [Traps.exitWithStatus runtimePrefix (scalarVar (runtimePrefix <> "status")) | traps]) <> definitions) body)
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
    <> privateGuardsWithCaptured guardFailure captured prefix incoming markerGuards
  where
    markerGuards = foldr (\name next -> guardWhen (builtin "set" [arg (ExprLiteral "--query"), arg (ExprLiteral name)]) ("reserved launcher metadata " <> name) next) (callerGuards (callerContract cfg) execution) ["MONK_LAUNCH_ORIGINAL", "MONK_LAUNCH_WRAPPER"]
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

privateGuardsWithCaptured :: (Text -> [FishStatement]) -> [Text] -> Text -> Text -> [FishStatement] -> [FishStatement]
privateGuardsWithCaptured failure captured prefix incoming next =
  foldr (\(predicate, message) continuation -> [ifStatements [predicate] (failure message) continuation]) next (privateGuardChecks captured prefix incoming)

privateGuardChecks :: [Text] -> Text -> Text -> [(FishStatement, Text)]
privateGuardChecks captured prefix incoming =
  [ (namesMatch (builtin "functions" [arg (ExprLiteral "--all"), arg (ExprLiteral "--names")]) [], "private function namespace is occupied"),
    ( namesMatch
        (builtin "set" [arg (ExprLiteral "--names")])
        [PipeTo [] (builtin "string" [arg (ExprLiteral "match"), arg (ExprLiteral "--invert"), arg (ExprLiteral "--regex"), arg (ExprLiteral "--"), arg (ExprLiteral ("^(" <> T.intercalate "|" excluded <> ")$"))]) | not (null excluded)],
      "private variable namespace is occupied"
    )
  ]
  where
    excluded = filter (not . T.null) (incoming : captured)
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
  [ifStatements [predicate] (standaloneFailure message) [] | (predicate, message) <- privateGuardChecks [] prefix ""]
    <> standaloneBindingGuards bindings
    <> next

standaloneFailure :: Text -> [FishStatement]
standaloneFailure message =
  [ builtin "printf" [arg (ExprLiteral "%s\n"), arg (ExprLiteral ("monk: runtime contract failed: " <> message)), RedirectVal (MkRedirect RedirectStdout RedirectOut (RedirectTargetFD 2))],
    builtin "exit" [arg (ExprLiteral "125")]
  ]

-- These checks can terminate their owned standalone process directly. Keeping
-- their continuation flat makes each boundary obligation visible only once.
standaloneBindingGuards :: S.Set Text -> [FishStatement]
standaloneBindingGuards bindings = concatMap bindingGuard (S.toAscList (S.delete "IFS" bindings))
  where
    rejectWhen predicate message = ifStatements [predicate] (standaloneFailure message) []
    require predicate message = ifStatements [predicate] [] (standaloneFailure message)
    query flags name = builtin "set" (map (arg . ExprLiteral) ("--query" : flags <> [name]))
    bindingGuard name =
      [ rejectWhen (query ["--universal"] name) ("universal binding " <> name),
        rejectWhen (query ["--path"] name) ("path binding " <> name),
        ifStatements [query [] name] (shape name) []
      ]
    shape name =
      let count = ExprQuotedCommandSubst (builtin "count" [arg (ExprVariable (VarAll name))] :| [])
       in [ require (testEquals count "1") ("non-scalar binding " <> name),
            rejectWhen (query ["--local"] name) ("local binding " <> name),
            require (query ["--export"] name) ("non-environment binding " <> name)
          ]

sessionEnvironmentGuards :: [FishStatement]
sessionEnvironmentGuards =
  [ ifStatements [builtin "set" [arg (ExprLiteral "--query"), arg (ExprLiteral name)]] (standaloneFailure ("private session binding " <> name)) []
  | name <- ["MONK_SESSION_SOCKET", "MONK_SESSION_TOKEN", "MONK_SESSION_REPLY", "MONK_SESSION_FDS"]
  ]

-- Arrays are owned by the translated program. An ambient scalar with the same
-- name carries export attributes which are outside this initial contract.
standaloneArrayGuards :: S.Set Text -> [FishStatement]
standaloneArrayGuards = concatMap checkArray . S.toAscList
  where
    checkArray name = [ifStatements [builtin "set" [arg (ExprLiteral "--query"), arg (ExprLiteral name)]] (standaloneFailure ("preexisting array binding " <> name)) []]

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
          guardUnless (testEquals cardinality "1") ("non-scalar binding " <> name) $
            guardWhen (query (scopeFlags <> ["--path"]) name) ("path binding " <> name) $
              (if exported == ExportedBinding then guardUnless else guardWhen)
                (query (scopeFlags <> ["--export"]) name)
                ("export attribute mismatch for " <> name)
                remaining

literalWord :: P.Word -> Maybe Text
literalWord (P.OneField (P.Literal value)) = Just value
literalWord _ = Nothing

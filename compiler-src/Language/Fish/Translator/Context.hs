{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Materialization scope state, diagnostics, helper ownership and boundary effects.
module Language.Fish.Translator.Context
  ( LoopFrame (..),
    Materialization (..),
    Materialize,
    diagnosticOrigin,
    diagnosticArguments,
    diagnosticSite,
    diagnosticOriginExpression,
    loadModule,
    planDiagnostic,
    choosePrefix,
    fresh,
    runtimeName,
    bindingName,
    registerHelpers,
    needProgram,
    mergeRequirement,
    needNative,
    setSourceStatus,
    captureStatus,
    errexitGuard,
    withDescriptorRoot,
    withControlScope,
    unwindDescriptors,
    arithmeticMaterialization,
    arrayWrite,
    externalHelper,
    SessionEmitter (..),
    projectedSessionRequest,
  )
where

import Control.Monad.State.Strict (gets)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Language.Bash.Arithmetic.Plan (ArithmeticExpr)
import Language.Bash.Plan qualified as P
import Language.Bash.Plan.Control qualified as Control
import Language.Fish.DSL.Argument
import Language.Fish.DSL.Internal
import Language.Fish.Translator.ArithmeticPlan qualified as Arithmetic
import Language.Fish.Translator.Binding qualified as Binding
import Language.Fish.Translator.Boundary (helperNames)
import Language.Fish.Translator.HelperRegistry qualified as Helpers
import Language.Fish.Translator.Identifier (compilerIdentifier)
import Language.Fish.Translator.NativeRuntime qualified as NativeRuntime
import Language.Fish.Translator.Session qualified as Session
import Language.Fish.Translator.Session.Request qualified as Request
import Language.Fish.Translator.Statement
import Language.Fish.Translator.Traps qualified as Traps
import Monk.Translation.Types
import Numeric (showHex)
import Prelude hiding (exitFailure, gets, isPrefixOf)

-- | One stack entry owns every operation needed by a loop exit.
type role LoopFrame nominal

data LoopFrame scope = LoopFrame
  { loopTarget :: Control.LoopTarget scope,
    loopResult :: Identifier,
    loopContinue :: [FishStatement],
    loopDescriptorDepth :: Int
  }

type role Materialization nominal

data Materialization scope = MkMaterialization
  { materialRoot :: Control.Root scope,
    materialPrefix :: Text,
    materialNext :: Int,
    materialRequirements :: M.Map RuntimeProgram (NonEmpty RequirementUse),
    materialHelpers :: Helpers.Registry,
    materialConfig :: TranslateConfig,
    materialIdentity :: Text,
    materialSuppressed :: Bool,
    materialAssignment :: Bool,
    materialRange :: Maybe SourceRange,
    materialDiagnostics :: [Diagnostic],
    materialBindings :: S.Set Text,
    materialSeparate :: Bool,
    materialModules :: M.Map FilePath FishStatement,
    materialLoops :: [LoopFrame scope],
    materialErrexitRelevant :: Bool,
    materialSession :: Bool,
    materialDescriptorDepth :: Int,
    materialReturnDepth :: Int,
    materialTraps :: Bool,
    materialDeferCompletion :: Bool,
    materialCallback :: Bool
  }

type Materialize scope = StateT (Materialization scope) (Either (NonEmpty Diagnostic))

diagnosticOrigin :: Materialize scope (Text, Text)
diagnosticOrigin = do
  range <- gets materialRange
  pure (maybe "<input>" (srcFile . rangeStart) range, maybe "1" (show . srcLine . rangeStart) range)

-- Callback diagnostics belong to the execution site, not the trap declaration.
-- Handler line offsets are bounded compiler metadata, not Bash arithmetic.
diagnosticArguments :: Materialize scope [ExprOrRedirect]
diagnosticArguments = do
  callback <- gets materialCallback
  prefix <- gets materialPrefix
  (origin, line) <- diagnosticOrigin
  range <- gets materialRange
  let offset = maybe 0 (subtract 1 . srcLine . rangeStart) range
  pure $
    if callback
      then [arg (scalarVar (compilerIdentifier (prefix <> "callback_origin"))), if offset == 0 then arg (scalarVar (compilerIdentifier (prefix <> "callback_line"))) else arg (ExprMath (scalarVar (compilerIdentifier (prefix <> "callback_line")) :| [ExprLiteral "+", ExprLiteral (show offset)]))]
      else map (arg . ExprLiteral) [origin, line]

diagnosticOriginExpression :: Materialize scope (FishExpr TStr)
diagnosticOriginExpression = do
  callback <- gets materialCallback
  prefix <- gets materialPrefix
  (origin, _) <- diagnosticOrigin
  pure (if callback then scalarVar (compilerIdentifier (prefix <> "callback_origin")) else ExprLiteral origin)

loadModule :: Bool -> Text -> FilePath -> [FishStatement]
loadModule sourceable root path =
  [ builtin "source" [arg (ExprStringConcat (scalarVar (compilerIdentifier root)) (ExprLiteral ("/" <> toText path)))],
    ifStatements [testEquals (scalarVar "status") "0"] [] [if sourceable then Stmt (ReturnScalar (ExprLiteral "125")) else builtin "exit" [arg (ExprLiteral "125")]]
  ]

planDiagnostic :: Text -> Text -> Diagnostic
planDiagnostic code message = MkDiagnostic (MkDiagnosticCode ("monk.semantic." <> code)) PhaseTranslate DiagnosticError Unsafe message Nothing

choosePrefix :: S.Set Text -> Int -> Text
choosePrefix names index =
  let prefix = "__monk_plan_" <> show index <> "_"
   in if any (prefix `isPrefixOf`) (S.toList names) then choosePrefix names (index + 1) else prefix
  where
    isPrefixOf prefix value = take (length (toString prefix)) (toString value) == toString prefix

fresh :: Text -> Materialize scope Identifier
fresh role = do
  prefix <- gets materialPrefix
  index <- gets materialNext
  modify' (\s -> s {materialNext = index + 1})
  pure (compilerIdentifier (prefix <> role <> "_" <> show index))

runtimeName :: Text -> Materialize scope Identifier
runtimeName role = compilerIdentifier . (<> role) <$> gets materialPrefix

bindingName :: Text -> Materialize scope Identifier
bindingName "IFS" = runtimeName "ifs"
bindingName name = pure (compilerIdentifier name)

registerHelpers :: [FishStatement] -> Materialize scope ()
registerHelpers statements = do
  current <- gets materialHelpers
  updated <- either (\message -> lift (Left (planDiagnostic "helper-identity" message :| []))) pure (Helpers.intern statements current)
  modify' (\material -> material {materialHelpers = updated})

needProgram :: RuntimeProgram -> Text -> Materialize scope ()
needProgram program reason = do
  range <- gets materialRange
  mergeRequirement (MkRuntimeRequirement program (MkRequirementUse reason range :| []))

mergeRequirement :: RuntimeRequirement -> Materialize scope ()
mergeRequirement (MkRuntimeRequirement program uses) = do
  range <- gets materialRange
  let located use = use {requirementRange = requirementRange use <|> range}
  modify' (\s -> s {materialRequirements = M.insertWith (flip (<>)) program (fmap located uses) (materialRequirements s)})

needNative :: NativeOperation -> Text -> Materialize scope ()
needNative operation reason = do
  mergeRequirement (nativeRuntimeRequirement operation reason)
  needProgram (RequiresFishFeature FunctionScopeSharing) "Private native runtime environment isolation"
  needProgram (RequiresFishFeature NulDelimitedCapture) "Native provider pathname byte preservation"
  name <- runtimeName "native"
  helpers <- gets materialHelpers
  unless (operation `elem` [NativeExec, NativeWrite] || identifierText name `elem` Helpers.names helpers) $ do
    bindings <- gets materialBindings
    prefix <- gets materialPrefix
    registerHelpers [NativeRuntime.nativeRuntimeDefinition prefix bindings]

setSourceStatus :: FishExpr TStr -> Materialize scope FishStatement
setSourceStatus value = do
  name <- runtimeName "status"
  pure (assign [] name value)

captureStatus :: Materialize scope FishStatement
captureStatus = setSourceStatus (scalarVar "status")

errexitGuard :: Bool -> Materialize scope [FishStatement]
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

-- Opening an existential body changes the lexical index and resets loop
-- evidence. Ordinary finite compiler state is preserved across the boundary.
withControlScope :: Control.Root inner -> Materialize inner value -> Materialize outer value
withControlScope root action = do
  previous <- get
  let inner = previous {materialRoot = root, materialLoops = []}
  (result, final) <- lift (runStateT action inner)
  put final {materialRoot = materialRoot previous, materialLoops = materialLoops previous}
  pure result

withDescriptorRoot :: Control.Root inner -> Materialize inner value -> Materialize outer value
withDescriptorRoot root action = withControlScope root $ do
  depth <- gets materialDescriptorDepth
  returnDepth <- gets materialReturnDepth
  deferred <- gets materialDeferCompletion
  modify' (\material -> material {materialDescriptorDepth = 0, materialReturnDepth = 0, materialDeferCompletion = False})
  result <- action
  modify' (\material -> material {materialDescriptorDepth = depth, materialReturnDepth = returnDepth, materialDeferCompletion = deferred})
  pure result

unwindDescriptors :: Int -> Materialize scope [FishStatement]
unwindDescriptors remaining = do
  depth <- gets materialDescriptorDepth
  prefix <- gets materialPrefix
  pure (replicate (max 0 (depth - remaining)) (Session.request prefix Request.FdPop))

arithmeticMaterialization :: ArithmeticExpr -> M.Map Text P.Storage -> Materialize scope Arithmetic.ArithmeticMaterialization
arithmeticMaterialization expression bindings = do
  native <- runtimeName "native"
  prefix <- fresh "arithmetic"
  ownerPrefix <- gets materialPrefix
  ifs <- runtimeName "ifs"
  let actualName name = if name == "IFS" then ifs else compilerIdentifier name
      getter "#" = ExprQuotedCommandSubst (builtin "count" [arg (ExprVariable (VarAll "argv"))] :| [])
      getter name = scalarVar (actualName name)
      setter name = Binding.writeBinding (Binding.bindingRuntime ownerPrefix (identifierText (actualName name))) (M.findWithDefault P.Global name bindings)
  result <- either (\message -> lift (Left (planDiagnostic "arithmetic-materialization" message :| []))) pure (Arithmetic.materializeArithmetic (identifierText native) (identifierText prefix) getter setter expression)
  unless (null (Arithmetic.arithmeticRequirements result)) (needNative NativeInteger "Bash signed-64-bit integer primitives")
  existing <- gets (Helpers.names . materialHelpers)
  let additions = [helper | helper <- Arithmetic.arithmeticHelpers result, all (`notElem` existing) (helperNames [helper])]
  registerHelpers additions
  traverse_ mergeRequirement (Arithmetic.arithmeticRequirements result)
  pure result

externalHelper :: Text -> Materialize scope Text
externalHelper executable = do
  needProgram (RequiresFishFeature FunctionScopeSharing) "Owned external environment projection"
  supervised <- gets materialSession
  unless supervised (needNative NativeExec "Source-located external exec failures")
  helper <- runtimeName ("external_" <> T.intercalate "_" [toText (showHex (ord character) "") | character <- toString executable])
  existing <- gets (Helpers.names . materialHelpers)
  unless (identifierText helper `elem` existing) $ do
    temporary <- fresh "environment"
    prefix <- gets materialPrefix
    names <- gets materialBindings
    bindings <- traverse (fmap (Binding.bindingRuntime prefix . identifierText) . bindingName) (S.toAscList (S.delete "IFS" names))
    let origin = ExprQuotedVariable (VarIndex "argv" (IndexSingle (ExprNumLiteral 1)))
        line = ExprQuotedVariable (VarIndex "argv" (IndexSingle (ExprNumLiteral 2)))
        arguments = ExprVariable (VarIndex "argv" (IndexRange (Just (ExprNumLiteral 3)) Nothing))
        invocation =
          if supervised
            then Session.request prefix (Request.Run (Request.singleBody (Request.ExternalSiteStage (Request.Site origin line) (ExprLiteral executable) [SomeArgument (ListArgument arguments)])))
            else Stmt (Decorated DecCommand (CommandExpr (variableExecutable (compilerIdentifier (NativeRuntime.runtimePathName prefix))) (map (arg . ExprLiteral) ["--abi", "2", "exec-site"] <> map arg [origin, line, ExprLiteral executable] <> [arg arguments])))
        body = Binding.environmentShadows (identifierText temporary) bindings <> [invocation]
        definition = Stmt (Function (MkFishFunction (identifierText helper) [FuncUnknownFlag "--no-scope-shadowing"] [] (bodyNE body)))
    registerHelpers [definition]
  pure (identifierText helper)

arrayWrite :: P.Storage -> Text -> Text -> [ExprOrRedirect] -> [FishStatement]
arrayWrite storage name target values =
  let write flags = builtin "set" (map (arg . ExprLiteral) (flags <> ["--unexport", "--unpath", target]) <> values)
   in case storage of
        P.Global -> [write ["--global"]]
        P.CallerGlobal _ -> [write ["--global"]]
        P.Local -> [write []]
        _ -> [ifStatements [builtin "set" [arg (ExprLiteral "--query"), arg (ExprLiteral name)]] [write []] [write ["--global"]]]

-- Complete request templates are applied only after their owned operands.
newtype SessionEmitter = SessionEmitter {emitSessionRequest :: forall operation. Request.Request operation -> FishStatement}

projectedSessionRequest :: Materialize scope SessionEmitter
projectedSessionRequest = do
  temporary <- fresh "session_environment"
  prefix <- gets materialPrefix
  names <- gets materialBindings
  bindings <- traverse (fmap (Binding.bindingRuntime prefix . identifierText) . bindingName) (S.toAscList (S.delete "IFS" names))
  pure (SessionEmitter (\request -> Stmt (Begin (bodyNE (Binding.environmentShadows (identifierText temporary) bindings <> [Session.request prefix request])) [])))

-- Typed diagnostic locations preserve callback-time origin and line arithmetic.
diagnosticSite :: Materialize scope Request.Site
diagnosticSite = do
  callback <- gets materialCallback
  prefix <- gets materialPrefix
  (origin, line) <- diagnosticOrigin
  range <- gets materialRange
  let offset = maybe 0 (subtract 1 . srcLine . rangeStart) range
      callbackLine = if offset == 0 then scalarVar (compilerIdentifier (prefix <> "callback_line")) else ExprQuotedCommandSubst (builtin "math" (map arg [scalarVar (compilerIdentifier (prefix <> "callback_line")), ExprLiteral "+", ExprLiteral (show offset)]) :| [])
  pure (if callback then Request.Site (scalarVar (compilerIdentifier (prefix <> "callback_origin"))) callbackLine else Request.Site (ExprLiteral origin) (ExprLiteral line))

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Whole materialized artifacts own their bound configuration, entry, source
-- members and requirements. Admission cannot be detached from that payload.
module Monk.Compiler.Artifact
  ( Phase (..),
    Target,
    Entry,
    Provider,
    Context,
    Artifact,
    withContext,
    materializeArtifact,
    admitArtifact,
    loaderArtifact,
    ArtifactView,
    entryView,
    memberViews,
    viewArtifact,
    viewScript,
    viewStatistics,
    artifactConfig,
    artifactEntry,
    artifactMembers,
    artifactDiagnostics,
    artifactRequirements,
    artifactStatistics,
    artifactMemberStatistics,
    sameArtifact,
  )
where

import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import GHC.Show (Show (showsPrec))
import Language.Bash.Plan qualified as P
import Language.Bash.Plan.Control qualified as Control
import Language.Bash.Plan.Effects qualified as Effects
import Language.Fish.DSL.Internal
import Language.Fish.Translator.Boundary
import Language.Fish.Translator.Context
import Language.Fish.Translator.Directory qualified as Directory
import Language.Fish.Translator.HelperRegistry qualified as Helpers
import Language.Fish.Translator.Identifier (compilerIdentifier)
import Language.Fish.Translator.Materialize (lowerStatements)
import Language.Fish.Translator.Native qualified as Native
import Language.Fish.Translator.NativeRuntime qualified as NativeRuntime
import Language.Fish.Translator.Session qualified as Session
import Language.Fish.Translator.Statement
import Language.Fish.Translator.Statistics (materializationStatistics)
import Language.Fish.Translator.Traps qualified as Traps
import Monk.Compiler.Context
import Monk.Runtime.NativeTarget (runtimeABI)
import Monk.Translation.Types
import System.FilePath (isAbsolute)
import Prelude hiding (exitFailure, first, force, gets, isPrefixOf, one, second)

data Payload = Payload
  { payloadEntry :: Script,
    payloadMembers :: Map FilePath Script,
    payloadDiagnostics :: [Diagnostic],
    payloadRequirements :: [RuntimeRequirement],
    payloadStatistics :: TranslationStatistics,
    payloadMemberStatistics :: Map FilePath TranslationStatistics,
    payloadPrefix :: Text,
    payloadRuntime :: Text
  }
  deriving stock (Show, Eq)

type role Artifact nominal nominal nominal nominal nominal

data Artifact owner target entry provider (phase :: Phase) where
  DraftArtifact :: Context owner target entry provider -> Payload -> Artifact owner target entry provider Draft
  AdmittedArtifact :: Context owner target entry provider -> Payload -> Artifact owner target entry provider Admitted

instance Show (Artifact owner target entry provider phase) where
  showsPrec precedence artifact = showsPrec precedence (configOf artifact, payloadOf artifact)

draftArtifact :: P.OwnedPlan owner target entry provider Normalized -> Text -> Text -> Script -> Map FilePath Script -> [Diagnostic] -> [RuntimeRequirement] -> Artifact owner target entry provider Draft
draftArtifact normalized = draftContext (P.normalizedContext normalized)

draftContext :: Context owner target entry provider -> Text -> Text -> Script -> Map FilePath Script -> [Diagnostic] -> [RuntimeRequirement] -> Artifact owner target entry provider Draft
draftContext context prefix runtime entry members diagnostics requirements =
  DraftArtifact context (Payload entry members diagnostics requirements (statistics entry) (fmap statistics members) prefix runtime)
  where
    statistics = materializationStatistics prefix runtime

-- | Capability checks cover the one requirement registry accumulated while
-- constructing the entry, modules and embedded child bodies. Statistics force
-- structural traversal of that complete artifact before admission succeeds.
admitArtifact :: Artifact owner target entry provider Draft -> Either (NonEmpty Diagnostic) (Artifact owner target entry provider Admitted)
admitArtifact (DraftArtifact context payload) = do
  let config = contextConfig context
  traverse_ (checkRequirement config . requiredProgram) (payloadRequirements payload)
  case entryMode config of
    Standalone -> pure ()
    Sourceable -> when (executionStrategyFor (payloadRequirements payload) == SupervisedExecution) (Left (diagnostic "session-entry" "Session effects require standalone execution" :| []))
  payloadStatistics payload `seq` traverse_ (\statistics -> statistics `seq` pure ()) (M.elems (payloadMemberStatistics payload))
  pure (AdmittedArtifact context payload)
  where
    requiredProgram (MkRuntimeRequirement program _) = program

-- | A loader is the sole post-admission transformation. Its new operation is
-- fixed here, so it cannot add arbitrary effects while retaining requirements.
-- The original generation entry and members remain owned by the new draft.
loaderArtifact :: FilePath -> Artifact owner target entry provider Admitted -> Either (NonEmpty Diagnostic) (Artifact owner target entry provider Draft)
loaderArtifact path (AdmittedArtifact context payload) = do
  unless (isAbsolute path && not (T.any (== '\0') (toText path))) (Left (diagnostic "bundle-loader-path" "A bundle loader needs an absolute NUL-free generation entry" :| []))
  let entry = MkScript [Stmt (Decorated DecBuiltin (Command "source" [ExprVal (ExprLiteral (toText path)), ExprVal (ExprVariable (VarAll "argv"))]))]
      members = M.insert path (payloadEntry payload) (payloadMembers payload)
  pure (draftContext context (payloadPrefix payload) (payloadRuntime payload) entry members (payloadDiagnostics payload) (payloadRequirements payload))

-- | A sealed projection is an owned payload key, never an independently
-- supplied script/statistics pair. Constructors are private to this module.
data ViewKey = EntryKey | MemberKey FilePath
  deriving stock (Show, Eq)

type role ArtifactView nominal nominal nominal nominal

data ArtifactView owner target entry provider = ArtifactView (Artifact owner target entry provider Admitted) ViewKey

instance Show (ArtifactView owner target entry provider) where
  showsPrec precedence (ArtifactView artifact key) = showsPrec precedence (artifact, key)

entryView :: Artifact owner target entry provider Admitted -> ArtifactView owner target entry provider
entryView artifact = ArtifactView artifact EntryKey

memberViews :: Artifact owner target entry provider Admitted -> Map FilePath (ArtifactView owner target entry provider)
memberViews artifact = M.mapWithKey (\key _ -> ArtifactView artifact (MemberKey key)) (artifactMembers artifact)

viewArtifact :: ArtifactView owner target entry provider -> Artifact owner target entry provider Admitted
viewArtifact (ArtifactView artifact _) = artifact

viewScript :: ArtifactView owner target entry provider -> Script
viewScript (ArtifactView artifact EntryKey) = artifactEntry artifact
viewScript (ArtifactView artifact (MemberKey key)) =
  -- This key is allocated exclusively by mapWithKey over this same immutable
  -- artifact. The total fallback keeps inspection free of partial functions.
  M.findWithDefault (artifactEntry artifact) key (artifactMembers artifact)

viewStatistics :: ArtifactView owner target entry provider -> TranslationStatistics
viewStatistics (ArtifactView artifact EntryKey) = artifactStatistics artifact
viewStatistics (ArtifactView artifact (MemberKey key)) = M.findWithDefault (artifactStatistics artifact) key (artifactMemberStatistics artifact)

checkRequirement :: TranslateConfig -> RuntimeProgram -> Either (NonEmpty Diagnostic) ()
checkRequirement config = \case
  RequiresFishFeature feature -> unless (profileSupportsFishFeature (targetProfile config) feature) (unsupported (fishFeatureName feature))
  RequiresPlatformCapability capability -> unless (profileSupportsPlatformCapability (targetProfile config) capability) (unsupported (platformCapabilityName capability))
  RequiresCommand _ -> pure ()
  RequiresNativeRuntime abi profile _ -> unless (abi == runtimeABI && profile == targetProfile config) (Left (diagnostic "native-runtime-capability" "Native runtime ABI/profile is incompatible" :| []))
  where
    unsupported feature = Left (diagnostic "target-capability" ("Target profile does not support " <> feature) :| [])

diagnostic :: Text -> Text -> Diagnostic
diagnostic code message = MkDiagnostic (MkDiagnosticCode ("monk.semantic." <> code)) PhaseTranslate DiagnosticError Unsafe message Nothing

configOf :: Artifact owner target entry provider phase -> TranslateConfig
configOf artifact = case artifact of
  DraftArtifact context _ -> contextConfig context
  AdmittedArtifact context _ -> contextConfig context

payloadOf :: Artifact owner target entry provider phase -> Payload
payloadOf (DraftArtifact _ payload) = payload
payloadOf (AdmittedArtifact _ payload) = payload

artifactConfig :: Artifact owner target entry provider Admitted -> TranslateConfig
artifactConfig = configOf

artifactEntry :: Artifact owner target entry provider Admitted -> Script
artifactEntry = payloadEntry . payloadOf

artifactMembers :: Artifact owner target entry provider Admitted -> Map FilePath Script
artifactMembers = payloadMembers . payloadOf

artifactDiagnostics :: Artifact owner target entry provider Admitted -> [Diagnostic]
artifactDiagnostics = payloadDiagnostics . payloadOf

artifactRequirements :: Artifact owner target entry provider Admitted -> [RuntimeRequirement]
artifactRequirements = payloadRequirements . payloadOf

artifactStatistics :: Artifact owner target entry provider Admitted -> TranslationStatistics
artifactStatistics = payloadStatistics . payloadOf

artifactMemberStatistics :: Artifact owner target entry provider Admitted -> Map FilePath TranslationStatistics
artifactMemberStatistics = payloadMemberStatistics . payloadOf

sameArtifact :: Artifact owner target entry provider phase -> Artifact otherOwner otherTarget otherEntry otherProvider otherPhase -> Bool
sameArtifact left right = configOf left == configOf right && payloadOf left == payloadOf right

materializeArtifact :: Bool -> P.OwnedPlan owner target entry provider Normalized -> Either (NonEmpty Diagnostic) (Artifact owner target entry provider Draft)
materializeArtifact separate normalized =
  P.withScopedBody (P.normalizedBody normalized) (materializeRootArtifact separate normalized)

materializeRootArtifact :: Bool -> P.OwnedPlan owner target entry provider Normalized -> Control.Root scope -> [P.Statement scope] -> Either (NonEmpty Diagnostic) (Artifact owner target entry provider Draft)
materializeRootArtifact separate normalized root statements
  | any (`S.member` reserved) ["MONK_LAUNCH_ORIGINAL", "MONK_LAUNCH_WRAPPER"] =
      Left (planDiagnostic "launch-binding" "Source bindings may not overlap private standalone launch metadata" :| [])
  | entryMode cfg == Standalone,
    profileSupportsFishFeature (targetProfile cfg) Fish46,
    let prefix = choosePrefix reserved 0,
    Just (body, commands, writesOutput) <- Native.nativeStatements root prefix statements =
      let effects = foldMap Effects.statementEffects statements
          bindings = S.delete "#" (Effects.effectReads effects <> Effects.effectWrites effects)
          operations = S.fromList ([NativeExec | not (S.null commands)] <> [NativeWrite | writesOutput])
          script = MkScript (if S.null operations then standaloneBindingGuards bindings <> body else standaloneGuards prefix bindings (NativeRuntime.nativeRuntimeSetup cfg prefix operations <> [NativeRuntime.nativeWriterDefinition prefix | writesOutput] <> body))
          requirement program reason = MkRuntimeRequirement program (MkRequirementUse reason Nothing :| [])
          requirements = nativeRuntimeRequirement NativeLaunch "Preserve streams before Fish startup" : requirement (RequiresFishFeature Fish46) "Structural Fish execution profile" : [requirement (RequiresCommand name) "Explicit external command dispatch" | name <- S.toAscList commands] <> [nativeRuntimeRequirement NativeExec "Source-located external exec failures" | not (S.null commands)] <> [nativeRuntimeRequirement NativeWrite "Bash output errno and signal semantics" | writesOutput]
       in pure (draftArtifact normalized prefix (NativeRuntime.runtimeHelperName prefix) script mempty [] requirements)
  | otherwise = do
      let prefix = choosePrefix reserved 0
          identityTag = show (cfg, statements)
          effects = foldMap Effects.statementEffects statements
          bindings = S.delete "#" (Effects.effectReads effects <> Effects.effectWrites effects <> M.keysSet (callerVariables (callerContract cfg)))
          initial = MkMaterialization root prefix 0 mempty Helpers.empty cfg identityTag False False Nothing [] bindings (separate && not (Effects.effectSession effects)) mempty [] (Effects.effectMayEnableErrexit effects) (Effects.effectSession effects) 0 0 (Effects.effectTraps effects) False False
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
                registerHelpers [Session.requestDefinition prefix]
              when (Effects.effectTraps effects) $
                registerHelpers (Traps.definitions prefix)
              when (stableDirectoryEnabled cfg && not (S.null (bindings `S.intersection` S.fromList ["PWD", "OLDPWD", "dirstack"]))) $
                needNative NativeDirectory "Directory binding boundary obligations"
              when (entryMode cfg == Sourceable) $ do
                needProgram (RequiresCommand "fish") "Caller boundary failure status restoration"
                needProgram (RequiresFishFeature FunctionScopeSharing) "Sourceable caller frame"
              lowerStatements False statements
          )
          initial
      let ownedName = (compilerIdentifier prefix <>)
          statusName = ownedName "status"
          initialization =
            [assign [SetGlobal] statusName (ExprLiteral "0")]
              <> [assign [SetGlobal] (ownedName "source_origin") (ExprLiteral (fromMaybe "<input>" (listToMaybe [srcFile (rangeStart range) | P.Statement (Just range) _ <- statements]))) | Effects.effectTraps effects]
              <> [assign [SetGlobal] (ownedName role) (ExprLiteral "0") | Effects.effectMayEnableErrexit effects, role <- ["errexit", "suppress"]]
              <> [assign [SetGlobal] (ownedName "last_pid") (ExprLiteral "") | Effects.effectSession effects]
              <> [assign [SetGlobal] (ownedName "pipefail") (ExprLiteral "0") | Effects.effectPipefail effects]
              <> [assign [SetGlobal] (ownedName "ifs") (ExprLiteral " \t\n") | S.member "IFS" bindings]
              <> [assign [SetGlobal] (ownedName role) (ExprLiteral "0") | Effects.effectSubstitution effects, role <- ["substitution_executed", "substitution_status"]]
          moduleDefinitions = M.elems (materialModules final)
          moduleFunctions = helperNames moduleDefinitions
          modules = fmap (MkScript . (: [])) (materialModules final)
          moduleRoot = ownedName "module_root"
          loaders =
            [assign [SetLocal] moduleRoot (NativeRuntime.entryDirectory prefix) | not (M.null modules)]
              <> concatMap (loadModule (entryMode cfg == Sourceable) (identifierText moduleRoot)) (M.keys modules)
          helpers = Helpers.definitions (materialHelpers final) <> loaders
          nativeOperations = foldMap (\case RequiresNativeRuntime _ _ operations -> operations; _ -> mempty) (M.keys (materialRequirements final))
          runtimeSetup = NativeRuntime.nativeRuntimeSetup cfg prefix nativeOperations
          programBody = initialization <> [statement | Effects.effectTraps effects, statement <- Traps.initialize prefix] <> helpers <> (if S.member NativeDirectory nativeOperations then Directory.directorySetup cfg prefix else []) <> body <> [if Effects.effectTraps effects then Traps.exitWithStatus prefix (scalarVar statusName) else builtin "exit" [arg (scalarVar statusName)]]
          complete =
            if entryMode cfg == Sourceable
              then [asCommand (sourceableEntry cfg nativeOperations prefix identityTag (scalarVar "status") (prefix <> "entry") moduleFunctions helpers body)]
              else standaloneGuards prefix (bindings S.\\ Effects.effectArrays effects) (standaloneArrayGuards (Effects.effectArrays effects) <> [statement | Effects.effectSession effects, statement <- sessionEnvironmentGuards] <> runtimeSetup <> if Effects.effectSession effects then Session.launchSession prefix programBody else programBody)
          requirements = [MkRuntimeRequirement program uses | (program, uses) <- M.toAscList (materialRequirements final)]
      pure (draftArtifact normalized prefix (NativeRuntime.runtimeHelperName prefix) (MkScript complete) modules (materialDiagnostics final) requirements)
  where
    cfg = contextConfig (P.normalizedContext normalized)
    reserved = P.normalizedReserved normalized

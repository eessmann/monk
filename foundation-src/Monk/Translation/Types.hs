{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Copyright: (c) 2025 Erich Essmann
-- SPDX-License-Identifier: MIT
-- Maintainer: Erich Essmann <essmanne@gmail.com>
--
-- Stable public translation, diagnostics, and runtime-requirement types.
module Monk.Translation.Types
  ( TranslateConfig (..),
    TranslationPolicy (..),
    TranslationStatistics (..),
    ExecutionStrategy (..),
    executionStrategyFor,
    Approximation (..),
    TargetProfile (..),
    EntryMode (..),
    RuntimeSelection (..),
    DirectoryContract (..),
    DirectoryPermissions (..),
    DirectoryAccess (..),
    noDirectoryPermissions,
    stableDirectoryEnabled,
    NativeOperation (..),
    nativeOperationName,
    nativeRuntimeRequirement,
    CallerContract (..),
    VariableContract (..),
    BindingAccess (..),
    BindingScope (..),
    BindingExport (..),
    FunctionContract (..),
    AmbientEffects (..),
    emptyCallerContract,
    strictMode,
    allowsApproximation,
    approximationName,
    parseApproximation,
    defaultConfig,
    strictConfig,
    DiagnosticCode (..),
    DiagnosticPhase (..),
    DiagnosticSeverity (..),
    ReviewRisk (..),
    Diagnostic (..),
    RuntimeProgram (..),
    FishFeature (..),
    fishFeatureName,
    profileSupportsFishFeature,
    PlatformCapability (..),
    platformCapabilityName,
    profileSupportsPlatformCapability,
    RequirementUse (..),
    RuntimeRequirement (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Monk.Runtime.Abi2 (NativeOperation (..), abiOperationName)
import Monk.Runtime.NativeTarget (runtimeABI)
import Monk.Source.Location (SourceRange)
import Relude

-- | Translation is exact under the selected profile and caller contract.
-- A migration policy permits only individually selected approximations.
data TranslateConfig = MkTranslateConfig
  { translationPolicy :: TranslationPolicy,
    targetProfile :: TargetProfile,
    entryMode :: EntryMode,
    callerContract :: CallerContract,
    translationRuntime :: RuntimeSelection,
    directoryContract :: DirectoryContract
  }
  deriving stock (Show, Eq)

-- | Static sites in the complete materialized Fish, including embedded child
-- scripts. Native sites call the owned native operation dispatcher. Provider
-- implementation and ABI probes are not additional operation call sites.
-- These counts say nothing about dynamic executions or process launches.
data TranslationStatistics = MkTranslationStatistics
  { statisticsHelperDefinitions :: !Int,
    statisticsHelperCallSites :: !Int,
    statisticsNativeCallSites :: !Int,
    statisticsRenderedFishBytes :: !Int,
    statisticsStatementSites :: !Int,
    statisticsTemporaryBindings :: !Int,
    statisticsStatusCaptures :: !Int,
    statisticsExternalCallSites :: !Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Direct Fish can use bounded primitive helpers. Supervised execution also
-- gives a native owner authority over process lifetimes and descriptors.
data ExecutionStrategy = DirectExecution | SupervisedExecution
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

executionStrategyFor :: [RuntimeRequirement] -> ExecutionStrategy
executionStrategyFor requirements
  | any supervised requirements = SupervisedExecution
  | otherwise = DirectExecution
  where
    supervised (MkRuntimeRequirement (RequiresNativeRuntime _ _ operations) _) = Set.member NativeSession operations
    supervised _ = False

data TranslationPolicy
  = ExactOnly
  | Migration (Set.Set Approximation)
  deriving stock (Show, Eq)

data Approximation
  = ReadonlyUnchecked
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | UTF-8 source, C locale, noninteractive Bash 5.3, signed 64-bit arithmetic;
-- initial errexit/pipefail/lastpipe/job-control disabled. The initial runtime
-- evidence endpoints are Bash 5.3.9 and Fish 4.6.0 on 64-bit Linux systems.
data TargetProfile = Bash53Signed64Fish46
  deriving stock (Show, Eq, Ord)

data EntryMode = Standalone | Sourceable
  deriving stock (Show, Eq, Ord)

-- | Installed runtime selection, or a member relative to an immutable entry.
-- RuntimeGeneration is selected by managed output planning before admission.
data RuntimeSelection = RuntimeOnPath | RuntimePath FilePath | RuntimeGeneration FilePath
  deriving stock (Show, Eq, Ord)

nativeOperationName :: NativeOperation -> Text
nativeOperationName = toText . abiOperationName

nativeRuntimeRequirement :: NativeOperation -> Text -> RuntimeRequirement
nativeRuntimeRequirement operation reason =
  MkRuntimeRequirement (RequiresNativeRuntime runtimeABI Bash53Signed64Fish46 (Set.singleton operation)) (MkRequirementUse reason Nothing :| [])

-- | Caller declarations are semantic obligations, not proofs about arbitrary
-- Fish functions. Admission checks supported shapes; runtime guards check only
-- observable preconditions. Unknown ambient effects do not authorize execution.
data DirectoryContract = NoDirectoryContract | StableDirectoryContract
  deriving stock (Show, Eq, Ord)

data DirectoryAccess = NoDirectoryAccess | ReadDirectory | WriteDirectory | ReadWriteDirectory
  deriving stock (Show, Eq, Ord)

-- | Separate permissions; the stable contract includes empty CDPATH, ordinary
-- exported global PWD, and stable logical cwd ancestry across external effects.
data DirectoryPermissions = MkDirectoryPermissions
  { directoryCwd :: DirectoryAccess,
    directoryPwd :: DirectoryAccess,
    directoryOldpwd :: DirectoryAccess,
    directoryStack :: DirectoryAccess
  }
  deriving stock (Show, Eq, Ord)

noDirectoryPermissions :: DirectoryPermissions
noDirectoryPermissions = MkDirectoryPermissions NoDirectoryAccess NoDirectoryAccess NoDirectoryAccess NoDirectoryAccess

stableDirectoryEnabled :: TranslateConfig -> Bool
stableDirectoryEnabled cfg = case entryMode cfg of
  Standalone -> directoryContract cfg == StableDirectoryContract
  Sourceable -> isJust (callerDirectory (callerContract cfg))

data CallerContract = MkCallerContract
  { callerVariables :: M.Map Text VariableContract,
    callerFunctions :: M.Map Text FunctionContract,
    callerExportedFunctions :: Set.Set Text,
    callerAmbientEffects :: AmbientEffects,
    callerDirectory :: Maybe DirectoryPermissions,
    callerFunctionDirectories :: M.Map Text DirectoryPermissions
  }
  deriving stock (Show, Eq)

data VariableContract = ScalarBinding BindingAccess BindingScope BindingExport
  deriving stock (Show, Eq, Ord)

data BindingAccess = InputBinding | OutputBinding | InputOutputBinding
  deriving stock (Show, Eq, Ord)

data BindingScope = VisibleBinding | GlobalBinding
  deriving stock (Show, Eq, Ord)

-- | Initial export attribute of a caller binding. An absent global output is
-- admitted only as unexported; exporting it requires an executed Bash export
-- operation. Path/list attributes are not scalar values.
data BindingExport = UnexportedBinding | ExportedBinding
  deriving stock (Show, Eq, Ord)

-- | The caller supplies a corresponding Fish function which returns normally,
-- performs only the declared scalar effects, and does not mutate dispatch or
-- register/trigger relevant callbacks. Output/status behavior is an obligation
-- of the declared cross-language import, not something a name check can prove.
data FunctionContract = MkFunctionContract
  { functionTarget :: Text,
    functionReads :: Set.Set Text,
    functionWrites :: Set.Set Text
  }
  deriving stock (Show, Eq)

data AmbientEffects = UnknownAmbientEffects | NoRelevantAmbientEffects
  deriving stock (Show, Eq, Ord)

emptyCallerContract :: CallerContract
emptyCallerContract = MkCallerContract mempty mempty mempty UnknownAmbientEffects Nothing mempty

defaultConfig :: TranslateConfig
defaultConfig = MkTranslateConfig (Migration mempty) Bash53Signed64Fish46 Standalone emptyCallerContract RuntimeOnPath NoDirectoryContract

strictConfig :: TranslateConfig
strictConfig = defaultConfig {translationPolicy = ExactOnly}

-- | Compatibility projection for private legacy construction helpers.
strictMode :: TranslateConfig -> Bool
strictMode cfg = translationPolicy cfg == ExactOnly

allowsApproximation :: TranslateConfig -> Approximation -> Bool
allowsApproximation cfg approximation = case translationPolicy cfg of
  ExactOnly -> False
  Migration selected -> Set.member approximation selected

approximationName :: Approximation -> Text
approximationName ReadonlyUnchecked = "readonly-unchecked"

parseApproximation :: Text -> Maybe Approximation
parseApproximation name = find ((== name) . approximationName) [minBound .. maxBound]

newtype DiagnosticCode = MkDiagnosticCode
  { diagnosticCodeText :: Text
  }
  deriving stock (Show, Eq, Ord)

data DiagnosticPhase
  = PhaseParse
  | PhaseTranslate
  | PhaseSource
  | PhaseOutput
  deriving stock (Show, Eq, Ord)

data DiagnosticSeverity
  = DiagnosticError
  | DiagnosticWarning
  | DiagnosticNote
  deriving stock (Show, Eq, Ord)

data ReviewRisk
  = Clean
  | Review
  | Unsafe
  deriving stock (Show, Eq, Ord)

data Diagnostic = MkDiagnostic
  { diagnosticCode :: DiagnosticCode,
    diagnosticPhase :: DiagnosticPhase,
    diagnosticSeverity :: DiagnosticSeverity,
    diagnosticRisk :: ReviewRisk,
    diagnosticMessage :: Text,
    diagnosticRange :: Maybe SourceRange
  }
  deriving stock (Show, Eq)

data RuntimeProgram
  = RequiresCommand Text
  | RequiresFishFeature FishFeature
  | RequiresPlatformCapability PlatformCapability
  | RequiresNativeRuntime Int TargetProfile (Set.Set NativeOperation)
  deriving stock (Show, Eq, Ord)

data FishFeature = Fish46 | FunctionScopeSharing | NulDelimitedCapture
  deriving stock (Show, Eq, Ord)

fishFeatureName :: FishFeature -> Text
fishFeatureName Fish46 = "4.6"
fishFeatureName FunctionScopeSharing = "function-scope-sharing"
fishFeatureName NulDelimitedCapture = "nul-delimited-capture"

-- | Capabilities justified by the selected execution profile. Admission checks
-- the actual materialized requirements against this exhaustive mapping.
profileSupportsFishFeature :: TargetProfile -> FishFeature -> Bool
profileSupportsFishFeature Bash53Signed64Fish46 Fish46 = True
profileSupportsFishFeature Bash53Signed64Fish46 FunctionScopeSharing = True
profileSupportsFishFeature Bash53Signed64Fish46 NulDelimitedCapture = True

-- | Platform behavior consumed by a materialized runtime boundary.
data PlatformCapability = Linux64DescriptorFilesystem | PosixOwnedDescriptors | PipeDescriptorPaths
  deriving stock (Show, Eq, Ord)

platformCapabilityName :: PlatformCapability -> Text
platformCapabilityName Linux64DescriptorFilesystem = "linux-64-descriptor-filesystem"
platformCapabilityName PosixOwnedDescriptors = "posix-owned-descriptors"
platformCapabilityName PipeDescriptorPaths = "pipe-descriptor-paths"

profileSupportsPlatformCapability :: TargetProfile -> PlatformCapability -> Bool
profileSupportsPlatformCapability Bash53Signed64Fish46 Linux64DescriptorFilesystem = True
profileSupportsPlatformCapability Bash53Signed64Fish46 PosixOwnedDescriptors = True
profileSupportsPlatformCapability Bash53Signed64Fish46 PipeDescriptorPaths = True

data RequirementUse = MkRequirementUse
  { requirementReason :: Text,
    requirementRange :: Maybe SourceRange
  }
  deriving stock (Show, Eq, Ord)

data RuntimeRequirement = MkRuntimeRequirement
  { requirementProgram :: RuntimeProgram,
    requirementUses :: NonEmpty RequirementUse
  }
  deriving stock (Show, Eq)

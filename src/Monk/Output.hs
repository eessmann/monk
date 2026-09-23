{-# LANGUAGE LambdaCase #-}

-- | Opaque publication products. Planning owns bytes and destination together;
-- rendering and filesystem publication remain separate operations.
module Monk.Output
  ( OutputTarget (..),
    GeneratedFile,
    generatedTarget,
    generatedScript,
    generatedDiagnostics,
    generatedRuntimeRequirements,
    generatedStatistics,
    generatedExecutionStrategy,
    OutputBundle,
    bundleUserFiles,
    NativeRuntimeImage,
    NativeRuntimeArtifact,
    bundleRuntimeArtifacts,
    runtimeArtifactTarget,
    runtimeArtifactImage,
    runtimeArtifactMode,
    nativeImageBytes,
    nativeImageTarget,
    nativeImageDigest,
    nativeImageOperations,
    nativeImageABI,
    nativeImageProfile,
    OutputFailure,
    OutputFailureKind (..),
    OutputObservedEntry (..),
    outputFailureKind,
    outputFailureDiagnostic,
    outputFailureObservedEntry,
    OutputReceipt,
    outputReceiptDestination,
    outputReceiptGeneration,
    outputReceiptWarnings,
    planCombinedOutputBundle,
    planSeparateOutputBundle,
    planManagedOutputBundle,
    renderOutputBundle,
    publishOutputBundle,
  )
where

import Control.Exception (IOException, try)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import GHC.Show qualified as GHC
import Language.Bash.Plan qualified as P
import Language.Fish.DSL (Script, renderScript)
import Language.Fish.Translator.Plan (PlannedTranslation, compileBundleLoader, compileSourceBundle, plannedBundleEntry, plannedBundleModuleTranslations, plannedDiagnostics, plannedRequirements, plannedScript, plannedStatistics)
import Monk.Output.Publication qualified as Publication
import Monk.Output.Runtime
import Monk.Source (SourceGraph)
import Monk.Source.Product (graphParseDiagnostics, graphPlan, graphTranslation)
import Monk.Translation.Types
import System.Directory (makeAbsolute)
import System.FilePath qualified as FP

data OutputTarget = OutputStdout | OutputPath FilePath
  deriving stock (Show, Eq, Ord)

-- Each file is a view of an admitted artifact, with source parse diagnostics.
data GeneratedFile = MkGeneratedFile OutputTarget PlannedTranslation [Diagnostic]
  deriving stock (Show, Eq)

data NativeRuntimeArtifact = MkNativeRuntimeArtifact OutputTarget NativeRuntimeImage
  deriving stock (Show, Eq)

data OutputBundle = MkOutputBundle (NonEmpty GeneratedFile) [NativeRuntimeArtifact] (Maybe Publication.PublicationPlan)

instance GHC.Show OutputBundle where
  show (MkOutputBundle files runtime _) = "OutputBundle " <> show files <> " " <> show runtime

data OutputFailure = MkOutputFailure OutputFailureKind Diagnostic (Maybe OutputObservedEntry)
  deriving stock (Show, Eq)

data OutputFailureKind
  = OutputNoFilesystemPublication
  | OutputInvalidPublicationPlan
  | OutputOwnershipMismatch
  | OutputSymlinkConflict
  | OutputGenerationCollision
  | OutputPublicationIOFailure
  | OutputInjectedPublicationFailure
  deriving stock (Show, Eq, Ord)

data OutputObservedEntry
  = OutputEntryMissing
  | OutputEntryMatchesPlanned
  | OutputEntryDiffers
  | OutputEntryUnreadable
  deriving stock (Show, Eq, Ord)

data OutputReceipt = MkOutputReceipt FilePath (Maybe FilePath) [Text]
  deriving stock (Show, Eq)

generatedTarget :: GeneratedFile -> OutputTarget
generatedTarget (MkGeneratedFile target _ _) = target

generatedScript :: GeneratedFile -> Script
generatedScript (MkGeneratedFile _ translation _) = plannedScript translation

generatedDiagnostics :: GeneratedFile -> [Diagnostic]
generatedDiagnostics (MkGeneratedFile _ translation diagnostics) = diagnostics <> plannedDiagnostics translation

generatedRuntimeRequirements :: GeneratedFile -> [RuntimeRequirement]
generatedRuntimeRequirements (MkGeneratedFile _ translation _) = plannedRequirements translation

generatedExecutionStrategy :: GeneratedFile -> ExecutionStrategy
generatedExecutionStrategy = executionStrategyFor . generatedRuntimeRequirements

generatedStatistics :: GeneratedFile -> TranslationStatistics
generatedStatistics (MkGeneratedFile _ translation _) = plannedStatistics translation

bundleUserFiles :: OutputBundle -> NonEmpty GeneratedFile
bundleUserFiles (MkOutputBundle files _ _) = files

bundleRuntimeArtifacts :: OutputBundle -> [NativeRuntimeArtifact]
bundleRuntimeArtifacts (MkOutputBundle _ runtime _) = runtime

runtimeArtifactTarget :: NativeRuntimeArtifact -> OutputTarget
runtimeArtifactTarget (MkNativeRuntimeArtifact target _) = target

runtimeArtifactImage :: NativeRuntimeArtifact -> NativeRuntimeImage
runtimeArtifactImage (MkNativeRuntimeArtifact _ image) = image

-- | The private executable mode included in managed generation identity.
runtimeArtifactMode :: NativeRuntimeArtifact -> Word32
runtimeArtifactMode _ = 0o700

outputFailureKind :: OutputFailure -> OutputFailureKind
outputFailureKind (MkOutputFailure kind _ _) = kind

outputFailureDiagnostic :: OutputFailure -> Diagnostic
outputFailureDiagnostic (MkOutputFailure _ diagnostic _) = diagnostic

outputFailureObservedEntry :: OutputFailure -> Maybe OutputObservedEntry
outputFailureObservedEntry (MkOutputFailure _ _ observed) = observed

outputReceiptDestination :: OutputReceipt -> FilePath
outputReceiptDestination (MkOutputReceipt destination _ _) = destination

outputReceiptGeneration :: OutputReceipt -> Maybe FilePath
outputReceiptGeneration (MkOutputReceipt _ generation _) = generation

outputReceiptWarnings :: OutputReceipt -> [Text]
outputReceiptWarnings (MkOutputReceipt _ _ warnings) = warnings

planCombinedOutputBundle :: OutputTarget -> SourceGraph -> IO (Either Diagnostic OutputBundle)
planCombinedOutputBundle target graph = do
  resolved <- case target of
    OutputStdout -> pure (Right OutputStdout)
    OutputPath path -> fmap OutputPath <$> resolveOutputPath path
  pure $ do
    ownedTarget <- resolved
    let script = plannedScript (graphTranslation graph)
        file = MkGeneratedFile ownedTarget (graphTranslation graph) (graphParseDiagnostics graph)
    publication <- case ownedTarget of
      OutputStdout -> pure Nothing
      OutputPath path -> Just <$> first publicationPlanningDiagnostic (Publication.planSingleFilePublication path (encodeUtf8 (renderScript script)))
    pure (MkOutputBundle (file :| []) [] publication)

-- | Own an absolute destination and immutable module layout before publishing.
-- Resolving the output cwd is independent of source discovery and writes no
-- files. The private materializer constructs and admits the loader as well as
-- every member; this layer only selects their immutable generation paths.
planSeparateOutputBundle :: FilePath -> SourceGraph -> IO (Either Diagnostic OutputBundle)
planSeparateOutputBundle = planManagedOutputBundle

-- | Capture the selected runtime and rematerialize against its immutable
-- generation member before constructing any publication product.
planManagedOutputBundle :: FilePath -> SourceGraph -> IO (Either Diagnostic OutputBundle)
planManagedOutputBundle destination graph = do
  absolute <- resolveOutputPath destination
  let cfg = P.sourcePlanConfig (graphPlan graph)
      runtimeMember = "bin/monk-runtime"
      rebound = P.rebindSourcePlan (RuntimeGeneration runtimeMember) (graphPlan graph)
  case (absolute, first NE.head (compileSourceBundle rebound)) of
    (Left diagnostic, _) -> pure (Left diagnostic)
    (_, Left diagnostic) -> pure (Left diagnostic)
    (Right target, Right planned) -> do
      -- Capture against the requirements of the fully rebound artifact. The
      -- original graph's requirements cannot certify a changed materialization.
      let entry = plannedBundleEntry planned
          operations = foldMap (\case MkRuntimeRequirement (RequiresNativeRuntime _ _ ops) _ -> ops; _ -> mempty) (plannedRequirements entry)
      imageResult <- if S.null operations then pure (Right Nothing) else fmap Just <$> captureNativeRuntime (translationRuntime cfg) operations
      pure $ do
        image <- first (outputDiagnostic "native-runtime") imageResult
        let translations = ("entry.fish", entry) : M.toAscList (plannedBundleModuleTranslations planned)
            members =
              [Publication.PublicationMember path Publication.FishSource (encodeUtf8 (renderScript (plannedScript translation))) | (path, translation) <- translations]
                <> [Publication.PublicationMember runtimeMember Publication.NativeExecutable (nativeImageBytes native) | native <- maybeToList image]
            diagnostics = graphParseDiagnostics graph
        relativeGeneration <- first publicationPlanningDiagnostic (Publication.generationRelativeDirectoryMembers target members)
        let generation = FP.takeDirectory target FP.</> relativeGeneration
        loader <- first NE.head (compileBundleLoader (generation FP.</> "entry.fish") planned)
        publication <- first publicationPlanningDiagnostic (Publication.planManagedPublicationMembers target members (encodeUtf8 (renderScript (plannedScript loader))))
        let files = [MkGeneratedFile (OutputPath (generation FP.</> path)) translation diagnostics | (path, translation) <- translations]
            entryFile = MkGeneratedFile (OutputPath target) loader diagnostics
            runtime = [MkNativeRuntimeArtifact (OutputPath (generation FP.</> runtimeMember)) native | native <- maybeToList image]
        pure (MkOutputBundle (entryFile :| files) runtime (Just publication))

resolveOutputPath :: FilePath -> IO (Either Diagnostic FilePath)
resolveOutputPath destination
  | null destination || '\0' `elem` destination || FP.hasTrailingPathSeparator destination =
      pure (Left (outputDiagnostic "destination" "Output destination must be a nonempty file path without a trailing separator or NUL"))
  | otherwise = first (outputDiagnostic "destination" . show) <$> try @IOException (FP.normalise <$> makeAbsolute destination)

renderOutputBundle :: OutputBundle -> [(OutputTarget, Text)]
renderOutputBundle bundle =
  [ (generatedTarget file, renderScript (generatedScript file))
  | file <- toList (bundleUserFiles bundle)
  ]

publishOutputBundle :: OutputBundle -> IO (Either OutputFailure OutputReceipt)
publishOutputBundle (MkOutputBundle _ _ Nothing) =
  pure
    ( Left
        ( MkOutputFailure
            OutputNoFilesystemPublication
            (outputDiagnostic "stdout-publication" "Stdout output has no filesystem publication plan")
            Nothing
        )
    )
publishOutputBundle (MkOutputBundle _ _ (Just plan)) =
  fmap (bimap publicationFailure publicationReceipt) (Publication.publishPublication plan)

publicationFailure :: Publication.PublicationFailure -> OutputFailure
publicationFailure failure =
  MkOutputFailure
    (publicationFailureKind (Publication.publicationFailureKind failure))
    ( outputDiagnostic
        "publication"
        (Publication.publicationFailureMessage failure <> maybe "" (\observed -> "; observed entry: " <> show observed) (Publication.publicationFailureObservedEntry failure))
    )
    (publicationObservedEntry <$> Publication.publicationFailureObservedEntry failure)

publicationFailureKind :: Publication.PublicationFailureKind -> OutputFailureKind
publicationFailureKind = \case
  Publication.InvalidPublicationPlan -> OutputInvalidPublicationPlan
  Publication.OwnershipMismatch -> OutputOwnershipMismatch
  Publication.SymlinkConflict -> OutputSymlinkConflict
  Publication.GenerationCollision -> OutputGenerationCollision
  Publication.PublicationIOFailure -> OutputPublicationIOFailure
  Publication.InjectedPublicationFailure -> OutputInjectedPublicationFailure

publicationObservedEntry :: Publication.ObservedEntry -> OutputObservedEntry
publicationObservedEntry = \case
  Publication.ObservedEntryMissing -> OutputEntryMissing
  Publication.ObservedEntryMatchesPlanned -> OutputEntryMatchesPlanned
  Publication.ObservedEntryDiffers -> OutputEntryDiffers
  Publication.ObservedEntryUnreadable -> OutputEntryUnreadable

publicationReceipt :: Publication.PublicationReceipt -> OutputReceipt
publicationReceipt receipt =
  MkOutputReceipt
    (Publication.publicationReceiptDestination receipt)
    (Publication.publicationReceiptGeneration receipt)
    (Publication.publicationReceiptWarnings receipt)

publicationPlanningDiagnostic :: Publication.PublicationFailure -> Diagnostic
publicationPlanningDiagnostic failure =
  outputDiagnostic "publication" (Publication.publicationFailureMessage failure)

outputDiagnostic :: Text -> Text -> Diagnostic
outputDiagnostic code message = MkDiagnostic (MkDiagnosticCode ("monk.output." <> code)) PhaseOutput DiagnosticError Unsafe message Nothing

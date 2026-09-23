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
    plannedBundleModuleTranslations,
    plannedScript,
    plannedDiagnostics,
    plannedRequirements,
    plannedStatistics,
  )
where

import Data.Map.Strict qualified as M
import Data.Text qualified as T
import GHC.Show (Show (showsPrec))
import Language.Bash.Plan qualified as P
import Language.Bash.Plan.Normalize (normalizeDocument, normalizeSource)
import Language.Fish.DSL.Internal (Script)
import Language.Fish.Translator.Context (planDiagnostic)
import Monk.Compiler.Artifact qualified as Artifact
import Monk.Translation.Types
import ShellCheck.Interface (ParseResult)
import System.FilePath (isAbsolute)

-- | A projection always retains the complete admitted owner. Module views
-- cannot swap a script while preserving an unrelated admission receipt.
data PlannedTranslation where
  MkPlannedTranslation :: Artifact.ArtifactView owner target entry provider -> PlannedTranslation

instance Show PlannedTranslation where
  showsPrec precedence (MkPlannedTranslation view) = showsPrec precedence view

instance Eq PlannedTranslation where
  MkPlannedTranslation left == MkPlannedTranslation right =
    Artifact.sameArtifact (Artifact.viewArtifact left) (Artifact.viewArtifact right) && Artifact.viewScript left == Artifact.viewScript right && Artifact.viewStatistics left == Artifact.viewStatistics right

plannedScript :: PlannedTranslation -> Script
plannedScript (MkPlannedTranslation view) = Artifact.viewScript view

plannedDiagnostics :: PlannedTranslation -> [Diagnostic]
plannedDiagnostics (MkPlannedTranslation view) = Artifact.artifactDiagnostics (Artifact.viewArtifact view)

plannedRequirements :: PlannedTranslation -> [RuntimeRequirement]
plannedRequirements (MkPlannedTranslation view) = Artifact.artifactRequirements (Artifact.viewArtifact view)

plannedStatistics :: PlannedTranslation -> TranslationStatistics
plannedStatistics (MkPlannedTranslation view) = Artifact.viewStatistics view

newtype PlannedBundle = MkPlannedBundle {plannedBundleEntry :: PlannedTranslation}
  deriving stock (Show, Eq)

plannedBundleModules :: PlannedBundle -> M.Map FilePath Script
plannedBundleModules (MkPlannedBundle (MkPlannedTranslation view)) = Artifact.artifactMembers (Artifact.viewArtifact view)

plannedBundleModuleStatistics :: PlannedBundle -> M.Map FilePath TranslationStatistics
plannedBundleModuleStatistics (MkPlannedBundle (MkPlannedTranslation view)) = Artifact.artifactMemberStatistics (Artifact.viewArtifact view)

plannedBundleModuleTranslations :: PlannedBundle -> M.Map FilePath PlannedTranslation
plannedBundleModuleTranslations (MkPlannedBundle (MkPlannedTranslation view)) =
  fmap MkPlannedTranslation (Artifact.memberViews (Artifact.viewArtifact view))

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
  case plannedBundleEntry bundle of
    MkPlannedTranslation view -> do
      draft <- Artifact.loaderArtifact entry (Artifact.viewArtifact view)
      artifact <- Artifact.admitArtifact draft
      pure (MkPlannedTranslation (Artifact.entryView artifact))

compileMaterialization :: Bool -> P.SourcePlan -> Either (NonEmpty Diagnostic) PlannedBundle
compileMaterialization separate plan = P.withSourcePlan plan $ \normalized -> do
  draft <- Artifact.materializeArtifact separate normalized
  artifact <- Artifact.admitArtifact draft
  pure (MkPlannedBundle (MkPlannedTranslation (Artifact.entryView artifact)))

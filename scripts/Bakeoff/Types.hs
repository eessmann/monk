{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bakeoff.Types
  ( FixtureGroup (..),
    SelectionSource (..),
    SkipReason (..),
    ToolName (..),
    ToolVersion (..),
    CommandStatus (..),
    DiffStatus (..),
    BakeoffConfig (..),
    ResolvedTools (..),
    GitMetadata (..),
    FixtureSpec (..),
    FixtureMetadataSummary (..),
    TranslationReport (..),
    RuntimeReport (..),
    DiffArtifact (..),
    DiffReport (..),
    FixtureReport (..),
    MetaReport (..),
    ConfigReport (..),
    FixtureSelectionReport (..),
    BenchmarkPlan (..),
    BenchmarkSuite (..),
    HyperfineResult (..),
    HyperfineSummary (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime)
import Bakeoff.Fixture (FixtureMetadata (..))
import Bakeoff.Shell (ShellRunMode)
import Path (Abs, Dir, File, Path, Rel)

data FixtureGroup
  = FixtureGroupCorpus
  | FixtureGroupBenchmark
  | FixtureGroupIntegration
  | FixtureGroupGolden
  | FixtureGroupRealWorld
  | FixtureGroupCustom
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SelectionSource
  = SelectionDefault FixtureGroup
  | SelectionGroup FixtureGroup
  | SelectionFile (Path Abs File)
  | SelectionFileList (Path Abs File)
  | SelectionCompatible (Path Abs File)
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SkipReason
  = SkipPlatformMismatch Text [Text]
  | SkipMissingPrereqs [Text]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ToolName
  = ToolMonk
  | ToolBabelfish
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype ToolVersion = MkToolVersion
  { unToolVersion :: Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data CommandStatus
  = CommandSucceeded
  | CommandFailed
  | CommandTimedOut
  | CommandSkipped
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data DiffStatus
  = DiffNone
  | DiffDifferent
  | DiffUnavailable
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data BakeoffConfig = MkBakeoffConfig
  { bakeoffCwd :: Path Abs Dir,
    bakeoffOutputDir :: Path Abs Dir,
    bakeoffForce :: Bool,
    bakeoffGroups :: [FixtureGroup],
    bakeoffFiles :: [Path Abs File],
    bakeoffFileLists :: [Path Abs File],
    bakeoffCompatibleFileLists :: [Path Abs File],
    bakeoffJobs :: Maybe Int,
    bakeoffTranslationTimeoutSeconds :: Int,
    bakeoffRuntimeTimeoutSeconds :: Int,
    bakeoffBenchmarksEnabled :: Bool,
    bakeoffHyperfineRuns :: Int,
    bakeoffHyperfineWarmup :: Int,
    bakeoffBabelfishPathHint :: Maybe (Path Abs File),
    bakeoffFishPathHint :: Maybe (Path Abs File),
    bakeoffHyperfinePathHint :: Maybe (Path Abs File),
    bakeoffBabelfishVersionOverride :: Maybe Text
  }
  deriving stock (Eq, Show)

data ResolvedTools = MkResolvedTools
  { toolsMonkExecutable :: Path Abs File,
    toolsBabelfishPath :: Path Abs File,
    toolsFishPath :: Path Abs File,
    toolsHyperfinePath :: Maybe (Path Abs File),
    toolsBabelfishVersion :: ToolVersion,
    toolsFishVersion :: ToolVersion,
    toolsHyperfineVersion :: Maybe ToolVersion
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data GitMetadata = MkGitMetadata
  { gitSha :: Maybe Text,
    gitDirty :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data FixtureSpec = MkFixtureSpec
  { specPath :: Path Abs File,
    specRelativePath :: Path Rel File,
    specGroup :: FixtureGroup,
    specMetadata :: FixtureMetadata,
    specSelectionSources :: [SelectionSource],
    specArtifactDir :: Path Rel Dir,
    specSkipReason :: Maybe SkipReason
  }
  deriving stock (Eq, Show)

data FixtureMetadataSummary = MkFixtureMetadataSummary
  { fixtureMetaArgs :: [Text],
    fixtureMetaMode :: ShellRunMode,
    fixtureMetaPlatforms :: Maybe [Text],
    fixtureMetaPrereqs :: [Text],
    fixtureMetaRecursive :: Bool,
    fixtureMetaHasStdin :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data TranslationReport = MkTranslationReport
  { translationTool :: ToolName,
    translationStatus :: CommandStatus,
    translationExitCode :: Maybe Int,
    translationWarningCount :: Int,
    translationNotesCount :: Int,
    translationHighWarnings :: Int,
    translationMediumWarnings :: Int,
    translationLowWarnings :: Int,
    translationConfidenceScore :: Maybe Int,
    translationOutputPath :: Maybe (Path Abs File),
    translationStderrPath :: Maybe (Path Abs File),
    translationErrorMessage :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data RuntimeReport = MkRuntimeReport
  { runtimeTool :: ToolName,
    runtimeStatus :: CommandStatus,
    runtimeExitCode :: Maybe Int,
    runtimeStdoutPath :: Maybe (Path Abs File),
    runtimeStderrPath :: Maybe (Path Abs File),
    runtimeErrorMessage :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data DiffArtifact = MkDiffArtifact
  { diffArtifactStatus :: DiffStatus,
    diffArtifactPath :: Maybe (Path Abs File)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data DiffReport = MkDiffReport
  { diffStdout :: DiffArtifact,
    diffStderr :: DiffArtifact,
    diffExitCode :: DiffArtifact
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data FixtureReport = MkFixtureReport
  { fixtureReportPath :: Path Abs File,
    fixtureReportRelativePath :: Path Rel File,
    fixtureReportArtifactDir :: Path Rel Dir,
    fixtureReportGroup :: FixtureGroup,
    fixtureReportSelectionSources :: [SelectionSource],
    fixtureReportMetadata :: FixtureMetadataSummary,
    fixtureReportSkipReason :: Maybe SkipReason,
    fixtureReportMonkTranslation :: Maybe TranslationReport,
    fixtureReportBabelfishTranslation :: Maybe TranslationReport,
    fixtureReportMonkRuntime :: Maybe RuntimeReport,
    fixtureReportBabelfishRuntime :: Maybe RuntimeReport,
    fixtureReportDiff :: Maybe DiffReport
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ConfigReport = MkConfigReport
  { configTranslationTimeoutSeconds :: Int,
    configRuntimeTimeoutSeconds :: Int,
    configBenchmarksEnabled :: Bool,
    configHyperfineRuns :: Int,
    configHyperfineWarmup :: Int,
    configJobs :: Maybe Int,
    configGroups :: [FixtureGroup],
    configFiles :: [Path Abs File],
    configFileLists :: [Path Abs File],
    configCompatibleFileLists :: [Path Abs File]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data FixtureSelectionReport = MkFixtureSelectionReport
  { selectionPath :: Path Abs File,
    selectionRelativePath :: Path Rel File,
    selectionGroup :: FixtureGroup,
    selectionSources :: [SelectionSource],
    selectionArtifactDir :: Path Rel Dir,
    selectionSkipReason :: Maybe SkipReason
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data MetaReport = MkMetaReport
  { metaTimestamp :: UTCTime,
    metaCwd :: Path Abs Dir,
    metaOutputDir :: Path Abs Dir,
    metaGit :: GitMetadata,
    metaHostOs :: Text,
    metaHostArch :: Text,
    metaTools :: ResolvedTools,
    metaConfig :: ConfigReport,
    metaFixtures :: [FixtureSelectionReport]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data BenchmarkPlan = MkBenchmarkPlan
  { benchmarkAllFixtures :: [Path Abs File],
    benchmarkFixtures :: [Path Abs File],
    benchmarkBabelfishPath :: Path Abs File
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data BenchmarkSuite
  = BenchmarkSuiteAll
  | BenchmarkSuiteBenchmark
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data HyperfineResult = MkHyperfineResult
  { hyperfineCommand :: Text,
    hyperfineMean :: Double,
    hyperfineStddev :: Double
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data HyperfineSummary = MkHyperfineSummary
  { hyperfineTitle :: Text,
    hyperfineJsonPath :: Path Abs File,
    hyperfineMarkdownPath :: Path Abs File,
    hyperfineResults :: [HyperfineResult]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

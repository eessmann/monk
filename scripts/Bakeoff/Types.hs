{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bakeoff.Types
  ( FixtureGroup (..),
    SelectionSource (..),
    SkipReason (..),
    ToolName (..),
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

data BakeoffConfig = BakeoffConfig
  { bcCwd :: Path Abs Dir,
    bcOutputDir :: Path Abs Dir,
    bcForce :: Bool,
    bcGroups :: [FixtureGroup],
    bcFiles :: [Path Abs File],
    bcFileLists :: [Path Abs File],
    bcCompatibleFileLists :: [Path Abs File],
    bcJobs :: Maybe Int,
    bcTranslationTimeoutSeconds :: Int,
    bcRuntimeTimeoutSeconds :: Int,
    bcBenchmarksEnabled :: Bool,
    bcHyperfineRuns :: Int,
    bcHyperfineWarmup :: Int,
    bcBabelfishPathHint :: Maybe (Path Abs File),
    bcFishPathHint :: Maybe (Path Abs File),
    bcHyperfinePathHint :: Maybe (Path Abs File),
    bcBabelfishVersionOverride :: Maybe Text
  }
  deriving stock (Eq, Show)

data ResolvedTools = ResolvedTools
  { rtMonkExecutable :: Path Abs File,
    rtBabelfishPath :: Path Abs File,
    rtFishPath :: Path Abs File,
    rtHyperfinePath :: Maybe (Path Abs File),
    rtBabelfishVersion :: Text,
    rtFishVersion :: Text,
    rtHyperfineVersion :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data GitMetadata = GitMetadata
  { gmSha :: Maybe Text,
    gmDirty :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data FixtureSpec = FixtureSpec
  { fsPath :: Path Abs File,
    fsRelativePath :: Path Rel File,
    fsGroup :: FixtureGroup,
    fsMetadata :: FixtureMetadata,
    fsSelectionSources :: [SelectionSource],
    fsArtifactDir :: Path Rel Dir,
    fsSkipReason :: Maybe SkipReason
  }
  deriving stock (Eq, Show)

data FixtureMetadataSummary = FixtureMetadataSummary
  { fmsArgs :: [Text],
    fmsMode :: ShellRunMode,
    fmsPlatforms :: Maybe [Text],
    fmsPrereqs :: [Text],
    fmsRecursive :: Bool,
    fmsHasStdin :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data TranslationReport = TranslationReport
  { trTool :: ToolName,
    trStatus :: CommandStatus,
    trExitCode :: Maybe Int,
    trWarnings :: Int,
    trNotes :: Int,
    trWarningHigh :: Int,
    trWarningMedium :: Int,
    trWarningLow :: Int,
    trConfidenceScore :: Maybe Int,
    trOutputPath :: Maybe (Path Abs File),
    trStderrPath :: Maybe (Path Abs File),
    trErrorMessage :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data RuntimeReport = RuntimeReport
  { rrTool :: ToolName,
    rrStatus :: CommandStatus,
    rrExitCodeValue :: Maybe Int,
    rrStdoutPath :: Maybe (Path Abs File),
    rrStderrPath :: Maybe (Path Abs File),
    rrErrorMessage :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data DiffArtifact = DiffArtifact
  { daStatus :: DiffStatus,
    daPath :: Maybe (Path Abs File)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data DiffReport = DiffReport
  { drStdout :: DiffArtifact,
    drStderr :: DiffArtifact,
    drExitCode :: DiffArtifact
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data FixtureReport = FixtureReport
  { frPath :: Path Abs File,
    frRelativePath :: Path Rel File,
    frArtifactDir :: Path Rel Dir,
    frGroup :: FixtureGroup,
    frSelectionSources :: [SelectionSource],
    frMetadata :: FixtureMetadataSummary,
    frSkipReason :: Maybe SkipReason,
    frMonkTranslation :: Maybe TranslationReport,
    frBabelfishTranslation :: Maybe TranslationReport,
    frMonkRuntime :: Maybe RuntimeReport,
    frBabelfishRuntime :: Maybe RuntimeReport,
    frDiff :: Maybe DiffReport
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ConfigReport = ConfigReport
  { crTranslationTimeoutSeconds :: Int,
    crRuntimeTimeoutSeconds :: Int,
    crBenchmarksEnabled :: Bool,
    crHyperfineRuns :: Int,
    crHyperfineWarmup :: Int,
    crJobs :: Maybe Int,
    crGroups :: [FixtureGroup],
    crFiles :: [Path Abs File],
    crFileLists :: [Path Abs File],
    crCompatibleFileLists :: [Path Abs File]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data FixtureSelectionReport = FixtureSelectionReport
  { fsrPath :: Path Abs File,
    fsrRelativePath :: Path Rel File,
    fsrGroup :: FixtureGroup,
    fsrSelectionSources :: [SelectionSource],
    fsrArtifactDir :: Path Rel Dir,
    fsrSkipReason :: Maybe SkipReason
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data MetaReport = MetaReport
  { mrTimestamp :: UTCTime,
    mrCwd :: Path Abs Dir,
    mrOutputDir :: Path Abs Dir,
    mrGit :: GitMetadata,
    mrHostOs :: Text,
    mrHostArch :: Text,
    mrTools :: ResolvedTools,
    mrConfig :: ConfigReport,
    mrFixtures :: [FixtureSelectionReport]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data BenchmarkPlan = BenchmarkPlan
  { bpAllFixtures :: [Path Abs File],
    bpBenchmarkFixtures :: [Path Abs File],
    bpBabelfishPath :: Path Abs File
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data BenchmarkSuite
  = BenchmarkSuiteAll
  | BenchmarkSuiteBenchmark
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data HyperfineResult = HyperfineResult
  { hrCommand :: Text,
    hrMean :: Double,
    hrStddev :: Double
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data HyperfineSummary = HyperfineSummary
  { hsTitle :: Text,
    hsJsonPath :: Path Abs File,
    hsMarkdownPath :: Path Abs File,
    hsResults :: [HyperfineResult]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

{-# LANGUAGE LambdaCase #-}

-- | Private publication types boundary.
module Monk.Output.Publication.Types
  ( Destination (..),
    GenerationRelative (..),
    ValidatedMembers (..),
    PublicationPlan (..),
    ManagedPublicationPlan (..),
    SingleFilePublicationPlan (..),
    MemberRole (..),
    PublicationMember (..),
    memberMode,
    PublicationFailure (..),
    PublicationReceipt (..),
    PublicationHook (..),
    PublicationFailureKind (..),
    ObservedEntry (..),
    PublicationStage (..),
    PublicationAbort (..),
    mkPublicationHook,
    publicationFailureKind,
    publicationFailureMessage,
    publicationFailureObservedEntry,
    publicationReceiptDestination,
    publicationReceiptGeneration,
    publicationReceiptWarnings,
    runHookOrAbort,
    noPublicationHook,
    mkFailure,
    ioExceptionFailure,
    invalid,
    abort,
  )
where

import Control.Exception qualified as Exception
import Data.ByteString qualified as BS
import System.Posix.Types (FileMode)

-- Validated identities retain the proof across planning, locking and staging.
newtype Destination = Destination FilePath

newtype GenerationRelative = GenerationRelative FilePath

-- Constructors and record labels stay private: publication consumes this checked
-- layout without sorting, validating, or hashing it again.
data ValidatedMembers = ValidatedMembers [PublicationMember] [FilePath] BS.ByteString FilePath

data PublicationPlan
  = MkManagedPublicationPlan ManagedPublicationPlan
  | MkSingleFilePublicationPlan SingleFilePublicationPlan

data ManagedPublicationPlan = MkManagedPlan
  { managedDestination :: Destination,
    managedMembers :: ValidatedMembers,
    managedLoaderBytes :: BS.ByteString,
    managedGenerationRelative :: GenerationRelative
  }

data SingleFilePublicationPlan = MkSingleFilePlan
  { singleDestination :: Destination,
    singleFileBytes :: BS.ByteString
  }

data MemberRole = FishSource | NativeExecutable
  deriving stock (Show, Eq, Ord)

data PublicationMember = PublicationMember
  { memberPath :: FilePath,
    memberRole :: MemberRole,
    memberBytes :: BS.ByteString
  }
  deriving stock (Show, Eq)

memberMode :: PublicationMember -> FileMode
memberMode member = case memberRole member of
  FishSource -> 0o600
  NativeExecutable -> 0o700

data PublicationFailure = MkPublicationFailure
  { failureKindInternal :: PublicationFailureKind,
    failureMessageInternal :: Text,
    failureObservedEntryInternal :: Maybe ObservedEntry
  }
  deriving stock (Show, Eq)

data PublicationReceipt = MkPublicationReceipt
  { receiptDestinationInternal :: FilePath,
    receiptGenerationInternal :: Maybe FilePath,
    receiptWarningsInternal :: [Text]
  }
  deriving stock (Show, Eq)

newtype PublicationHook = MkPublicationHook
  { runPublicationHook :: PublicationStage -> IO (Maybe Text)
  }

data PublicationFailureKind
  = InvalidPublicationPlan
  | OwnershipMismatch
  | SymlinkConflict
  | GenerationCollision
  | PublicationIOFailure
  | InjectedPublicationFailure
  deriving stock (Show, Eq, Ord)

data ObservedEntry
  = ObservedEntryMissing
  | ObservedEntryMatchesPlanned
  | ObservedEntryDiffers
  | ObservedEntryUnreadable
  deriving stock (Show, Eq, Ord)

data PublicationStage
  = StageBeforeDestinationDirectoryCreate FilePath
  | StageAfterDestinationDirectoryCreate FilePath
  | StageAfterDestinationDirectoryParentFlush FilePath
  | StageBeforeGenerationStaging
  | StageAfterMemberWrite FilePath
  | StageAfterMemberFlush FilePath
  | StageAfterGenerationFlush
  | StageBeforeGenerationParentFlush FilePath
  | StageAfterGenerationPublish
  | StageBeforeEntryReplace
  | StageAfterEntryReplace
  | StageAfterParentFlush
  deriving stock (Show, Eq, Ord)

newtype PublicationAbort = MkPublicationAbort PublicationFailure
  deriving stock (Show)

instance Exception.Exception PublicationAbort

mkPublicationHook :: (PublicationStage -> IO (Maybe Text)) -> PublicationHook
mkPublicationHook = MkPublicationHook

publicationFailureKind :: PublicationFailure -> PublicationFailureKind
publicationFailureKind = failureKindInternal

publicationFailureMessage :: PublicationFailure -> Text
publicationFailureMessage = failureMessageInternal

publicationFailureObservedEntry :: PublicationFailure -> Maybe ObservedEntry
publicationFailureObservedEntry = failureObservedEntryInternal

publicationReceiptDestination :: PublicationReceipt -> FilePath
publicationReceiptDestination = receiptDestinationInternal

publicationReceiptGeneration :: PublicationReceipt -> Maybe FilePath
publicationReceiptGeneration = receiptGenerationInternal

publicationReceiptWarnings :: PublicationReceipt -> [Text]
publicationReceiptWarnings = receiptWarningsInternal

runHookOrAbort :: PublicationHook -> PublicationStage -> IO ()
runHookOrAbort hook stage =
  runPublicationHook hook stage >>= \case
    Nothing -> pure ()
    Just message -> abort InjectedPublicationFailure message

noPublicationHook :: PublicationHook
noPublicationHook = MkPublicationHook (const (pure Nothing))

mkFailure :: PublicationFailureKind -> Text -> PublicationFailure
mkFailure kind message =
  MkPublicationFailure
    { failureKindInternal = kind,
      failureMessageInternal = message,
      failureObservedEntryInternal = Nothing
    }

ioExceptionFailure :: Exception.IOException -> PublicationFailure
ioExceptionFailure =
  mkFailure PublicationIOFailure . toText . Exception.displayException

invalid :: Text -> Either PublicationFailure a
invalid = Left . mkFailure InvalidPublicationPlan

abort :: PublicationFailureKind -> Text -> IO a
abort kind message = Exception.throwIO (MkPublicationAbort (mkFailure kind message))

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Private, durable publication of admitted output bytes.
module Monk.Output.Publication
  ( PublicationPlan,
    PublicationMember (..),
    MemberRole (..),
    memberMode,
    generationRelativeDirectoryMembers,
    planManagedPublicationMembers,
    PublicationFailure,
    PublicationReceipt,
    PublicationHook,
    PublicationFailureKind (..),
    ObservedEntry (..),
    PublicationStage (..),
    generationRelativeDirectory,
    planManagedPublication,
    planSingleFilePublication,
    publishPublication,
    publishPublicationWithHook,
    mkPublicationHook,
    publicationFailureKind,
    publicationFailureMessage,
    publicationFailureObservedEntry,
    publicationReceiptDestination,
    publicationReceiptGeneration,
    publicationReceiptWarnings,
  )
where

import Control.Exception qualified as Exception
import Control.Monad (foldM)
import Data.Bits ((.&.))
import Data.ByteString qualified as BS
import Data.IORef qualified as IORef
import Data.List qualified as L
import Monk.Output.Publication.Lock
import Monk.Output.Publication.Manifest
import Monk.Output.Publication.Plan
import Monk.Output.Publication.Posix
import Monk.Output.Publication.Types
import System.Directory (createDirectory, listDirectory, makeAbsolute)
import System.FilePath qualified as FP
import System.IO.Error (isAlreadyExistsError)
import System.Posix.Files (fileMode, getSymbolicLinkStatus, isDirectory, isRegularFile, isSymbolicLink, rename)

publishPublication :: PublicationPlan -> IO (Either PublicationFailure PublicationReceipt)
publishPublication = publishPublicationWithHook noPublicationHook

publishPublicationWithHook ::
  PublicationHook ->
  PublicationPlan ->
  IO (Either PublicationFailure PublicationReceipt)
publishPublicationWithHook hook plan = do
  absoluteResult <- Exception.try @Exception.IOException (FP.normalise <$> makeAbsolute (destinationPath (publicationPlanDestination plan)))
  case absoluteResult of
    Left ioFailure -> pure (Left (ioExceptionFailure ioFailure))
    Right absoluteDestination -> do
      replacementAttempted <- IORef.newIORef False
      let expectedEntry = expectedEntryBytes plan
          publishBody :: forall owner. LockedDestination owner -> IO PublicationReceipt
          publishBody lock =
            case plan of
              MkManagedPublicationPlan MkManagedPlan {managedMembers, managedLoaderBytes, managedGenerationRelative} ->
                publishManaged
                  hook
                  replacementAttempted
                  lock
                  managedGenerationRelative
                  managedMembers
                  managedLoaderBytes
              MkSingleFilePublicationPlan MkSingleFilePlan {singleFileBytes} ->
                publishSingleFile
                  hook
                  replacementAttempted
                  lock
                  singleFileBytes
          publishUnderLock :: forall owner. LockedDestination owner -> IO (Either PublicationFailure PublicationReceipt)
          publishUnderLock lock = do
            result <- Exception.try @PublicationAbort (Exception.try @Exception.IOException (publishBody lock))
            case result of
              Left (MkPublicationAbort failure) ->
                Left <$> attachObservedEntry replacementAttempted absoluteDestination expectedEntry failure
              Right (Left ioFailure) ->
                Left <$> attachObservedEntry replacementAttempted absoluteDestination expectedEntry (ioExceptionFailure ioFailure)
              Right (Right receipt) -> pure (Right receipt)
          execute = do
            validateExistingPathComponents absoluteDestination
            ensureDestinationDirectory hook (FP.takeDirectory absoluteDestination)
            validateLockPath absoluteDestination
            validatedDestination <- either (Exception.throwIO . MkPublicationAbort) pure (validateDestination absoluteDestination)
            withAdvisoryDestinationLock validatedDestination publishUnderLock
      result <-
        withProcessDestinationLock
          absoluteDestination
          (Exception.try @PublicationAbort (Exception.try @Exception.IOException execute))
      case result of
        Left (MkPublicationAbort failure) ->
          Left <$> attachObservedEntry replacementAttempted absoluteDestination expectedEntry failure
        Right (Left ioFailure) ->
          Left <$> attachObservedEntry replacementAttempted absoluteDestination expectedEntry (ioExceptionFailure ioFailure)
        Right (Right publicationResult) -> pure publicationResult

publishManaged ::
  PublicationHook ->
  IORef.IORef Bool ->
  LockedDestination owner ->
  GenerationRelative ->
  ValidatedMembers ->
  BS.ByteString ->
  IO PublicationReceipt
publishManaged hook replacementAttempted lock generationRelative members loaderBytes = do
  let destination = destinationPath (lockedDestination lock)
  validateDestinationLayout destination
  let destinationDirectory = FP.takeDirectory destination
      managedRoot = destinationDirectory FP.</> managedRootName destination
      generationsRoot = managedRoot FP.</> generationsDirectoryName
      generation = destinationDirectory FP.</> generationRelativePath generationRelative
  ensureManagedRoot managedRoot generationsRoot
  generationStatus <- pathStatus generation
  case generationStatus of
    Nothing -> publishGeneration hook lock members
    Just status
      | isSymbolicLink status -> abort SymlinkConflict "generation directory is a symbolic link"
      | not (isDirectory status) -> abort GenerationCollision "generation path is not a directory"
      | otherwise -> do
          verifyGeneration generation members
          syncVerifiedGeneration hook managedRoot generationsRoot generation members
  runHookOrAbort hook StageAfterGenerationPublish
  replaceEntry hook replacementAttempted lock loaderBytes
  pure
    MkPublicationReceipt
      { receiptDestinationInternal = destination,
        receiptGenerationInternal = Just (generationRelativePath generationRelative),
        receiptWarningsInternal = []
      }

publishSingleFile :: PublicationHook -> IORef.IORef Bool -> LockedDestination owner -> BS.ByteString -> IO PublicationReceipt
publishSingleFile hook replacementAttempted lock contents = do
  let destination = destinationPath (lockedDestination lock)
  validateDestinationLayout destination
  replaceEntry hook replacementAttempted lock contents
  pure
    MkPublicationReceipt
      { receiptDestinationInternal = destination,
        receiptGenerationInternal = Nothing,
        receiptWarningsInternal = []
      }

publishGeneration :: PublicationHook -> LockedDestination owner -> ValidatedMembers -> IO ()
publishGeneration hook lock members = do
  runHookOrAbort hook StageBeforeGenerationStaging
  withStagedGeneration lock members $ \staging -> do
    durable <- sealGeneration hook staging
    commitGeneration lock hook durable

syncVerifiedGeneration ::
  PublicationHook ->
  FilePath ->
  FilePath ->
  FilePath ->
  ValidatedMembers ->
  IO ()
syncVerifiedGeneration hook managedRoot generationsRoot generation members = do
  forM_ (membersList members) $ \member -> do
    let relativePath = memberPath member
    syncRegularFile (generation FP.</> relativePath)
    runHookOrAbort hook (StageAfterMemberFlush relativePath)
  syncRegularFile (generation FP.</> generationManifestName)
  mapM_ (syncDirectory . (generation FP.</>)) (reverse (expectedMemberDirectories members))
  syncDirectory generation
  runHookOrAbort hook StageAfterGenerationFlush
  runHookOrAbort hook (StageBeforeGenerationParentFlush generationsRoot)
  syncDirectory generationsRoot
  syncDirectory managedRoot

ensureManagedRoot :: FilePath -> FilePath -> IO ()
ensureManagedRoot managedRoot generationsRoot = do
  rootStatus <- pathStatus managedRoot
  case rootStatus of
    Nothing -> do
      createDirectory managedRoot
      writeNewFileSynced (managedRoot FP.</> ownershipMarkerName) ownershipMarkerBytes
      createDirectory generationsRoot
      syncDirectory managedRoot
      syncDirectory (FP.takeDirectory managedRoot)
    Just status
      | isSymbolicLink status -> abort SymlinkConflict "managed output directory is a symbolic link"
      | not (isDirectory status) -> abort OwnershipMismatch "managed output path is not a directory"
      | otherwise -> verifyOwnershipMarker managedRoot
  generationsStatus <- pathStatus generationsRoot
  case generationsStatus of
    Nothing -> do
      createDirectory generationsRoot
      syncDirectory managedRoot
    Just status
      | isSymbolicLink status -> abort SymlinkConflict "generations directory is a symbolic link"
      | not (isDirectory status) -> abort OwnershipMismatch "generations path is not a directory"
      | otherwise -> pure ()

verifyOwnershipMarker :: FilePath -> IO ()
verifyOwnershipMarker managedRoot = do
  let markerPath = managedRoot FP.</> ownershipMarkerName
  markerStatus <- pathStatus markerPath
  case markerStatus of
    Just status
      | isSymbolicLink status -> abort SymlinkConflict "ownership marker is a symbolic link"
      | isRegularFile status -> do
          marker <- readFileNoFollow markerPath
          unless (marker == ownershipMarkerBytes) $
            abort OwnershipMismatch "managed output ownership marker does not match"
      | otherwise -> abort OwnershipMismatch "ownership marker is not a regular file"
    Nothing -> abort OwnershipMismatch "managed output directory has no ownership marker"

verifyGeneration :: FilePath -> ValidatedMembers -> IO ()
verifyGeneration generation members = do
  (actualDirectories, actualFiles) <- walkGeneration generation
  let expectedDirectories = expectedMemberDirectories members
      expectedFiles = L.sort (generationManifestName : map memberPath (membersList members))
  unless (actualDirectories == expectedDirectories && actualFiles == expectedFiles) $
    abort GenerationCollision "existing generation manifest shape differs"
  manifest <- readFileNoFollow (generation FP.</> generationManifestName)
  unless (manifest == generationManifest members) $
    abort GenerationCollision "existing generation manifest differs"
  forM_ (membersList members) $ \member -> do
    let relativePath = memberPath member
        expectedBytes = memberBytes member
    status <- getSymbolicLinkStatus (generation FP.</> relativePath)
    unless (fileMode status .&. 0o7777 == memberMode member) $ abort GenerationCollision ("existing generation member mode differs: " <> toText relativePath)
    actualBytes <- readFileNoFollow (generation FP.</> relativePath)
    unless (actualBytes == expectedBytes) $
      abort GenerationCollision ("existing generation member differs: " <> toText relativePath)

-- The expected native image was validated during planning and initial
-- staging. Reuse verifies validatedDestination as data; never execute an existing
-- destination member, which may have been replaced outside publication.

walkGeneration :: FilePath -> IO ([FilePath], [FilePath])
walkGeneration root = do
  (directories, files) <- go ""
  pure (L.sort directories, L.sort files)
  where
    go relativeDirectory = do
      let absoluteDirectory = root FP.</> relativeDirectory
      names <- L.sort <$> listDirectory absoluteDirectory
      foldM (visit relativeDirectory) ([], []) names
    visit relativeDirectory (directories, files) name = do
      let relativePath =
            if null relativeDirectory
              then name
              else relativeDirectory FP.</> name
          absolutePath = root FP.</> relativePath
      status <- requirePathStatus absolutePath
      if
        | isSymbolicLink status -> abort SymlinkConflict "generation contains a symbolic link"
        | isDirectory status -> do
            (nestedDirectories, nestedFiles) <- go relativePath
            pure (relativePath : nestedDirectories <> directories, nestedFiles <> files)
        | isRegularFile status -> pure (directories, relativePath : files)
        | otherwise -> abort GenerationCollision "generation contains an unsupported filesystem object"

replaceEntry ::
  PublicationHook ->
  IORef.IORef Bool ->
  LockedDestination owner ->
  BS.ByteString ->
  IO ()
replaceEntry hook replacementAttempted lock contents =
  let destination = destinationPath (lockedDestination lock)
   in withUniqueFile (FP.takeDirectory destination) ("." <> FP.takeFileName destination <> ".tmp") $ \temporary -> do
        writeExistingFileSynced temporary contents
        runHookOrAbort hook StageBeforeEntryReplace
        IORef.writeIORef replacementAttempted True
        rename temporary destination
        runHookOrAbort hook StageAfterEntryReplace
        syncDirectory (FP.takeDirectory destination)
        runHookOrAbort hook StageAfterParentFlush

validateDestinationLayout :: FilePath -> IO ()
validateDestinationLayout destination = do
  parentStatus <- requirePathStatus (FP.takeDirectory destination)
  when (isSymbolicLink parentStatus) $
    abort SymlinkConflict "destination directory is a symbolic link"
  unless (isDirectory parentStatus) $
    abort InvalidPublicationPlan "destination parent is not a directory"
  destinationStatus <- pathStatus destination
  case destinationStatus of
    Just status
      | isSymbolicLink status -> abort SymlinkConflict "destination entry is a symbolic link"
      | isDirectory status -> abort InvalidPublicationPlan "destination entry is a directory"
      | not (isRegularFile status) -> abort InvalidPublicationPlan "destination entry is not a regular file"
      | otherwise -> pure ()
    Nothing -> pure ()

validateExistingPathComponents :: FilePath -> IO ()
validateExistingPathComponents destination =
  forM_ (absolutePathComponents destination) $ \component ->
    pathStatus component >>= \case
      Just status
        | isSymbolicLink status -> abort SymlinkConflict ("destination path contains a symbolic link: " <> toText component)
        | component /= destination && not (isDirectory status) ->
            abort InvalidPublicationPlan ("destination ancestor is not a directory: " <> toText component)
        | otherwise -> pure ()
      Nothing -> pure ()

ensureDestinationDirectory :: PublicationHook -> FilePath -> IO ()
ensureDestinationDirectory hook destinationDirectory =
  forM_ (absolutePathComponents destinationDirectory) $ \component ->
    pathStatus component >>= \case
      Just status
        | isSymbolicLink status -> abort SymlinkConflict ("destination path contains a symbolic link: " <> toText component)
        | not (isDirectory status) -> abort InvalidPublicationPlan ("destination parent is not a directory: " <> toText component)
        | otherwise -> pure ()
      Nothing -> do
        runHookOrAbort hook (StageBeforeDestinationDirectoryCreate component)
        created <- createDirectoryChecked component
        when created (runHookOrAbort hook (StageAfterDestinationDirectoryCreate component))
        let parent = FP.takeDirectory component
        syncDirectory parent
        when created (runHookOrAbort hook (StageAfterDestinationDirectoryParentFlush parent))

createDirectoryChecked :: FilePath -> IO Bool
createDirectoryChecked path =
  Exception.catch
    (createDirectory path >> pure True)
    ( \err ->
        if isAlreadyExistsError err
          then do
            status <- requirePathStatus path
            if
              | isSymbolicLink status -> abort SymlinkConflict "destination path contains a symbolic link"
              | isDirectory status -> pure False
              | otherwise -> abort InvalidPublicationPlan "destination parent is not a directory"
          else Exception.throwIO (err :: Exception.IOException)
    )

absolutePathComponents :: FilePath -> [FilePath]
absolutePathComponents path =
  case FP.splitDirectories path of
    [] -> []
    root : rest -> scanl (FP.</>) root rest

attachObservedEntry ::
  IORef.IORef Bool ->
  FilePath ->
  BS.ByteString ->
  PublicationFailure ->
  IO PublicationFailure
attachObservedEntry replacementAttempted destination expected failure = do
  attempted <- IORef.readIORef replacementAttempted
  if attempted
    then do
      observed <- inspectEntry destination expected
      pure failure {failureObservedEntryInternal = Just observed}
    else pure failure

inspectEntry :: FilePath -> BS.ByteString -> IO ObservedEntry
inspectEntry destination expected = do
  inspected <- Exception.try @Exception.IOException $ do
    pathStatus destination >>= \case
      Nothing -> pure ObservedEntryMissing
      Just status
        | isSymbolicLink status || not (isRegularFile status) -> pure ObservedEntryDiffers
        | otherwise -> do
            actual <- readFileNoFollow destination
            pure
              ( if actual == expected
                  then ObservedEntryMatchesPlanned
                  else ObservedEntryDiffers
              )
  pure (fromRight ObservedEntryUnreadable inspected)

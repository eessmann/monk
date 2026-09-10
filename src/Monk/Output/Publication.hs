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

import Control.Concurrent.MVar qualified as MVar
import Control.Exception qualified as Exception
import Control.Monad (foldM)
import Data.Bits (shiftR, (.&.))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.IORef qualified as IORef
import Data.List qualified as L
import Data.Text.Encoding qualified as Text
import Monk.Runtime.Compatibility (checkRuntimeFile)
import Monk.Runtime.Digest (sha256)
import System.Directory
  ( createDirectory,
    createDirectoryIfMissing,
    listDirectory,
    makeAbsolute,
    removeDirectoryRecursive,
    removeFile,
  )
import System.FilePath qualified as FP
import System.IO (SeekMode (AbsoluteSeek))
import System.IO qualified as IO
import System.IO.Error (ioError, isAlreadyExistsError, isDoesNotExistError, isEOFError, userError)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Files
  ( FileStatus,
    fileMode,
    getSymbolicLinkStatus,
    isDirectory,
    isRegularFile,
    isSymbolicLink,
    rename,
    setFdMode,
  )
import System.Posix.IO
  ( LockRequest (Unlock, WriteLock),
    OpenFileFlags (..),
    OpenMode (ReadOnly, ReadWrite, WriteOnly),
    closeFd,
    defaultFileFlags,
    openFd,
    setLock,
    waitToSetLock,
  )
import System.Posix.IO.ByteString qualified as PosixBS
import System.Posix.Types (Fd, FileMode)
import System.Posix.Unistd (fileSynchronise)

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

data PublicationPlan
  = MkManagedPublicationPlan ManagedPublicationPlan
  | MkSingleFilePublicationPlan SingleFilePublicationPlan

data ManagedPublicationPlan = MkManagedPlan
  { managedDestination :: FilePath,
    managedMembers :: [PublicationMember],
    managedLoaderBytes :: BS.ByteString,
    managedGenerationRelative :: FilePath
  }

data SingleFilePublicationPlan = MkSingleFilePlan
  { singleDestination :: FilePath,
    singleFileBytes :: BS.ByteString
  }

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

generationRelativeDirectory :: FilePath -> [(FilePath, BS.ByteString)] -> Either PublicationFailure FilePath
generationRelativeDirectory destination = generationRelativeDirectoryMembers destination . map (\(path, bytes) -> PublicationMember path FishSource bytes)

generationRelativeDirectoryMembers :: FilePath -> [PublicationMember] -> Either PublicationFailure FilePath
generationRelativeDirectoryMembers destination members = do
  validateDestination destination
  sortedMembers <- validateMembers members
  pure (managedRootName destination FP.</> generationsDirectoryName FP.</> generationDigest sortedMembers)

planManagedPublication :: FilePath -> [(FilePath, BS.ByteString)] -> BS.ByteString -> Either PublicationFailure PublicationPlan
planManagedPublication destination members = planManagedPublicationMembers destination [PublicationMember path FishSource bytes | (path, bytes) <- members]

planManagedPublicationMembers :: FilePath -> [PublicationMember] -> BS.ByteString -> Either PublicationFailure PublicationPlan
planManagedPublicationMembers destination members loaderBytes = do
  generation <- generationRelativeDirectoryMembers destination members
  sortedMembers <- validateMembers members
  pure (MkManagedPublicationPlan (MkManagedPlan destination sortedMembers loaderBytes generation))

planSingleFilePublication ::
  FilePath ->
  BS.ByteString ->
  Either PublicationFailure PublicationPlan
planSingleFilePublication destination contents = do
  validateDestination destination
  pure
    ( MkSingleFilePublicationPlan
        MkSingleFilePlan
          { singleDestination = destination,
            singleFileBytes = contents
          }
    )

publishPublication :: PublicationPlan -> IO (Either PublicationFailure PublicationReceipt)
publishPublication = publishPublicationWithHook noPublicationHook

publishPublicationWithHook ::
  PublicationHook ->
  PublicationPlan ->
  IO (Either PublicationFailure PublicationReceipt)
publishPublicationWithHook hook plan = do
  absoluteResult <- Exception.try @Exception.IOException (FP.normalise <$> makeAbsolute (publicationPlanDestination plan))
  case absoluteResult of
    Left ioFailure -> pure (Left (ioExceptionFailure ioFailure))
    Right absoluteDestination -> do
      replacementAttempted <- IORef.newIORef False
      let expectedEntry = expectedEntryBytes plan
          publishBody =
            case plan of
              MkManagedPublicationPlan MkManagedPlan {managedMembers, managedLoaderBytes, managedGenerationRelative} ->
                publishManaged
                  hook
                  replacementAttempted
                  absoluteDestination
                  managedGenerationRelative
                  managedMembers
                  managedLoaderBytes
              MkSingleFilePublicationPlan MkSingleFilePlan {singleFileBytes} ->
                publishSingleFile
                  hook
                  replacementAttempted
                  absoluteDestination
                  singleFileBytes
          publishUnderLock = do
            result <- Exception.try @PublicationAbort (Exception.try @Exception.IOException publishBody)
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
            withAdvisoryDestinationLock absoluteDestination publishUnderLock
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

publishManaged ::
  PublicationHook ->
  IORef.IORef Bool ->
  FilePath ->
  FilePath ->
  [PublicationMember] ->
  BS.ByteString ->
  IO PublicationReceipt
publishManaged hook replacementAttempted destination generationRelative members loaderBytes = do
  validateDestinationLayout destination
  let destinationDirectory = FP.takeDirectory destination
      managedRoot = destinationDirectory FP.</> managedRootName destination
      generationsRoot = managedRoot FP.</> generationsDirectoryName
      generation = destinationDirectory FP.</> generationRelative
  ensureManagedRoot managedRoot generationsRoot
  generationStatus <- pathStatus generation
  case generationStatus of
    Nothing -> publishGeneration hook managedRoot generationsRoot generation members
    Just status
      | isSymbolicLink status -> abort SymlinkConflict "generation path is a symbolic link"
      | not (isDirectory status) -> abort GenerationCollision "generation label is occupied by a non-directory"
      | otherwise -> do
          verifyGeneration generation members
          syncVerifiedGeneration hook managedRoot generationsRoot generation members
  runHookOrAbort hook StageAfterGenerationPublish
  replaceEntry hook replacementAttempted destination loaderBytes
  pure
    MkPublicationReceipt
      { receiptDestinationInternal = destination,
        receiptGenerationInternal = Just generationRelative,
        receiptWarningsInternal = []
      }

publishSingleFile ::
  PublicationHook ->
  IORef.IORef Bool ->
  FilePath ->
  BS.ByteString ->
  IO PublicationReceipt
publishSingleFile hook replacementAttempted destination contents = do
  validateDestinationLayout destination
  replaceEntry hook replacementAttempted destination contents
  pure
    MkPublicationReceipt
      { receiptDestinationInternal = destination,
        receiptGenerationInternal = Nothing,
        receiptWarningsInternal = []
      }

publishGeneration ::
  PublicationHook ->
  FilePath ->
  FilePath ->
  FilePath ->
  [PublicationMember] ->
  IO ()
publishGeneration hook managedRoot generationsRoot generation members = do
  runHookOrAbort hook StageBeforeGenerationStaging
  withUniqueDirectory managedRoot ".staging-generation" $ \staging -> do
    let memberDirectories = expectedMemberDirectories members
    forM_ memberDirectories $ \directory ->
      createDirectoryIfMissing True (staging FP.</> directory)
    forM_ members $ \member ->
      writeMemberFile hook (staging FP.</> memberPath member) member
    writeNewFileSynced (staging FP.</> generationManifestName) (generationManifest members)
    mapM_ (syncDirectory . (staging FP.</>)) (reverse memberDirectories)
    syncDirectory staging
    runHookOrAbort hook StageAfterGenerationFlush
    rename staging generation
    runHookOrAbort hook (StageBeforeGenerationParentFlush generationsRoot)
    syncDirectory generationsRoot

syncVerifiedGeneration ::
  PublicationHook ->
  FilePath ->
  FilePath ->
  FilePath ->
  [PublicationMember] ->
  IO ()
syncVerifiedGeneration hook managedRoot generationsRoot generation members = do
  forM_ members $ \member -> do
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

verifyGeneration :: FilePath -> [PublicationMember] -> IO ()
verifyGeneration generation members = do
  (actualDirectories, actualFiles) <- walkGeneration generation
  let expectedDirectories = expectedMemberDirectories members
      expectedFiles = L.sort (generationManifestName : map memberPath members)
  unless (actualDirectories == expectedDirectories && actualFiles == expectedFiles) $
    abort GenerationCollision "existing generation manifest shape differs"
  manifest <- readFileNoFollow (generation FP.</> generationManifestName)
  unless (manifest == generationManifest members) $
    abort GenerationCollision "existing generation manifest differs"
  forM_ members $ \member -> do
    let relativePath = memberPath member
        expectedBytes = memberBytes member
    status <- getSymbolicLinkStatus (generation FP.</> relativePath)
    unless (fileMode status .&. 0o7777 == memberMode member) $ abort GenerationCollision ("existing generation member mode differs: " <> toText relativePath)
    actualBytes <- readFileNoFollow (generation FP.</> relativePath)
    unless (actualBytes == expectedBytes) $
      abort GenerationCollision ("existing generation member differs: " <> toText relativePath)

-- The expected native image was validated during planning and initial
-- staging. Reuse verifies identity as data; never execute an existing
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
            pure (relativePath : directories <> nestedDirectories, files <> nestedFiles)
        | isRegularFile status -> pure (directories, relativePath : files)
        | otherwise -> abort GenerationCollision "generation contains an unsupported filesystem object"

replaceEntry ::
  PublicationHook ->
  IORef.IORef Bool ->
  FilePath ->
  BS.ByteString ->
  IO ()
replaceEntry hook replacementAttempted destination contents =
  withUniqueFile (FP.takeDirectory destination) ("." <> FP.takeFileName destination <> ".tmp") $ \temporary -> do
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

withAdvisoryDestinationLock :: FilePath -> IO a -> IO a
withAdvisoryDestinationLock destination action = do
  let lockPath = FP.takeDirectory destination FP.</> ("." <> FP.takeFileName destination <> ".monk.lock")
  Exception.bracket (openLockFile lockPath) closeFd $ \lockFd ->
    Exception.bracket_
      (waitToSetLock lockFd writeLock)
      (setLock lockFd unlock)
      action
  where
    writeLock = (WriteLock, AbsoluteSeek, 0, 0)
    unlock = (Unlock, AbsoluteSeek, 0, 0)

openLockFile :: FilePath -> IO Fd
openLockFile path =
  openFd
    path
    ReadWrite
    defaultFileFlags
      { creat = Just ownerReadWriteMode,
        nofollow = True,
        cloexec = True
      }

validateLockPath :: FilePath -> IO ()
validateLockPath destination = do
  let lockPath = FP.takeDirectory destination FP.</> ("." <> FP.takeFileName destination <> ".monk.lock")
  pathStatus lockPath >>= \case
    Just status
      | isSymbolicLink status -> abort SymlinkConflict "destination lock is a symbolic link"
      | isDirectory status -> abort PublicationIOFailure "destination lock path is a directory"
      | not (isRegularFile status) -> abort PublicationIOFailure "destination lock path is not a regular file"
      | otherwise -> pure ()
    Nothing -> pure ()

withProcessDestinationLock :: FilePath -> IO a -> IO a
withProcessDestinationLock destination action =
  Exception.bracket
    (registerProcessLock destination)
    (const (unregisterProcessLock destination))
    (\lock -> Exception.bracket_ (MVar.takeMVar lock) (MVar.putMVar lock ()) action)

registerProcessLock :: FilePath -> IO (MVar.MVar ())
registerProcessLock destination =
  MVar.modifyMVar processLocks $ \locks ->
    case [(lock, users) | (path, lock, users) <- locks, path == destination] of
      (lock, users) : _ ->
        pure
          ( (destination, lock, users + 1) : filter (\(path, _, _) -> path /= destination) locks,
            lock
          )
      [] -> do
        lock <- MVar.newMVar ()
        pure ((destination, lock, 1) : locks, lock)

unregisterProcessLock :: FilePath -> IO ()
unregisterProcessLock destination =
  MVar.modifyMVar_ processLocks $ \locks ->
    pure (concatMap release locks)
  where
    release entry@(path, lock, users)
      | path /= destination = [entry]
      | users > 1 = [(path, lock, users - 1)]
      | otherwise = []

{-# NOINLINE processLocks #-}
processLocks :: MVar.MVar [(FilePath, MVar.MVar (), Int)]
processLocks = unsafePerformIO (MVar.newMVar [])

runHookOrAbort :: PublicationHook -> PublicationStage -> IO ()
runHookOrAbort hook stage =
  runPublicationHook hook stage >>= \case
    Nothing -> pure ()
    Just message -> abort InjectedPublicationFailure message

noPublicationHook :: PublicationHook
noPublicationHook = MkPublicationHook (const (pure Nothing))

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

withUniqueDirectory :: FilePath -> String -> (FilePath -> IO a) -> IO a
withUniqueDirectory parent prefix action =
  withOwnedTemporary parent prefix $ \path handle -> do
    IO.hClose handle
    removeFile path
    createDirectory path
    action path

withUniqueFile :: FilePath -> String -> (FilePath -> IO a) -> IO a
withUniqueFile parent prefix action =
  withOwnedTemporary parent prefix $ \path handle -> do
    IO.hClose handle
    action path

withOwnedTemporary :: FilePath -> String -> (FilePath -> IO.Handle -> IO a) -> IO a
withOwnedTemporary parent prefix action =
  Exception.bracket
    (IO.openBinaryTempFile parent prefix)
    cleanup
    (uncurry action)
  where
    cleanup (path, handle) = do
      Exception.catch (IO.hClose handle) (const (pure ()) :: Exception.IOException -> IO ())
      status <- pathStatus path
      case status of
        Nothing -> pure ()
        Just value
          | isDirectory value -> removeDirectoryRecursive path
          | otherwise -> removeFile path

writeMemberFile :: PublicationHook -> FilePath -> PublicationMember -> IO ()
writeMemberFile hook absolutePath member = do
  Exception.bracket (openNewFile absolutePath) closeFd $ \fd -> do
    writeAll fd (memberBytes member)
    setFdMode fd (memberMode member)
    runHookOrAbort hook (StageAfterMemberWrite (memberPath member))
    fileSynchronise fd
    runHookOrAbort hook (StageAfterMemberFlush (memberPath member))
  validateNativeMember absolutePath member

validateNativeMember :: FilePath -> PublicationMember -> IO ()
validateNativeMember path member = when (memberRole member == NativeExecutable) $ do
  checked <- checkRuntimeFile path []
  either (abort InvalidPublicationPlan . toText) pure checked

writeNewFileSynced :: FilePath -> BS.ByteString -> IO ()
writeNewFileSynced path contents =
  Exception.bracket (openNewFile path) closeFd $ \fd -> do
    writeAll fd contents
    fileSynchronise fd

writeExistingFileSynced :: FilePath -> BS.ByteString -> IO ()
writeExistingFileSynced path contents =
  Exception.bracket (openExistingFile path) closeFd $ \fd -> do
    writeAll fd contents
    fileSynchronise fd

openNewFile :: FilePath -> IO Fd
openNewFile path =
  openFd
    path
    WriteOnly
    defaultFileFlags
      { exclusive = True,
        nofollow = True,
        creat = Just ownerReadWriteMode,
        cloexec = True
      }

openExistingFile :: FilePath -> IO Fd
openExistingFile path =
  openFd
    path
    WriteOnly
    defaultFileFlags
      { trunc = True,
        nofollow = True,
        cloexec = True
      }

readFileNoFollow :: FilePath -> IO BS.ByteString
readFileNoFollow path =
  Exception.bracket
    (openFd path ReadOnly defaultFileFlags {nofollow = True, cloexec = True})
    closeFd
    readAll

syncRegularFile :: FilePath -> IO ()
syncRegularFile path =
  Exception.bracket
    (openFd path ReadOnly defaultFileFlags {nofollow = True, cloexec = True})
    closeFd
    fileSynchronise

writeAll :: Fd -> BS.ByteString -> IO ()
writeAll fd = go
  where
    go remaining
      | BS.null remaining = pure ()
      | otherwise = do
          written <- PosixBS.fdWrite fd remaining
          when (written <= 0) (ioError (userError "write returned no progress"))
          go (BS.drop (fromIntegral written) remaining)

readAll :: Fd -> IO BS.ByteString
readAll fd = go []
  where
    go chunks = do
      result <- Exception.try @Exception.IOException (PosixBS.fdRead fd 32768)
      case result of
        Left err
          | isEOFError err -> pure (BS.concat (reverse chunks))
          | otherwise -> Exception.throwIO err
        Right chunk
          | BS.null chunk -> pure (BS.concat (reverse chunks))
          | otherwise -> go (chunk : chunks)

syncDirectory :: FilePath -> IO ()
syncDirectory path =
  Exception.bracket
    (openFd path ReadOnly defaultFileFlags {nofollow = True, directory = True, cloexec = True})
    closeFd
    fileSynchronise

pathStatus :: FilePath -> IO (Maybe FileStatus)
pathStatus path =
  Exception.catch
    (Just <$> getSymbolicLinkStatus path)
    (\err -> if isDoesNotExistError err then pure Nothing else Exception.throwIO (err :: Exception.IOException))

requirePathStatus :: FilePath -> IO FileStatus
requirePathStatus path =
  pathStatus path >>= \case
    Nothing -> ioError (userError ("required path does not exist: " <> path))
    Just status -> pure status

validateDestination :: FilePath -> Either PublicationFailure ()
validateDestination destination
  | null destination = invalid "destination is empty"
  | '\0' `elem` destination = invalid "destination contains a NUL byte"
  | FP.hasTrailingPathSeparator destination = invalid "destination must name a file"
  | FP.normalise destination /= destination = invalid "destination path is not normalized"
  | FP.takeFileName destination `elem` ["", ".", ".."] = invalid "destination must have an entry basename"
  | otherwise = Right ()

validateMembers ::
  [PublicationMember] ->
  Either PublicationFailure [PublicationMember]
validateMembers members
  | null members = invalid "managed publication has no members"
  | otherwise = do
      traverse_ (validateMemberPath . memberPath) members
      let sortedMembers = L.sortOn memberPath members
          paths = map memberPath sortedMembers
      when (hasDuplicates paths) (invalid "managed publication has duplicate member paths")
      when (hasPathCollision paths) (invalid "managed publication has a file-directory member collision")
      pure sortedMembers

validateMemberPath :: FilePath -> Either PublicationFailure ()
validateMemberPath path
  | null path = invalid "member path is empty"
  | '\0' `elem` path = invalid "member path contains a NUL byte"
  | FP.isAbsolute path = invalid "member path must be relative"
  | FP.hasTrailingPathSeparator path = invalid "member path must name a file"
  | FP.normalise path /= path = invalid "member path is not normalized"
  | any (`elem` ["", ".", ".."]) (FP.splitDirectories path) = invalid "member path contains traversal"
  | path == generationManifestName = invalid "member path uses the reserved manifest name"
  | otherwise = Right ()

hasDuplicates :: (Eq a) => [a] -> Bool
hasDuplicates = any ((> 1) . length) . L.group

hasPathCollision :: [FilePath] -> Bool
hasPathCollision paths =
  or
    [ (left <> [FP.pathSeparator]) `L.isPrefixOf` right
    | left <- paths,
      right <- paths,
      left /= right
    ]

expectedMemberDirectories :: [PublicationMember] -> [FilePath]
expectedMemberDirectories =
  L.sort
    . L.nub
    . concatMap (parentDirectories . FP.takeDirectory . memberPath)
  where
    parentDirectories "." = []
    parentDirectories directory =
      let parent = FP.takeDirectory directory
       in parentDirectories parent <> [directory]

generationManifest :: [PublicationMember] -> BS.ByteString
generationManifest members =
  BS.concat
    ( generationManifestHeader
        : concatMap memberEntry members
    )
  where
    memberEntry member =
      [ frameBytes (Text.encodeUtf8 (toText (memberPath member))),
        frameBytes (BS8.pack (show (memberRole member))),
        frameBytes (BS8.pack (show (memberMode member))),
        frameBytes (encodeWord64 (fromIntegral (BS.length (memberBytes member)))),
        frameBytes (sha256 (memberBytes member))
      ]

generationDigest :: [PublicationMember] -> FilePath
generationDigest = BS8.unpack . sha256 . generationManifest

frameBytes :: BS.ByteString -> BS.ByteString
frameBytes bytes = encodeWord64 (fromIntegral (BS.length bytes)) <> bytes

encodeWord64 :: Word64 -> BS.ByteString
encodeWord64 value =
  BS.pack
    [ byte 56,
      byte 48,
      byte 40,
      byte 32,
      byte 24,
      byte 16,
      byte 8,
      byte 0
    ]
  where
    byte shift = fromIntegral ((value `shiftR` shift) .&. 0xff) :: Word8

expectedEntryBytes :: PublicationPlan -> BS.ByteString
expectedEntryBytes = \case
  MkManagedPublicationPlan MkManagedPlan {managedLoaderBytes} -> managedLoaderBytes
  MkSingleFilePublicationPlan MkSingleFilePlan {singleFileBytes} -> singleFileBytes

publicationPlanDestination :: PublicationPlan -> FilePath
publicationPlanDestination = \case
  MkManagedPublicationPlan MkManagedPlan {managedDestination} -> managedDestination
  MkSingleFilePublicationPlan MkSingleFilePlan {singleDestination} -> singleDestination

managedRootName :: FilePath -> FilePath
managedRootName destination = "." <> FP.takeFileName destination <> ".monk"

ownershipMarkerName :: FilePath
ownershipMarkerName = ".monk-owner"

ownershipMarkerBytes :: BS.ByteString
ownershipMarkerBytes = "monk-managed-output-v1\n"

generationsDirectoryName :: FilePath
generationsDirectoryName = "generations"

generationManifestName :: FilePath
generationManifestName = ".monk-manifest"

generationManifestHeader :: BS.ByteString
generationManifestHeader = "monk-generation-v2\n"

ownerReadWriteMode :: FileMode
ownerReadWriteMode = 0o600

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

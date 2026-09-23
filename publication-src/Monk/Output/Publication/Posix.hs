{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Private publication posix boundary.
module Monk.Output.Publication.Posix
  ( StagedGeneration,
    DurableGeneration,
    withStagedGeneration,
    sealGeneration,
    commitGeneration,
    withUniqueFile,
    writeMemberFile,
    writeNewFileSynced,
    writeExistingFileSynced,
    readFileNoFollow,
    syncRegularFile,
    syncDirectory,
    pathStatus,
    requirePathStatus,
  )
where

import Control.Exception qualified as Exception
import Data.ByteString qualified as BS
import Monk.Output.Publication.Lease (Lease, newLease, revokeLease, withLease)
import Monk.Output.Publication.Lock (LockedDestination, lockedDestination, withLockedDestination)
import Monk.Output.Publication.Manifest (expectedMemberDirectories, generationManifest, generationManifestName, membersList, ownerReadWriteMode)
import Monk.Output.Publication.Plan (destinationPath, generationFor, generationRelativePath)
import Monk.Output.Publication.Types
import Monk.Runtime.Compatibility (checkRuntimeFile)
import System.Directory (createDirectory, createDirectoryIfMissing, removeDirectoryRecursive, removeFile)
import System.FilePath qualified as FP
import System.IO qualified as IO
import System.IO.Error (ioError, isDoesNotExistError, isEOFError, userError)
import System.IO.Temp (createTempDirectory)
import System.Posix.Files (FileStatus, getSymbolicLinkStatus, isDirectory, rename, setFdMode)
import System.Posix.IO (OpenFileFlags (..), OpenMode (ReadOnly, WriteOnly), closeFd, defaultFileFlags, openFd)
import System.Posix.IO.ByteString qualified as PosixBS
import System.Posix.Types (Fd)
import System.Posix.Unistd (fileSynchronise)

-- The staging directory is created atomically on the destination filesystem.
-- Only a completely written and flushed generation can cross the rename boundary.
type role StagedGeneration nominal nominal

data StagedGeneration owner stage = StagedGeneration (LockedDestination owner) Lease FilePath FilePath ValidatedMembers

type role DurableGeneration nominal nominal

data DurableGeneration owner stage = DurableGeneration Lease FilePath FilePath

withStagedGeneration :: LockedDestination owner -> ValidatedMembers -> (forall stage. StagedGeneration owner stage -> IO a) -> IO a
withStagedGeneration lock layout action = do
  let destination = lockedDestination lock
      generation = FP.takeDirectory (destinationPath destination) FP.</> generationRelativePath (generationFor destination layout)
      managedRoot = FP.takeDirectory (FP.takeDirectory generation)
      acquire = withLockedDestination lock $ do
        workspace <- createTempDirectory managedRoot ".staging-generation"
        ( do
            -- The container is private and atomic; the generation keeps the
            -- ordinary directory mode, including the caller's umask.
            let staging = workspace FP.</> "generation"
            createDirectory staging
            lease <- newLease
            pure (workspace, lease, StagedGeneration lock lease staging generation layout)
          )
          `Exception.onException` removeDirectoryRecursive workspace
      release (workspace, lease, _) = do
        revokeLease lease
        removeDirectoryRecursive workspace
  Exception.bracket acquire release (\(_, _, staged) -> action staged)

sealGeneration :: PublicationHook -> StagedGeneration owner stage -> IO (DurableGeneration owner stage)
sealGeneration hook (StagedGeneration lock lease staging generation layout) =
  withLockedDestination lock $ withLease lease $ do
    let directories = expectedMemberDirectories layout
    forM_ directories $ \directory -> createDirectoryIfMissing True (staging FP.</> directory)
    forM_ (membersList layout) $ \member -> writeMemberFile hook (staging FP.</> memberPath member) member
    writeNewFileSynced (staging FP.</> generationManifestName) (generationManifest layout)
    mapM_ (syncDirectory . (staging FP.</>)) (reverse directories)
    syncDirectory staging
    runHookOrAbort hook StageAfterGenerationFlush
    pure (DurableGeneration lease staging generation)

commitGeneration :: LockedDestination owner -> PublicationHook -> DurableGeneration owner stage -> IO ()
commitGeneration lock hook (DurableGeneration lease staging generation) =
  withLockedDestination lock $ withLease lease $ do
    rename staging generation
    let parent = FP.takeDirectory generation
    runHookOrAbort hook (StageBeforeGenerationParentFlush parent)
    syncDirectory parent

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

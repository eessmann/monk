{-# LANGUAGE RoleAnnotations #-}

-- | Private publication lock boundary.
module Monk.Output.Publication.Lock
  ( LockedDestination,
    lockedDestination,
    withLockedDestination,
    withAdvisoryDestinationLock,
    withProcessDestinationLock,
    validateLockPath,
  )
where

import Control.Concurrent.MVar qualified as MVar
import Control.Exception qualified as Exception
import Monk.Output.Publication.Lease (Lease, newLease, revokeLease, withLease)
import Monk.Output.Publication.Manifest (ownerReadWriteMode)
import Monk.Output.Publication.Plan (destinationPath)
import Monk.Output.Publication.Types
import System.FilePath qualified as FP
import System.IO (SeekMode (AbsoluteSeek))
import System.IO.Error (isDoesNotExistError)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Files (getSymbolicLinkStatus, isDirectory, isRegularFile, isSymbolicLink)
import System.Posix.IO (LockRequest (Unlock, WriteLock), OpenFileFlags (..), OpenMode (ReadWrite), closeFd, defaultFileFlags, openFd, setLock, waitToSetLock)
import System.Posix.Types (Fd)

-- Each lock callback receives a distinct owner that cannot be coerced.
type role LockedDestination nominal

data LockedDestination owner = LockedDestination Destination Lease

lockedDestination :: LockedDestination owner -> Destination
lockedDestination (LockedDestination path _) = path

withLockedDestination :: LockedDestination owner -> IO a -> IO a
withLockedDestination (LockedDestination _ lease) = withLease lease

withAdvisoryDestinationLock :: Destination -> (forall owner. LockedDestination owner -> IO a) -> IO a
withAdvisoryDestinationLock validatedDestination action = do
  let destination = destinationPath validatedDestination
      lockPath = FP.takeDirectory destination FP.</> ("." <> FP.takeFileName destination <> ".monk.lock")
  Exception.bracket (openLockFile lockPath) closeFd $ \lockFd ->
    Exception.bracket_
      (waitToSetLock lockFd writeLock)
      (setLock lockFd unlock)
      (Exception.bracket newLease revokeLease (action . LockedDestination validatedDestination))
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
  result <- Exception.try @Exception.IOException (getSymbolicLinkStatus lockPath)
  case result of
    Right status
      | isSymbolicLink status -> abort SymlinkConflict "destination lock is a symbolic link"
      | isDirectory status -> abort PublicationIOFailure "destination lock path is a directory"
      | not (isRegularFile status) -> abort PublicationIOFailure "destination lock path is not a regular file"
      | otherwise -> pure ()
    Left failure
      | isDoesNotExistError failure -> pure ()
      | otherwise -> Exception.throwIO failure

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

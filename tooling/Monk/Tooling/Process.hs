-- | Run external tools while preserving raw streams and bounding their lifetime.
module Monk.Tooling.Process
  ( ProcessSpec (..),
    ProcessResult (..),
    runProcess,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar qualified as MVar
import Control.Exception (IOException, catch, evaluate, throwIO, try)
import Data.ByteString qualified as B
import GHC.Clock (getMonotonicTimeNSec)
import System.Exit (ExitCode)
import System.IO (hClose)
import System.Posix.Signals (sigKILL, signalProcessGroup)
import System.Process
  ( CreateProcess (..),
    ProcessHandle,
    StdStream (CreatePipe),
    createProcess,
    getPid,
    getProcessExitCode,
    proc,
    waitForProcess,
  )
import System.Timeout (timeout)

data ProcessSpec = ProcessSpec
  { executable :: FilePath,
    arguments :: [String],
    workingDirectory :: Maybe FilePath,
    environment :: Maybe [(String, String)],
    stdinBytes :: B.ByteString,
    timeoutMicros :: Int
  }
  deriving stock (Eq, Show)

data ProcessResult = ProcessResult
  { processExit :: ExitCode,
    processStdout :: B.ByteString,
    processStderr :: B.ByteString,
    processTimedOut :: Bool
  }
  deriving stock (Eq, Show)

runProcess :: ProcessSpec -> IO ProcessResult
runProcess spec = do
  let command =
        (proc (executable spec) (arguments spec))
          { cwd = workingDirectory spec,
            env = environment spec,
            std_in = CreatePipe,
            std_out = CreatePipe,
            std_err = CreatePipe,
            create_group = True
          }
  (Just input, Just output, Just errors, process) <- createProcess command
  processGroup <- getPid process
  stdoutResult <- capture output
  stderrResult <- capture errors
  _ <- forkIO $ do
    _ <- try @IOException (B.hPut input (stdinBytes spec))
    hClose input `catch` ignoreIOException
  started <- getMonotonicTimeNSec
  let deadline = started + fromIntegral (max 0 (timeoutMicros spec)) * 1000
      stopGroup = mapM_ (\pid -> signalProcessGroup sigKILL pid `catch` ignoreIOException) processGroup
  completed <- pollExit process deadline
  status <- case completed of
    Just exitCode -> pure exitCode
    Nothing -> do
      stopGroup
      waitForProcess process
  remaining <- remainingMicros deadline
  captured <- timeout remaining ((,) <$> MVar.readMVar stdoutResult <*> MVar.readMVar stderrResult)
  streams <- case captured of
    Just pair -> pure pair
    Nothing -> do
      stopGroup
      drained <- timeout 5000000 ((,) <$> MVar.readMVar stdoutResult <*> MVar.readMVar stderrResult)
      maybe (fail "subprocess pipes remained open after process-group termination") pure drained
  let (stdoutBytes, stderrBytes) = streams
  out <- either throwIO pure stdoutBytes
  err <- either throwIO pure stderrBytes
  pure (ProcessResult status out err (isNothing completed || isNothing captured))

remainingMicros :: Word64 -> IO Int
remainingMicros deadline = do
  now <- getMonotonicTimeNSec
  pure (fromIntegral ((deadline - min deadline now) `div` 1000))

pollExit :: ProcessHandle -> Word64 -> IO (Maybe ExitCode)
pollExit process deadline = do
  exited <- getProcessExitCode process
  case exited of
    Just status -> pure (Just status)
    Nothing -> do
      now <- getMonotonicTimeNSec
      if now >= deadline
        then pure Nothing
        else threadDelay 10000 >> pollExit process deadline

capture :: Handle -> IO (MVar (Either IOException B.ByteString))
capture handle = do
  result <- MVar.newEmptyMVar
  _ <- forkIO $ do
    bytes <- try @IOException $ do
      value <- B.hGetContents handle
      evaluate (B.length value)
      pure value
    MVar.putMVar result bytes
  pure result

ignoreIOException :: IOException -> IO ()
ignoreIOException _ = pure ()

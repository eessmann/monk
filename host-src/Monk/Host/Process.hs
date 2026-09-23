-- | Owned external processes with byte streams and whole-group cancellation.
module Monk.Host.Process
  ( ProcessSpec (..),
    ProcessResult (..),
    runProcess,
    runCreateProcess,
  )
where

import Control.Concurrent (rtsSupportsBoundThreads, threadDelay)
import Control.Concurrent.Async (waitSTM, withAsync)
import Control.Exception (IOException, bracket, bracketOnError, catch, finally, mask, onException, throwIO)
import Data.ByteString qualified as B
import System.Exit (ExitCode)
import System.IO qualified as IO
import System.IO.Error (isDoesNotExistError, isResourceVanishedError)
import System.Posix.Signals (sigKILL, signalProcessGroup)
import System.Process qualified as P
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

-- The PID must be captured before any task can reap the leader. In particular,
-- typed-process-0.2.13.0 System.Process.Typed.startProcess starts its waiter
-- before returning, while getPid reads the already closable ProcessHandle.
-- That API cannot own groups whose fast leader exits before its descendants.
data OwnedProcess = OwnedProcess IO.Handle IO.Handle IO.Handle P.ProcessHandle P.Pid

runProcess :: ProcessSpec -> IO ProcessResult
runProcess spec =
  runCreateProcess
    (Just (timeoutMicros spec))
    (P.proc (executable spec) (arguments spec)) {P.cwd = workingDirectory spec, P.env = environment spec}
    (stdinBytes spec)

-- | Existing CreateProcess callers share the same byte/lifetime boundary.
-- Streams are owned here. The deadline covers exit and pipe EOF; Nothing means
-- no deadline. Exceptions, including cancellation, terminate the entire group.
runCreateProcess :: Maybe Int -> P.CreateProcess -> B.ByteString -> IO ProcessResult
runCreateProcess deadline command input =
  mask $ \restore ->
    bracketOnError (acquire command) terminateAndReap $ \owned@(OwnedProcess stdinHandle stdoutHandle stderrHandle process _) ->
      bracket (pure ()) (const (closeStreams owned)) $ \() ->
        withAsync (restore (B.hGetContents stdoutHandle)) $ \stdoutTask ->
          withAsync (restore (B.hGetContents stderrHandle)) $ \stderrTask ->
            withAsync (restore (writeInput stdinHandle)) $ \stdinTask ->
              withAsync (restore (waitExit process)) $ \exitTask -> do
                let observe = atomically $ do
                      status <- waitSTM exitTask
                      output <- waitSTM stdoutTask
                      errors <- waitSTM stderrTask
                      waitSTM stdinTask
                      pure (status, output, errors)
                    run = do
                      completed <- case deadline of
                        Nothing -> Just <$> observe
                        Just micros -> timeout (max 1 micros) observe
                      (timedOut, (status, output, errors)) <- case completed of
                        Just result -> pure (False, result)
                        Nothing -> do
                          terminateGroup owned
                          drained <- timeout 5000000 observe
                          result <- maybe (fail "subprocess pipes remained open after process-group termination") pure drained
                          pure (True, result)
                      pure (ProcessResult status output errors timedOut)
                restore run `onException` terminateGroup owned
  where
    writeInput handle = (B.hPut handle input `catch` ignoreBrokenPipe) `finally` (IO.hClose handle `catch` ignoreBrokenPipe)
    ignoreBrokenPipe :: IOException -> IO ()
    ignoreBrokenPipe failure
      | isResourceVanishedError failure = pure ()
      | otherwise = throwIO failure

-- The nonthreaded RTS cannot schedule pipe readers or timers during the process
-- package's blocking wait. Poll only in that runtime; threaded clients use the
-- ordinary event-driven waiter.
waitExit :: P.ProcessHandle -> IO ExitCode
waitExit process
  | rtsSupportsBoundThreads = P.waitForProcess process
  | otherwise = poll
  where
    poll = P.getProcessExitCode process >>= maybe (threadDelay 1000 >> poll) pure

acquire :: P.CreateProcess -> IO OwnedProcess
acquire command =
  bracketOnError
    (P.createProcess command {P.std_in = P.CreatePipe, P.std_out = P.CreatePipe, P.std_err = P.CreatePipe, P.create_group = True})
    cleanupCreated
    ( \(input, output, errors, process) -> do
        -- No wait/getProcessExitCode may run before this identity is captured.
        processGroup <- P.getPid process >>= maybe (fail "new subprocess has no process-group identity") pure
        case (input, output, errors) of
          (Just inputHandle, Just outputHandle, Just errorHandle) -> do
            mapM_ (`IO.hSetBuffering` IO.NoBuffering) [inputHandle, outputHandle, errorHandle]
            pure (OwnedProcess inputHandle outputHandle errorHandle process processGroup)
          _ -> fail "subprocess did not provide its owned pipes"
    )

cleanupCreated :: (Maybe IO.Handle, Maybe IO.Handle, Maybe IO.Handle, P.ProcessHandle) -> IO ()
cleanupCreated created@(_, _, _, process) = do
  processGroup <- P.getPid process
  forM_ processGroup signalGroup
  P.cleanupProcess created

terminateGroup :: OwnedProcess -> IO ()
terminateGroup (OwnedProcess _ _ _ _ processGroup) = signalGroup processGroup

signalGroup :: P.Pid -> IO ()
signalGroup processGroup =
  signalProcessGroup sigKILL processGroup `catch` \failure ->
    unless (isDoesNotExistError failure) (throwIO (failure :: IOException))

terminateAndReap :: OwnedProcess -> IO ()
terminateAndReap owned@(OwnedProcess _ _ _ process _) = do
  terminateGroup owned
  void (waitExit process)

closeStreams :: OwnedProcess -> IO ()
closeStreams (OwnedProcess input output errors _ _) =
  forM_ [input, output, errors] $ \handle -> IO.hClose handle `catch` ignoreClosed
  where
    ignoreClosed :: IOException -> IO ()
    ignoreClosed _ = pure ()

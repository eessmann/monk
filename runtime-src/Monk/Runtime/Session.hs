{-# LANGUAGE OverloadedStrings #-}

-- | A native owner for generated Fish control flow. Requests carry closed
-- operations and literal argv or compiled Fish bodies, never Bash syntax.
module Monk.Runtime.Session (dispatchSession, dispatchSessionClient, dispatchSessionWrite, dispatchSessionChild, dispatchSessionDirectoryDiagnostic, dispatchSessionExecError) where

import Control.Concurrent (forkIO, killThread, myThreadId, threadDelay, throwTo)
import Control.Exception (Exception, IOException, bracket, catch, finally, mask_, onException)
import Control.Exception qualified as Exception
import Control.Monad (filterM, forM, forM_, unless, void, when)
import Data.Bits (testBit)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as M
import Data.Maybe (isNothing)
import Foreign.C.Error (Errno (..), eNOENT)
import GHC.Foreign qualified as Foreign
import GHC.IO.Encoding (getFileSystemEncoding)
import GHC.IO.Exception (ioe_errno)
import Monk.Runtime.DescriptorTable qualified as DT
import Monk.Runtime.Descriptors (duplicatePrivate, initialDescriptorOpen, nativeErrorMessage)
import Monk.Runtime.Directory (formatDirectoryDiagnostic)
import Monk.Runtime.Exec (executionFailure)
import Monk.Runtime.Fields (echoBytes)
import Monk.Runtime.Printf (printfBytes)
import Monk.Runtime.Protocol (decodeFrames, encodeFrames, protocolFailure)
import Monk.Runtime.Read qualified as Read
import Monk.Runtime.Session.Capsule (withCapsule)
import Monk.Runtime.Session.Transport
import Monk.Runtime.Spawn (initialSignalIgnored, openWorkingDirectory, spawnProcess, spawnProcessAtMode, spawnStatus)
import System.Directory (removeDirectory, removeDirectoryRecursive, removeFile)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..), exitWith)
import System.IO (IOMode (ReadMode, WriteMode), hClose, hFlush, openBinaryTempFile, stderr, stdin, stdout, withBinaryFile)
import System.Posix.Env.ByteString (getEnv, getEnvironment)
import System.Posix.IO (OpenMode (ReadOnly), createPipe, defaultFileFlags, fdToHandle, openFd)
import System.Posix.Process (ProcessStatus (..), exitImmediately, getProcessID, getProcessStatus)
import System.Posix.Signals (Handler (Catch, Default), Signal, fullSignalSet, installHandler, sigHUP, sigINT, sigKILL, sigPIPE, sigQUIT, sigTERM, signalProcess, unblockSignals)
import System.Posix.Temp (mkdtemp)
import System.Posix.Types (Fd (..), ProcessID)
import Text.Read (readMaybe)

data SessionEvent = EvaluatorFinished Int | OwnerSignal Signal
  deriving stock (Show)

instance Exception SessionEvent

data Job = Job
  { jobMembers :: [ProcessID],
    jobPipefail :: Bool,
    jobCompleted :: M.Map ProcessID Int,
    jobSubstitution :: Bool,
    jobImplicitWait :: Bool
  }

type Jobs = IORef (M.Map ProcessID Job)

type Endpoints = IORef (M.Map Int Fd)

socketVariable, tokenVariable, replyVariable, descriptorsVariable :: B.ByteString
socketVariable = "MONK_SESSION_SOCKET"
tokenVariable = "MONK_SESSION_TOKEN"
replyVariable = "MONK_SESSION_REPLY"
descriptorsVariable = "MONK_SESSION_FDS"

-- | SCRIPT is an already generated evaluator. The private owned form is used
-- only by compiled body regions; it transfers the script workspace to its new
-- native owner before the outer owner acknowledges a background spawn.
dispatchSession :: [String] -> IO ()
dispatchSession arguments = do
  (code, pending) <- case arguments of
    "--capsule" : workspace : token : rest -> withCapsule workspace (C.pack token) (`run` rest)
    "--owned" : directory : script : rest -> run script rest `finally` removeDirectoryRecursive directory
    script : rest -> run script rest
    _ -> protocolFailure "session-run needs a generated Fish script" >> pure (125, Nothing)
  case pending of
    Nothing -> exitWith (exitCode code)
    Just signal -> do
      void (installHandler signal Default Nothing)
      unblockSignals fullSignalSet
      getProcessID >>= signalProcess signal
      exitImmediately (exitCode (128 + fromIntegral signal))
  where
    run script rest = do
      scriptBytes <- nativeBytes script
      argumentBytes <- mapM nativeBytes rest
      runSession scriptBytes argumentBytes

runSession :: B.ByteString -> [B.ByteString] -> IO (Int, Maybe Signal)
runSession script arguments = bracket acquireDiagnostic (mapM_ closeSession) $ \diagnostic -> bracket adoptTable DT.closeDescriptorTable (\table -> runOwnedSession diagnostic table script arguments)

-- Bash retains its original diagnostic stream even after source fd2 closes.
-- This private reference is never installed as a user-visible descriptor.
acquireDiagnostic :: IO (Maybe Fd)
acquireDiagnostic = do
  available <- initialDescriptorOpen (2 :: Int)
  if available then Just <$> duplicatePrivate (Fd 2) else pure Nothing

diagnosticStreams :: Maybe Fd -> M.Map Int Fd -> M.Map Int Fd
diagnosticStreams original streams = if M.member 2 streams then streams else maybe streams (\fd -> M.insert 2 fd streams) original

adoptTable :: IO DT.DescriptorTable
adoptTable = do
  inherited <- getEnv descriptorsVariable
  numbers <- case inherited of
    Nothing -> pure []
    Just bytes -> mapM integer (filter (not . B.null) (C.split ',' bytes))
  unless (all (\number -> number >= 3 && number <= 1048575) numbers) (ioError (userError "invalid inherited descriptor table"))
  table <- DT.newDescriptorTable [(number, Fd (fromIntegral number)) | number <- numbers]
  mapM_ (closeSession . Fd . fromIntegral) numbers
  pure table

runOwnedSession :: Maybe Fd -> DT.DescriptorTable -> B.ByteString -> [B.ByteString] -> IO (Int, Maybe Signal)
runOwnedSession diagnostic table script arguments = do
  owner <- myThreadId
  pid <- getProcessID
  jobs <- newIORef M.empty
  pending <- newIORef Nothing
  endpoints <- newIORef M.empty
  environment <- getEnvironment
  bracket (mkdtemp "/tmp/monk-session-XXXXXX") removeDirectoryRecursive $ \workspace -> do
    socketPath <- nativeBytes (workspace <> "/control")
    -- Unpredictable kernel-backed bytes protect the otherwise private endpoint
    -- from accidental reuse by an unrelated same-user invocation.
    token <- tokenBytes
    responsePath <- writeScript workspace B.empty
    evaluatorScript <- prepareEvaluatorScript workspace script
    bracket (listenSession socketPath) closeSession $ \listener -> do
      let evaluatorEnvironment = (socketVariable, socketPath) : (tokenVariable, token) : (replyVariable, responsePath) : cleanEnvironment environment
      Exception.mask $ \restoreOwner -> do
        evaluator <- launchEvaluator evaluatorScript arguments evaluatorEnvironment
        evaluatorStatus <- newIORef Nothing
        watcher <- forkIO $ do
          let poll = do
                reapJobs jobs
                status <- mask_ $ do
                  actual <- pollStatus evaluator
                  forM_ actual (writeIORef evaluatorStatus . Just)
                  pure actual
                case status of
                  Just code -> throwTo owner (EvaluatorFinished code)
                  Nothing -> threadDelay 1000 >> poll
          poll
        let cleanup = do
              killThread watcher
              releaseEndpoints endpoints
              completed <- readIORef evaluatorStatus
              when (isNothing completed) (terminate evaluator)
            install signal = do previous <- installHandler signal (Catch (throwTo owner (OwnerSignal signal))) Nothing; pure (signal, previous)
            restore handlers = forM_ handlers $ \(signal, previous) -> void (installHandler signal previous Nothing)
            loop = do
              connection <- acceptSession listener
              serveConnection diagnostic table pending endpoints token pid jobs connection
              loop
            finished (EvaluatorFinished code) = (code,) <$> readIORef pending
            finished (OwnerSignal signal) = pure (128 + fromIntegral signal, Just signal)
        signals <- filterM (fmap not . initialSignalIgnored) [sigHUP, sigINT, sigQUIT, sigTERM, sigPIPE]
        (restoreOwner (bracket (forM signals install) restore (const loop)) `catch` finished) `finally` cleanup

serveConnection :: Maybe Fd -> DT.DescriptorTable -> IORef (Maybe Signal) -> Endpoints -> B.ByteString -> ProcessID -> Jobs -> Fd -> IO ()
serveConnection diagnostic table pending endpoints token pid jobs connection = do
  request <- receiveSession connection `onException` closeSession connection
  bracket (pure request) (\(handle, descriptors, _) -> hClose handle >> mapM_ closeSession descriptors) $ \(handle, descriptors, bytes) -> do
    response <- processRequest diagnostic table pending endpoints token pid jobs descriptors bytes `catch` (\(_ :: IOException) -> pure (reply 125 Nothing))
    replySession handle response `catch` ignore

processRequest :: Maybe Fd -> DT.DescriptorTable -> IORef (Maybe Signal) -> Endpoints -> B.ByteString -> ProcessID -> Jobs -> [Fd] -> B.ByteString -> IO B.ByteString
processRequest diagnostic table pending endpoints token owner jobs descriptors bytes = do
  frames <- either (ioError . userError . C.unpack) pure (decodeFrames bytes)
  case frames of
    authentication : operation : maskBytes : rest | authentication == token -> do
      mask <- integer maskBytes
      let selected = [fd | fd <- [0 .. 2], testBit mask fd]
          requiresCwd = operation `elem` ["run", "spawn", "capture", "substitution", "fd-open"]
          (userDescriptors, remaining) = splitAt (length selected) descriptors
          directory = case remaining of [fd] -> fd; _ -> Fd (-1)
      unless (mask >= 0 && mask <= 7 && length descriptors == length selected + (if requiresCwd then 1 else 0)) (ioError (userError "invalid session descriptor mask"))
      let inherited = M.fromList (zip selected userDescriptors)
      streams <- DT.mergedDescriptors table inherited
      let diagnostics = diagnosticStreams diagnostic streams
      case (operation, rest) of
        ("substitution-release", []) -> releaseEndpoints endpoints >> pure (reply 0 Nothing)
        ("fd-endpoint", [origin, line, targetBytes, leaseBytes]) -> do
          target <- integer targetBytes
          lease <- integer leaseBytes
          pendingEndpoints <- readIORef endpoints
          case M.lookup lease pendingEndpoints of
            Nothing -> pure (reply 125 Nothing)
            Just endpoint ->
              ( do
                  DT.duplicateDescriptor table (M.singleton lease endpoint) target lease
                  atomicModifyIORef' endpoints (\current -> (M.delete lease current, ()))
                  closeSession endpoint
                  pure (reply 0 Nothing)
              )
                `catch` descriptorFailure diagnostics origin line leaseBytes
        ("fd-reset", []) -> DT.resetDescriptors table >> pure (reply 0 Nothing)
        ("fd-push", []) -> DT.pushDescriptors table >> pure (reply 0 Nothing)
        ("fd-pop", []) -> DT.popDescriptors table 1 >> pure (reply 0 Nothing)
        ("fd-pop", [count]) -> integer count >>= DT.popDescriptors table >> pure (reply 0 Nothing)
        ("fd-close", [number]) -> integer number >>= DT.closeDescriptor table >> pure (reply 0 Nothing)
        ("fd-data", [number, value]) -> do target <- integer number; DT.dataDescriptor table target value; pure (reply 0 Nothing)
        ("fd-open", [_, origin, line, number, mode, path]) -> do
          target <- integer number
          (DT.openDescriptorAt table target mode path directory >> pure (reply 0 Nothing)) `catch` descriptorFailure diagnostics origin line path
        ("fd-dup", [origin, line, target, source]) -> do
          destination <- integer target
          original <- integer source
          (DT.duplicateDescriptor table inherited destination original >> pure (reply 0 Nothing)) `catch` descriptorFailure diagnostics origin line source
        ("read", [origin, line, number, raw, delimiter, countBytes, ifs, mode, names]) -> do
          source <- integer number
          count <- integer countBytes
          variables <- integer names
          destination <- case mode of
            "reply" -> pure Read.ReadReply
            "scalar" | variables > 0 -> pure (Read.ReadScalars variables)
            "array" -> pure Read.ReadArray
            _ -> ioError (userError "invalid read destination")
          unless (raw `elem` ["0", "1"] && count >= -1) (ioError (userError "invalid read flags"))
          let configuration = Read.ReadConfig (raw == "1") (if B.null delimiter then 0 else B.head delimiter) (if count < 0 then Nothing else Just count) ifs destination
          case M.lookup source streams of
            Nothing -> do
              writeStream diagnostics 2 (origin <> ": line " <> line <> ": read: " <> number <> ": invalid file descriptor: Bad file descriptor\n")
              pure (encodeFrames ["ok", "1", "0"])
            Just descriptor ->
              ( do
                  (code, values) <- Read.readDescriptor configuration descriptor
                  pure (encodeFrames (["ok", C.pack (show code), "1"] <> values))
              )
                `catch` ( \failure -> do
                            message <- nativeErrorMessage failure
                            writeStream diagnostics 2 (origin <> ": line " <> line <> ": read: " <> number <> ": read error: " <> message <> "\n")
                            pure (encodeFrames ["ok", "1", "0"])
                        )
        ("finish-signal", ["13"]) -> writeIORef pending (Just sigPIPE) >> pure (reply 0 Nothing)
        ("ping", []) -> pure (reply 0 (Just owner))
        ("wait", origin : line : pids) -> do
          code <- if null pids then waitAll jobs else waitArguments jobs (\message -> writeStream diagnostics 2 (origin <> ": line " <> line <> ": wait: " <> message)) pids
          pure (reply code Nothing)
        (mode, _ : countBytes : payload) | mode `elem` ["run", "spawn", "capture", "substitution"] -> do
          count <- integer countBytes
          unless (count >= 0 && count <= length payload `div` 2) (ioError (userError "invalid session environment count"))
          let (pairs, body) = splitAt (count * 2) payload
          environment <- environmentPairs pairs
          case body of
            direction : kind : operands | mode == "substitution" && direction `elem` ["input", "output"] -> do
              (reader, writer) <- ownedPipe
              let (endpoint, producer, target) = if direction == "input" then (reader, writer, 1) else (writer, reader, 0)
              ( do
                  (child, job) <- startJob True diagnostic (M.insert target producer streams) directory environment kind operands
                  closeSession producer
                  known <- readIORef endpoints
                  let number = maximum (255 : M.keys known) + 1
                  atomicModifyIORef' endpoints (\current -> (M.insert number endpoint current, ()))
                  atomicModifyIORef' jobs (\current -> (M.insert child (job {jobSubstitution = True}) (supersedeSubstitutions current), ()))
                  pure (encodeFrames ["ok", "0", C.pack (show child), "/dev/fd/" <> C.pack (show number), C.pack (show number)])
                )
                `onException` (closeSession reader >> closeSession writer)
            warning : "snapshot" : operands | mode == "capture" -> captureJob diagnostic streams jobs directory environment warning operands
            kind : operands | kind `elem` ["external", "external-site", "body", "snapshot", "builtin", "directory-output", "pipeline"] -> do
              extra <- readIORef endpoints
              let operationStreams = M.union extra streams
              (child, job) <-
                ( if mode == "spawn"
                    then withBackgroundInput operationStreams (\background -> startJob True diagnostic background directory environment kind operands)
                    else startJob False diagnostic operationStreams directory environment kind operands
                )
                  `finally` releaseEndpoints endpoints
              atomicModifyIORef' jobs (\known -> (M.insert child job (if mode == "spawn" then supersedeSubstitutions known else known), ()))
              if mode == "spawn"
                then pure (reply 0 (Just child))
                else do
                  code <- waitJob jobs child `onException` terminateJob jobs child
                  atomicModifyIORef' jobs (\known -> (M.delete child known, ()))
                  pure (reply code (Just child))
            _ -> ioError (userError "unknown compiled session body kind")
        _ -> ioError (userError "unknown session operation")
    _ -> ioError (userError "unauthenticated session request")

-- Noninteractive asynchronous lists receive /dev/null on their inherited
-- stdin. Explicit redirects inside a compiled region can replace that input.
withBackgroundInput :: M.Map Int Fd -> (M.Map Int Fd -> IO a) -> IO a
withBackgroundInput streams action =
  bracket (bracket (openFd "/dev/null" ReadOnly defaultFileFlags) closeSession duplicatePrivate) closeSession $ \input -> action (M.insert 0 input streams)

-- The last actual stage PID identifies a pipeline job; every member remains
-- owned/reapable, and status aggregation is independent of completion order.
startJob :: Bool -> Maybe Fd -> M.Map Int Fd -> Fd -> [(B.ByteString, B.ByteString)] -> B.ByteString -> [B.ByteString] -> IO (ProcessID, Job)
startJob asynchronous diagnostic streams cwd environment kind operands
  | kind /= "pipeline" = do
      pid <- startUser asynchronous (if asynchronous then M.lookup 2 streams else diagnostic) streams cwd environment kind operands
      pure (pid, Job [pid] False M.empty False True)
  | otherwise = case operands of
      pipefail : countBytes : frames -> do
        unless (pipefail `elem` ["0", "1"]) (ioError (userError "invalid pipeline status policy"))
        count <- integer countBytes
        unless (count > 0) (ioError (userError "empty pipeline"))
        stages <- decodeStages count frames
        withPipes (count - 1) $ \pipes -> do
          started <- newIORef []
          let cleanup = readIORef started >>= mapM_ terminate
              launch ((stageKind, arguments), input, output) = do
                let table = maybe id (M.insert 1) output (maybe id (M.insert 0) input streams)
                pid <- startUser asynchronous (M.lookup 2 table) table cwd environment stageKind arguments
                atomicModifyIORef' started (\pids -> (pids <> [pid], ()))
                pure pid
          pids <- mapM launch (zip3 stages (Nothing : map (Just . fst) pipes) (map (Just . snd) pipes <> [Nothing])) `onException` cleanup
          case reverse pids of
            pid : _ -> pure (pid, Job pids (pipefail == "1") M.empty False True)
            [] -> ioError (userError "empty pipeline")
      _ -> ioError (userError "invalid pipeline frames")

withPipes :: Int -> ([(Fd, Fd)] -> IO a) -> IO a
withPipes count action
  | count <= 0 = action []
  | otherwise = bracket ownedPipe (\(reader, writer) -> closeSession reader >> closeSession writer) $ \pipe -> withPipes (count - 1) (action . (pipe :))

ownedPipe :: IO (Fd, Fd)
ownedPipe = bracket createPipe (\(reader, writer) -> closeSession reader >> closeSession writer) $ \(reader, writer) -> do
  ownedRead <- duplicatePrivate reader
  ownedWrite <- duplicatePrivate writer `onException` closeSession ownedRead
  pure (ownedRead, ownedWrite)

releaseEndpoints :: Endpoints -> IO ()
releaseEndpoints endpoints = do
  owned <- atomicModifyIORef' endpoints (M.empty,)
  mapM_ closeSession (M.elems owned)

decodeStages :: Int -> [B.ByteString] -> IO [(B.ByteString, [B.ByteString])]
decodeStages 0 [] = pure []
decodeStages count (kind : countBytes : frames) | count > 0 = do
  unless (kind `elem` ["external", "external-site", "body", "snapshot", "builtin", "directory-output"]) (ioError (userError "invalid pipeline stage kind"))
  arguments <- integer countBytes
  unless (arguments >= 0 && arguments <= length frames) (ioError (userError "invalid pipeline stage arguments"))
  let (values, rest) = splitAt arguments frames
  ((kind, values) :) <$> decodeStages (count - 1) rest
decodeStages _ _ = ioError (userError "invalid pipeline stage frames")

startUser :: Bool -> Maybe Fd -> M.Map Int Fd -> Fd -> [(B.ByteString, B.ByteString)] -> B.ByteString -> [B.ByteString] -> IO ProcessID
startUser asynchronous diagnostic streams cwd environment kind operands = case (kind, operands) of
  ("external", command : arguments) -> spawnProcessAtMode asynchronous streams (Just cwd) (cleanEnvironment environment) command arguments `catch` (\(_ :: IOException) -> spawnStatus 127)
  ("external-site", origin : line : command : arguments) ->
    spawnProcessAtMode asynchronous streams (Just cwd) (cleanEnvironment environment) command arguments
      `catch` ( \failure -> do
                  let cause = maybe eNOENT Errno (ioe_errno failure)
                  (code, message) <- executionFailure (Just cwd) origin line command cause
                  workspace <- mkdtemp "/tmp/monk-exec-error-XXXXXX"
                  ( do
                      path <- writeScript workspace message
                      runtime <- getExecutablePath >>= nativeBytes
                      directory <- nativeBytes workspace
                      spawnProcessAtMode asynchronous streams (Just cwd) (cleanEnvironment environment) runtime ["--abi", "2", "session-exec-error", directory, path, C.pack (show code)]
                    )
                    `onException` removeDirectoryRecursive workspace
              )
  ("builtin", origin : line : name : arguments) -> do
    bytes <- case name of
      "echo" -> pure (echoBytes arguments)
      "printf" -> either (ioError . userError . C.unpack) pure (printfBytes arguments)
      _ -> ioError (userError "unsupported session builtin")
    stageWriter streams origin line name bytes
  ("directory-output", [origin, line, name, descriptor, bytes])
    | name `elem` ["pwd", "cd", "pushd", "popd"] && descriptor `elem` ["1", "2"] -> do
        let output = if descriptor == "1" then streams else maybe (M.delete 1 streams) (\fd -> M.insert 1 fd streams) (M.lookup 2 (diagnosticStreams diagnostic streams))
        stageWriter output origin line name bytes
  ("body", script : arguments) -> stageRegion script arguments Nothing
  ("snapshot", script : level : state) -> do
    workspace <- mkdtemp "/tmp/monk-region-XXXXXX"
    ( do
        statePath <- writeScript workspace (encodeFrames state)
        launchRegion workspace script [statePath] (Just level)
      )
      `onException` removeDirectoryRecursive workspace
  _ -> ioError (userError "empty session command")
  where
    stageWriter output origin line name bytes = do
      workspace <- mkdtemp "/tmp/monk-writer-XXXXXX"
      ( do
          path <- writeScript workspace bytes
          runtime <- getExecutablePath >>= nativeBytes
          directory <- nativeBytes workspace
          spawnProcessAtMode asynchronous (diagnosticStreams diagnostic output) (Just cwd) (cleanEnvironment environment) runtime ["--abi", "2", "session-write", directory, path, origin, line, name]
        )
        `onException` removeDirectoryRecursive workspace
    stageRegion script arguments level = do
      workspace <- mkdtemp "/tmp/monk-region-XXXXXX"
      launchRegion workspace script arguments level `onException` removeDirectoryRecursive workspace
    launchRegion workspace script arguments level = do
      -- This sibling workspace becomes the region's property before the
      -- parent acknowledges spawn; outer session cleanup cannot remove it.
      path <- writeScript workspace script
      runtime <- getExecutablePath >>= nativeBytes
      directory <- nativeBytes workspace
      let descriptorManifest = C.intercalate "," [C.pack (show number) | number <- M.keys streams, number > 2]
          inheritedEnvironment = (descriptorsVariable, descriptorManifest) : cleanEnvironment environment
          regionEnvironment = maybe inheritedEnvironment (\value -> ("SHLVL", value) : filter ((/= "SHLVL") . fst) inheritedEnvironment) level
      spawnProcessAtMode asynchronous streams (Just cwd) regionEnvironment runtime (["--abi", "2", "session-run", "--owned", directory, path] <> arguments) `onException` removeDirectoryRecursive workspace

-- Direct script execution preserves argv scope for the snapshot prelude.
-- Fish repairs absent stdio at startup, so close those streams once inside
-- the private script instead of sourcing it through an additional argv scope.
prepareEvaluatorScript :: FilePath -> B.ByteString -> IO B.ByteString
prepareEvaluatorScript workspace script = do
  closed <- filterM (fmap not . initialDescriptorOpen) [0 :: Int .. 2]
  if null closed
    then pure script
    else do
      encoding <- getFileSystemEncoding
      path <- B.useAsCString script (Foreign.peekCString encoding)
      body <- B.readFile path
      let closures = B.concat [" " <> C.pack (show fd) <> ">&-" | fd <- closed]
      writeScript workspace ("begin\n" <> body <> "\nend" <> closures <> "\n")

launchEvaluator :: B.ByteString -> [B.ByteString] -> [(B.ByteString, B.ByteString)] -> IO ProcessID
launchEvaluator script arguments environment = do
  closed <- filterM (fmap not . initialDescriptorOpen) [0 :: Int .. 2]
  let streams = M.fromList [(fd, Fd (fromIntegral fd)) | fd <- [0 .. 2], fd `notElem` closed]
  spawnProcess streams Nothing environment "fish" (["--no-config", script] <> arguments)

dispatchSessionClient :: Bool -> IO ()
dispatchSessionClient privateReply = action `catch` (\(_ :: IOException) -> publish (reply 125 Nothing))
  where
    action = do
      when privateReply (publish B.empty)
      socket <- getEnv socketVariable >>= maybe (ioError (userError "missing session socket")) pure
      token <- getEnv tokenVariable >>= maybe (ioError (userError "missing session token")) pure
      bytes <- B.hGetContents stdin
      frames <- either (ioError . userError . C.unpack) pure (decodeFrames bytes)
      mask <- case frames of _ : value : _ -> integer value; _ -> ioError (userError "invalid session request")
      unless (mask >= 0 && mask <= 7) (ioError (userError "invalid session descriptor mask"))
      let selected = [fd | fd <- [0 .. 2], testBit mask fd]
      forM_ selected $ \fd -> initialDescriptorOpen (fd + 3) >>= \open -> unless open (ioError (userError "missing user stream"))
      payload <- case frames of
        operation : maskFrame : operands | operation `elem` ["run", "spawn", "substitution"] -> do
          environment <- cleanEnvironment <$> getEnvironment
          pure (operation : maskFrame : B.empty : C.pack (show (length environment)) : concatMap (\(name, value) -> [name, value]) environment <> operands)
        "fd-open" : maskFrame : operands -> pure ("fd-open" : maskFrame : B.empty : operands)
        _ -> pure frames
      let descriptors = [Fd (fromIntegral (fd + 3)) | fd <- selected]
          request owned = requestSession socket (descriptors <> owned) (encodeFrames (token : payload))
      result <- case payload of
        operation : _ | operation `elem` ["run", "spawn", "substitution", "fd-open"] -> bracket openWorkingDirectory closeSession (\directory -> request [directory])
        _ -> request []
      publish result
    publish result
      | privateReply = do
          path <- getEnv replyVariable >>= maybe (ioError (userError "missing session reply path")) pure
          encoding <- getFileSystemEncoding
          decoded <- B.useAsCString path (Foreign.peekCString encoding)
          withBinaryFile decoded WriteMode (\handle -> B.hPut handle result >> hFlush handle)
      | otherwise = B.hPut stdout result >> hFlush stdout

supersedeSubstitutions :: M.Map ProcessID Job -> M.Map ProcessID Job
supersedeSubstitutions = M.map (\job -> if jobSubstitution job then job {jobImplicitWait = False} else job)

waitAll :: Jobs -> IO Int
waitAll jobs = do
  targets <- M.keys . M.filter jobImplicitWait <$> readIORef jobs
  mapM_ (waitJob jobs) targets
  atomicModifyIORef' jobs (\known -> (foldr M.delete known targets, ()))
  pure 0

waitArguments :: Jobs -> (B.ByteString -> IO ()) -> [B.ByteString] -> IO Int
waitArguments jobs diagnostic arguments = case arguments of
  first : _ | B.length first > 1 && B.head first == 45 -> do
    diagnostic (B.take 2 first <> ": invalid option\nwait: usage: wait [-fn] [-p var] [id ...]\n")
    pure 2
  _ -> loop arguments
  where
    loop [] = pure 0
    loop (value : rest) = do
      code <- case readMaybe (C.unpack value) :: Maybe Integer of
        Just number | not (B.null value) && B.all (\byte -> byte >= 48 && byte <= 57) value && number >= 0 && number <= toInteger (maxBound :: ProcessID) -> do
          let pid = fromIntegral number
          known <- M.member pid <$> readIORef jobs
          unless known (diagnostic ("pid " <> C.pack (show pid) <> " is not a child of this shell\n"))
          waitJob jobs pid
        _ -> diagnostic ("`" <> value <> "': not a pid or valid job spec\n") >> pure 1
      if null rest then pure code else loop rest

waitJob :: Jobs -> ProcessID -> IO Int
waitJob jobs pid = do
  reapJobs jobs
  known <- readIORef jobs
  case M.lookup pid known of
    Nothing -> pure 127
    Just job -> case traverse (`M.lookup` jobCompleted job) (jobMembers job) of
      Nothing -> threadDelay 1000 >> waitJob jobs pid
      Just statuses -> case reverse (if jobPipefail job then filter (/= 0) statuses else statuses) of
        code : _ -> pure code
        [] -> pure 0

reapJobs :: Jobs -> IO ()
reapJobs jobs = do
  known <- readIORef jobs
  forM_ (M.toList known) $ \(key, job) -> forM_ (jobMembers job) $ \pid -> unless (M.member pid (jobCompleted job)) $ mask_ $ do
    actual <- pollStatus pid
    forM_ actual $ \code -> atomicModifyIORef' jobs (\current -> (M.adjust (\existing -> existing {jobCompleted = M.insert pid code (jobCompleted existing)}) key current, ()))

terminateJob :: Jobs -> ProcessID -> IO ()
terminateJob jobs pid = do
  known <- readIORef jobs
  forM_ (M.lookup pid known) $ \job -> do
    forM_ (jobMembers job) $ \member -> unless (M.member member (jobCompleted job)) (signalProcess sigKILL member `catch` ignore)
    void (waitJob jobs pid)

pollStatus :: ProcessID -> IO (Maybe Int)
pollStatus pid = (fmap statusCode <$> getProcessStatus False False pid) `catch` (\(_ :: IOException) -> pure Nothing)

waitProcess :: ProcessID -> IO Int
waitProcess pid = do
  status <- fmap statusCode <$> getProcessStatus False False pid
  maybe (threadDelay 1000 >> waitProcess pid) pure status

statusCode :: ProcessStatus -> Int
statusCode (Exited code) = case code of ExitSuccess -> 0; ExitFailure value -> value
statusCode (Terminated signal _) = 128 + fromIntegral signal
statusCode (Stopped _) = 125

terminate :: ProcessID -> IO ()
terminate pid = signalProcess sigKILL pid `catch` ignore >> void (waitProcess pid)

reply :: Int -> Maybe ProcessID -> B.ByteString
reply code pid = encodeFrames ["ok", C.pack (show code), maybe B.empty (C.pack . show) pid]

integer :: B.ByteString -> IO Int
integer = maybe (ioError (userError "invalid session integer")) pure . readMaybe . C.unpack

environmentPairs :: [B.ByteString] -> IO [(B.ByteString, B.ByteString)]
environmentPairs [] = pure []
environmentPairs (name : value : rest) = do
  when (B.null name || B.elem 61 name) (ioError (userError "invalid environment name"))
  ((name, value) :) <$> environmentPairs rest
environmentPairs _ = ioError (userError "unpaired environment")

cleanEnvironment :: [(B.ByteString, B.ByteString)] -> [(B.ByteString, B.ByteString)]
cleanEnvironment = filter (\(name, _) -> name /= socketVariable && name /= tokenVariable && name /= replyVariable && name /= descriptorsVariable && name `notElem` ["MONK_LAUNCH_ORIGINAL", "MONK_LAUNCH_WRAPPER"])

nativeBytes :: FilePath -> IO B.ByteString
nativeBytes path = do
  encoding <- getFileSystemEncoding
  Foreign.withCString encoding path B.packCString

tokenBytes :: IO B.ByteString
tokenBytes = do
  bytes <- withBinaryFile "/dev/urandom" ReadMode (`B.hGet` 32)
  pure (C.pack (concatMap (\byte -> let value = fromIntegral byte :: Int in ["0123456789abcdef" !! (value `div` 16), "0123456789abcdef" !! (value `mod` 16)]) (B.unpack (B.take 32 bytes))))

writeScript :: FilePath -> B.ByteString -> IO B.ByteString
writeScript workspace script = do
  path <- bracket (openBinaryTempFile workspace "body") (hClose . snd) $ \(path, handle) -> B.hPut handle script >> hFlush handle >> pure path
  nativeBytes path

exitCode :: Int -> ExitCode
exitCode 0 = ExitSuccess
exitCode code = ExitFailure code

ignore :: IOException -> IO ()
ignore _ = pure ()

-- The byte writer execs so inherited pipe readers and protocol fds close before
-- output. Its input is unlinked before writing: a real SIGPIPE cannot leak a
-- transport file, and the wait owner observes the actual signal termination.
dispatchSessionWrite :: [String] -> IO ()
dispatchSessionWrite [directory, path, origin, line, name] = do
  outputOpen <- initialDescriptorOpen (1 :: Int)
  diagnosticOpen <- initialDescriptorOpen (2 :: Int)
  let diagnostic message = do
        when diagnosticOpen (B.hPut stderr (C.pack (origin <> ": line " <> line <> ": " <> name <> ": write error: ") <> message <> "\n") >> hFlush stderr)
        exitWith (ExitFailure 1)
      failed failure = nativeErrorMessage failure >>= diagnostic
  withBinaryFile
    path
    ReadMode
    ( \handle -> do
        removeFile path
        removeDirectory directory
        let copy = do
              chunk <- B.hGetSome handle 65536
              unless (B.null chunk) $ do
                unless outputOpen (diagnostic "Bad file descriptor")
                B.hPut stdout chunk
                copy
        copy
        when outputOpen (hFlush stdout)
    )
    `catch` failed
dispatchSessionWrite _ = protocolFailure "invalid native writer arguments"

writeStream :: M.Map Int Fd -> Int -> B.ByteString -> IO ()
writeStream streams number bytes = forM_ (M.lookup number streams) $ \original -> do
  owned <- duplicatePrivate original
  bracket (fdToHandle owned) hClose (\handle -> B.hPut handle bytes >> hFlush handle)

descriptorFailure :: M.Map Int Fd -> B.ByteString -> B.ByteString -> B.ByteString -> IOException -> IO B.ByteString
descriptorFailure streams origin line operand failure = do
  message <- nativeErrorMessage failure
  writeStream streams 2 (origin <> ": line " <> line <> ": " <> operand <> ": " <> message <> "\n")
  pure (reply 1 Nothing)

-- Nested command substitutions ask the existing owner to copy its virtual
-- descriptor table before replacing stdout. Their new region owns its own
-- evaluator/jobs; only open-file descriptions (and their offsets) are shared.
dispatchSessionChild :: Bool -> B.ByteString -> Int -> B.ByteString -> Integer -> B.ByteString -> IO (Int, B.ByteString)
dispatchSessionChild capture warning mask script level state = do
  socket <- getEnv socketVariable >>= maybe (ioError (userError "missing child session socket")) pure
  token <- getEnv tokenVariable >>= maybe (ioError (userError "missing child session token")) pure
  environment <- cleanEnvironment <$> getEnvironment
  values <- either (ioError . userError . C.unpack) pure (decodeFrames state)
  let operation = if capture then "capture" else "run"
      body = [warning | capture] <> ["snapshot", script, C.pack (show level)] <> values
      frames = [token, operation, C.pack (show mask), B.empty, C.pack (show (length environment))] <> concatMap (\(name, value) -> [name, value]) environment <> body
      descriptors = [Fd (fromIntegral (if fd == 0 then 3 else fd)) | fd <- [0 .. 2], testBit mask fd]
  response <- bracket openWorkingDirectory closeSession (\directory -> requestSession socket (descriptors <> [directory]) (encodeFrames frames))
  result <- either (ioError . userError . C.unpack) pure (decodeFrames response)
  case result of
    ["ok", status, value] -> do code <- integer status; pure (code, if capture then value else B.empty)
    _ -> ioError (userError "invalid child session reply")

captureJob :: Maybe Fd -> M.Map Int Fd -> Jobs -> Fd -> [(B.ByteString, B.ByteString)] -> B.ByteString -> [B.ByteString] -> IO B.ByteString
captureJob diagnostic streams jobs cwd environment warning operands = bracket captureHandles cleanup $ \(reader, writer, writerFd) -> do
  (pid, job) <- startJob False diagnostic (M.insert 1 writerFd streams) cwd environment "snapshot" operands
  atomicModifyIORef' jobs (\known -> (M.insert pid job known, ()))
  ( do
      hClose writer
      let drain warned chunks = do
            bytes <- B.hGetSome reader 65536
            if B.null bytes
              then pure (B.concat (reverse chunks))
              else do
                let nul = B.elem 0 bytes
                when (nul && not warned) (writeStream (diagnosticStreams diagnostic streams) 2 warning)
                drain (warned || nul) (B.filter (/= 0) bytes : chunks)
      output <- drain False []
      code <- waitJob jobs pid
      atomicModifyIORef' jobs (\known -> (M.delete pid known, ()))
      pure (encodeFrames ["ok", C.pack (show code), B.dropWhileEnd (== 10) output])
    )
    `onException` terminateJob jobs pid
  where
    cleanup (reader, writer, _) = hClose reader >> hClose writer
    captureHandles = bracket createPipe (\(reader, writer) -> closeSession reader >> closeSession writer) $ \(reader, writer) -> do
      ownedRead <- duplicatePrivate reader
      ownedWrite <- duplicatePrivate writer `onException` closeSession ownedRead
      input <- fdToHandle ownedRead `onException` (closeSession ownedRead >> closeSession ownedWrite)
      output <- fdToHandle ownedWrite `onException` (hClose input >> closeSession ownedWrite)
      pure (input, output, ownedWrite)

-- Keep this final cd-pipeline stage native, so Fish executes its state-changing
-- first builtin stage in the evaluator. Only the finite diagnostic crosses RPC.
dispatchSessionDirectoryDiagnostic :: IO ()
dispatchSessionDirectoryDiagnostic = do
  (origin, line, name, message) <- B.hGetContents stdin >>= either protocolFailure pure . formatDirectoryDiagnostic
  unless (B.null message) $ do
    socket <- getEnv socketVariable >>= maybe (ioError (userError "missing directory session socket")) pure
    token <- getEnv tokenVariable >>= maybe (ioError (userError "missing directory session token")) pure
    environment <- cleanEnvironment <$> getEnvironment
    errorOpen <- initialDescriptorOpen (2 :: Int)
    let mask = if errorOpen then "4" else "0"
        descriptors = [Fd 2 | errorOpen]
        frames = [token, "run", mask, B.empty, C.pack (show (length environment))] <> concatMap (\(key, value) -> [key, value]) environment <> ["directory-output", origin, line, name, "2", message]
    response <- bracket openWorkingDirectory closeSession (\directory -> requestSession socket (descriptors <> [directory]) (encodeFrames frames))
    values <- either protocolFailure pure (decodeFrames response)
    case values of
      ["ok", status, _] -> integer status >>= exitWith . exitCode
      _ -> protocolFailure "invalid directory session response"

-- Failed exec owns a real source child PID. Diagnostic transport is unlinked
-- before writing, and SIGPIPE retains actual kernel signal termination.
dispatchSessionExecError :: [String] -> IO ()
dispatchSessionExecError [directory, path, codeBytes] = do
  code <- integer (C.pack codeBytes)
  visible <- initialDescriptorOpen (2 :: Int)
  withBinaryFile path ReadMode $ \handle -> do
    removeFile path
    removeDirectory directory
    when visible ((B.hGetContents handle >>= B.hPut stderr >> hFlush stderr) `catch` ignore)
  exitWith (exitCode code)
dispatchSessionExecError _ = protocolFailure "invalid failed executable diagnostic arguments"

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Portable byte transport for already materialized Fish children. No source
-- language expressions are accepted or interpreted by this module.
module Monk.Runtime.Child (dispatchChild, dispatchChildInSession) where

import Control.Concurrent (myThreadId, throwTo)
import Control.Exception (IOException, bracket, bracketOnError, catch, onException)
import Control.Monad (filterM, forM, forM_, unless, void, when)
import Data.Bits (testBit)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.Map.Strict qualified as M
import GHC.Foreign qualified as Foreign
import GHC.IO.Encoding (getFileSystemEncoding)
import Monk.Runtime.Descriptors (duplicatePrivate, initialDescriptorOpen)
import Monk.Runtime.Session (dispatchSessionChild)
import Monk.Runtime.Spawn (initialSignalIgnored, spawnProcess)
import System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..), exitWith)
import System.IO (Handle, hClose, hFlush, openBinaryTempFile, stdin, stdout)
import System.Posix.Env.ByteString (getEnvironment)
import System.Posix.IO (closeFd, createPipe, fdToHandle)
import System.Posix.Process (ProcessStatus (..), getProcessStatus)
import System.Posix.Signals (Handler (Catch), installHandler, sigHUP, sigINT, sigKILL, sigPIPE, sigQUIT, sigTERM, signalProcess)
import System.Posix.Temp (mkdtemp)
import System.Posix.Types (Fd (..), ProcessID)
import Text.Read (readMaybe)

-- | The first four NUL-terminated frames are diagnostic bytes, descriptor mask, Fish script,
-- and SHLVL. The remaining bytes are the child's NUL-framed scalar/argv state.
dispatchChild :: Bool -> IO ()
dispatchChild = dispatchChildWith False

dispatchChildInSession :: Bool -> IO ()
dispatchChildInSession = dispatchChildWith True

dispatchChildWith :: Bool -> Bool -> IO ()
dispatchChildWith supervised capture = run `catch` failure
  where
    run = do
      (code, output) <- withOwnerSignals $ do
        bytes <- B.hGetContents stdin
        (warning, descriptorMask, script, level, state) <- either (ioError . userError) pure (decode bytes)
        originalInput <- initialDescriptorOpen 3
        when (testBit descriptorMask 0 && not originalInput) (ioError (userError "missing original stdin descriptor"))
        if supervised
          then dispatchSessionChild capture warning descriptorMask script level state
          else do
            environment <- getEnvironment
            let childEnvironment = ("SHLVL", C.pack (show level)) : filter ((/= "SHLVL") . fst) environment
            directory <- getTemporaryDirectory
            bracket (mkdtemp (directory <> "/monk-child-XXXXXX")) removeDirectoryRecursive $ \workspace -> do
              scriptPath <- transportFile workspace (restoreClosedStreams capture descriptorMask script)
              statePath <- transportFile workspace state
              if capture
                then bracket capturePipe (\(r, w, _) -> closeHandleQuiet r >> closeHandleQuiet w) $ \(reader, writer, writeFd) ->
                  bracketOnError (spawnChild supervised descriptorMask scriptPath statePath childEnvironment (Just writeFd)) terminateChild $ \pid -> do
                    hClose writer
                    output <- drainCapture (testBit descriptorMask 2) warning reader
                    code <- waitChild pid
                    pure (code, B.dropWhileEnd (== 10) output)
                else bracketOnError (spawnChild supervised descriptorMask scriptPath statePath childEnvironment Nothing) terminateChild $ \pid -> do
                  code <- waitChild pid
                  pure (code, B.empty)
      -- Publish only after owned transport resources have been released. A
      -- reader closing the packet pipe must not strand private files.
      if capture then packet "ok" code output else exitWith (if code == 0 then ExitSuccess else ExitFailure code)
    failure (_ :: IOException) =
      if capture
        then packet "error" 125 "child-transport-failure" `catch` (\(_ :: IOException) -> exitWith (ExitFailure 125))
        else writeDiagnostic "monk: child transport failed\n" >> exitWith (ExitFailure 125)

decode :: B.ByteString -> Either String (B.ByteString, Int, B.ByteString, Integer, B.ByteString)
decode input = do
  (warning, rest) <- frame input
  (maskBytes, payload) <- frame rest
  descriptorMask <- maybe (Left "invalid descriptor mask") Right (readMaybe (C.unpack maskBytes))
  unless (descriptorMask >= 0 && descriptorMask <= 7) (Left "invalid descriptor mask")
  (script, rest') <- frame payload
  (levelBytes, state) <- frame rest'
  level <- maybe (Left "invalid SHLVL") Right (readMaybe (C.unpack levelBytes))
  unless (B.null state || B.last state == 0) (Left "unterminated state")
  pure (warning, descriptorMask, script, level, state)
  where
    frame bytes = case B.break (== 0) bytes of
      (part, tailBytes) | not (B.null tailBytes) -> Right (part, B.tail tailBytes)
      _ -> Left "missing frame"

-- Files live inside a mode-0700 mkdtemp directory, and openBinaryTempFile
-- creates each mode-0600 file. The owning bracket removes the directory after
-- wait, including exceptions. Fish opens its own independent offset-zero file
-- descriptions; neither /proc nor /dev/fd reopening is part of the ABI.
transportFile :: FilePath -> B.ByteString -> IO B.ByteString
transportFile directory bytes = do
  path <- bracket (openBinaryTempFile directory "transport") (closeHandleQuiet . snd) $ \(path, handle) -> do
    B.hPut handle bytes
    hFlush handle
    pure path
  -- FilePath can contain surrogate escapes for non-UTF-8 native path bytes.
  encoding <- getFileSystemEncoding
  Foreign.withCString encoding path B.packCString

capturePipe :: IO (Handle, Handle, Fd)
capturePipe = bracket createPipe (\(r, w) -> closeQuiet r >> closeQuiet w) $ \(r, w) -> do
  r' <- duplicatePrivate r
  w' <- duplicatePrivate w `onException` closeFd r'
  reader <- fdToHandle r' `onException` (closeFd r' >> closeFd w')
  writer <- fdToHandle w' `onException` (hClose reader >> closeFd w')
  pure (reader, writer, w')

-- Prepare every descriptor and argv in the parent. The spawn implementation
-- performs only native file/signal actions before exec, never Haskell after fork.
spawnChild :: Bool -> Int -> B.ByteString -> B.ByteString -> [(B.ByteString, B.ByteString)] -> Maybe Fd -> IO ProcessID
spawnChild supervised descriptorMask scriptPath statePath environment output = do
  let input = [(0, Fd 3) | testBit descriptorMask 0]
      standardOutput = maybe [(1, Fd 1) | testBit descriptorMask 1] (\fd -> [(1, fd)]) output
      diagnostic = [(2, Fd 2) | testBit descriptorMask 2]
      descriptors = M.fromList (input <> standardOutput <> diagnostic)
  if supervised
    then do
      runtimePath <- getExecutablePath
      encoding <- getFileSystemEncoding
      runtime <- Foreign.withCString encoding runtimePath B.packCString
      spawnProcess descriptors Nothing environment runtime ["--abi", "2", "session-run", scriptPath, statePath]
    else spawnProcess descriptors Nothing environment "fish" ["--no-config", scriptPath, statePath]

-- The owner receives cleanup exceptions rather than abandoning transport
-- resources on termination. Its direct child is reaped before files disappear.
withOwnerSignals :: IO a -> IO a
withOwnerSignals action = do
  owner <- myThreadId
  let install signal = do
        previous <- installHandler signal (Catch (throwTo owner (ExitFailure (128 + fromIntegral signal)))) Nothing
        pure (signal, previous)
      restore handlers = forM_ handlers $ \(signal, previous) -> void (installHandler signal previous Nothing)
  signals <- filterM (fmap not . initialSignalIgnored) [sigHUP, sigINT, sigQUIT, sigTERM, sigPIPE]
  bracket (forM signals install) restore (const action)

terminateChild :: ProcessID -> IO ()
terminateChild pid = do
  signalProcess sigKILL pid `catch` ignore
  void (waitChild pid) `catch` ignore

-- Fish repairs absent standard streams at startup. Reapply the original
-- closures inside its script so builtin writes and external children observe
-- the caller's descriptor contract rather than silently writing to /dev/null.
restoreClosedStreams :: Bool -> Int -> B.ByteString -> B.ByteString
restoreClosedStreams capture descriptorMask script =
  "begin\n" <> script <> "\nend" <> B.concat [" " <> C.pack (show fd) <> ">&-" | fd <- [0 :: Int .. 2], not (testBit descriptorMask fd), not (capture && fd == 1)] <> "\n"

waitChild :: ProcessID -> IO Int
waitChild pid = do
  status <- getProcessStatus True False pid
  case status of
    Just (Exited ExitSuccess) -> pure 0
    Just (Exited (ExitFailure code)) -> pure code
    Just (Terminated signal _) -> pure (128 + fromIntegral signal)
    _ -> pure 125

-- Drain in bounded reads so the first NUL diagnostic is emitted while the
-- child is running. Delay only the final packet, whose trailing newlines cannot
-- be decided until EOF. Raw non-NUL bytes never cross a text decoder.
drainCapture :: Bool -> B.ByteString -> Handle -> IO B.ByteString
drainCapture stderrOpen warning handle = loop False []
  where
    loop warned chunks = do
      chunk <- B.hGetSome handle 65536
      if B.null chunk
        then pure (B.concat (reverse chunks))
        else do
          let nul = B.elem 0 chunk
          when (nul && not warned && stderrOpen) (writeDiagnostic warning)
          loop (warned || nul) (B.filter (/= 0) chunk : chunks)

packet :: B.ByteString -> Int -> B.ByteString -> IO ()
packet tag code bytes = B.hPut stdout (tag <> "\0" <> C.pack (show code) <> "\0" <> bytes <> "\0") >> hFlush stdout

writeDiagnostic :: B.ByteString -> IO ()
writeDiagnostic bytes = (do fd <- duplicatePrivate (Fd 2); bracket (fdToHandle fd) hClose (\handle -> B.hPut handle bytes >> hFlush handle)) `catch` ignore

closeHandleQuiet :: Handle -> IO ()
closeHandleQuiet handle = hClose handle `catch` ignore

closeQuiet :: Fd -> IO ()
closeQuiet fd = closeFd fd `catch` ignore

ignore :: IOException -> IO ()
ignore _ = pure ()

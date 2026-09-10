{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Linux byte transport for already materialized Fish children. No source
-- language expressions are accepted or interpreted by this module.
module Monk.Runtime.Child (dispatchChild) where

import Control.Exception (IOException, bracket, catch, finally, onException)
import Control.Monad (forM_, unless, void, when)
import Data.Bits (testBit)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Foreign.C.Error (throwErrnoIfMinus1)
import Foreign.C.Types (CInt (..))
import Monk.Runtime.Descriptors (initialDescriptorOpen)
import System.Directory (getTemporaryDirectory, listDirectory, removeFile)
import System.Exit (ExitCode (..), exitWith)
import System.IO (Handle, hClose, hFlush, openBinaryTempFile, stdin, stdout)
import System.Posix.Env.ByteString (getEnvironment)
import System.Posix.Files (deviceID, fileID, getFdStatus, getFileStatus)
import System.Posix.IO (FdOption (CloseOnExec), closeFd, createPipe, dupTo, fdToHandle, handleToFd, setFdOption)
import System.Posix.Process (ProcessStatus (..), exitImmediately, forkProcess, getProcessStatus)
import System.Posix.Process.ByteString (executeFile)
import System.Posix.Signals (Handler (Default), fullSignalSet, installHandler, sigCHLD, sigINT, sigPIPE, sigQUIT, sigTERM, sigTSTP, sigTTIN, sigTTOU, unblockSignals)
import System.Posix.Types (Fd (..), ProcessID)
import Text.Read (readMaybe)

foreign import ccall unsafe "fcntl" duplicateAbove :: CInt -> CInt -> CInt -> IO CInt

-- | The first four NUL-terminated frames are diagnostic bytes, descriptor mask, Fish script,
-- and SHLVL. The remaining bytes are the child's NUL-framed scalar/argv state.
dispatchChild :: Bool -> IO ()
dispatchChild capture = run `catch` failure
  where
    run = do
      bytes <- B.hGetContents stdin
      (warning, descriptorMask, script, level, state) <- either (ioError . userError) pure (decode bytes)
      originalInput <- initialDescriptorOpen 3
      when (testBit descriptorMask 0 && not originalInput) (ioError (userError "missing original stdin descriptor"))
      environment <- getEnvironment
      let childEnvironment = ("SHLVL", C.pack (show level)) : filter ((/= "SHLVL") . fst) environment
      bracket (anonymous (restoreClosedStreams capture descriptorMask script)) closeFd $ \scriptFd ->
        bracket (anonymous state) closeFd $ \stateFd -> do
          checkIdentity scriptFd
          checkIdentity stateFd
          descriptors <- privateDescriptors
          if capture
            then bracket capturePipe (\(r, w) -> closeQuiet r >> closeQuiet w) $ \(readFd, writeFd) -> do
              pid <- forkProcess (child descriptorMask scriptFd stateFd childEnvironment descriptors (Just (readFd, writeFd)))
              closeFd writeFd
              output <- bracket (fdToHandle readFd) hClose (drainCapture (testBit descriptorMask 2) warning)
              code <- waitChild pid
              packet "ok" code (B.dropWhileEnd (== 10) output)
            else do
              pid <- forkProcess (child descriptorMask scriptFd stateFd childEnvironment descriptors Nothing)
              code <- waitChild pid
              exitWith (if code == 0 then ExitSuccess else ExitFailure code)
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

-- Private files are unlinked before use, and moved above the standard streams
-- and fd3 even when the caller deliberately closed a standard descriptor.
anonymous :: B.ByteString -> IO Fd
anonymous bytes = do
  directory <- getTemporaryDirectory
  bracket (openBinaryTempFile directory "monk-child") (\(path, handle) -> closeHandleQuiet handle >> (removeFile path `catch` ignore)) $ \(path, handle) -> do
    removeFile path
    B.hPut handle bytes
    hFlush handle
    fd <- handleToFd handle
    -- Reopening through /dev/fd gives the child an independent offset at zero.
    promote fd `finally` closeFd fd

promote :: Fd -> IO Fd
promote (Fd fd) = do
  result <- Fd <$> throwErrnoIfMinus1 "fcntl(F_DUPFD)" (duplicateAbove fd 0 10)
  setFdOption result CloseOnExec True
  pure result

capturePipe :: IO (Fd, Fd)
capturePipe = bracket createPipe (\(r, w) -> closeQuiet r >> closeQuiet w) $ \(r, w) -> do
  r' <- promote r
  w' <- promote w `onException` closeFd r'
  pure (r', w')

checkIdentity :: Fd -> IO ()
checkIdentity fd = do
  actual <- getFileStatus (descriptorPath fd)
  expected <- getFdStatus fd
  unless (deviceID actual == deviceID expected && fileID actual == fileID expected) (ioError (userError "descriptor filesystem identity mismatch"))

descriptorPath :: Fd -> FilePath
descriptorPath (Fd fd) = "/dev/fd/" <> show fd

privateDescriptors :: IO [Fd]
privateDescriptors = do
  names <- listDirectory "/proc/self/fd"
  pure [Fd fd | name <- names, Just fd <- [readMaybe name], fd > 2]

child :: Int -> Fd -> Fd -> [(B.ByteString, B.ByteString)] -> [Fd] -> Maybe (Fd, Fd) -> IO ()
child descriptorMask scriptFd stateFd environment descriptors output = launch `catch` (\(_ :: IOException) -> exitImmediately (ExitFailure 125))
  where
    launch = do
      -- GHC ignores SIGPIPE and owns several other dispositions. A shell child
      -- must receive the ordinary exec signal contract, including SIGPIPE.
      forM_ [sigPIPE, sigINT, sigQUIT, sigTERM, sigCHLD, sigTSTP, sigTTIN, sigTTOU] $ \signal -> void (installHandler signal Default Nothing)
      unblockSignals fullSignalSet
      if testBit descriptorMask 0 then void (dupTo (Fd 3) (Fd 0)) else closeQuiet (Fd 0)
      case output of
        Nothing -> pure ()
        Just (r, w) -> closeFd r >> void (dupTo w (Fd 1)) >> closeFd w
      -- Preserve only the two anonymous transport descriptors across exec;
      -- fd3 and inherited provider/private descriptors are not child-owned.
      forM_ descriptors $ \fd -> when (fd /= scriptFd && fd /= stateFd) (setFdOption fd CloseOnExec True `catch` ignore)
      forM_ [scriptFd, stateFd] $ \fd -> setFdOption fd CloseOnExec False
      executeFile "fish" True ["--no-config", C.pack (descriptorPath scriptFd), C.pack (descriptorPath stateFd)] (Just environment)

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
writeDiagnostic bytes = (do fd <- promote (Fd 2); bracket (fdToHandle fd) hClose (\handle -> B.hPut handle bytes >> hFlush handle)) `catch` ignore

closeHandleQuiet :: Handle -> IO ()
closeHandleQuiet handle = hClose handle `catch` ignore

closeQuiet :: Fd -> IO ()
closeQuiet fd = closeFd fd `catch` ignore

ignore :: IOException -> IO ()
ignore _ = pure ()

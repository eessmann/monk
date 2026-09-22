{-# LANGUAGE OverloadedStrings #-}

-- | Byte-sized-independent bootstrap for an exec launcher. A guardian owns the
-- capsule until the native owner closes a transferred lease descriptor, even
-- when the launcher or owner is killed before normal cleanup.
module Monk.Runtime.Session.Capsule (prepareCapsule, withCapsule, dispatchCapsuleGuardian) where

import Control.Exception (IOException, bracket, catch, finally, onException)
import Control.Monad (unless, void)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.Map.Strict qualified as M
import GHC.Foreign qualified as Foreign
import GHC.IO.Encoding (getFileSystemEncoding)
import Monk.Runtime.Descriptors (duplicatePrivate)
import Monk.Runtime.Protocol (decodeFrames, encodeFrames, protocolFailure)
import Monk.Runtime.Session.Transport
import Monk.Runtime.Spawn (spawnProcess)
import System.Directory (removeDirectoryRecursive)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..))
import System.IO (Handle, IOMode (ReadMode), hClose, hFlush, openBinaryTempFile, stdin, stdout, withBinaryFile)
import System.Posix.Env.ByteString (getEnvironment)
import System.Posix.IO (FdOption (CloseOnExec), createPipe, fdToHandle, setFdOption)
import System.Posix.Process (createSession, exitImmediately)
import System.Posix.Temp (mkdtemp)
import System.Posix.Types (Fd (..))
import System.Timeout (timeout)
import Text.Read (readMaybe)

prepareCapsule :: IO ()
prepareCapsule = do
  frames <- B.hGetContents stdin >>= either protocolFailure pure . decodeFrames
  script <- case frames of [value] -> pure value; _ -> protocolFailure "session-prepare expects one compiled Fish script"
  workspace <- mkdtemp "/tmp/monk-capsule-XXXXXX"
  ( do
      scriptPath <- bracket (openBinaryTempFile workspace "script") (hClose . snd) $ \(path, handle) -> B.hPut handle script >> hFlush handle >> pure path
      token <- randomToken
      path <- nativeBytes (workspace <> "/lease")
      bracket (listenSession path) closeSession $ \listener -> do
        runtime <- getExecutablePath >>= nativeBytes
        scriptBytes <- nativeBytes scriptPath
        directoryBytes <- nativeBytes workspace
        environment <- getEnvironment
        let Fd listenerFd = listener
        void (spawnProcess (M.singleton (fromIntegral listenerFd) listener) Nothing environment runtime ["--abi", "2", "session-guardian", directoryBytes, scriptBytes, token, C.pack (show listenerFd)])
      directory <- nativeBytes workspace
      B.hPut stdout (encodeFrames ["ok", "0", directory, token])
      hFlush stdout
    )
    `onException` removeDirectoryRecursive workspace

-- The guardian exec boundary closes all inherited user/RTS descriptors. Only
-- its explicitly owned listening socket survives until a lease is transferred.
dispatchCapsuleGuardian :: [String] -> IO ()
dispatchCapsuleGuardian [workspace, script, token, descriptor] = case readMaybe descriptor of
  Just value | value >= 3 -> do
    -- Detach only after exec; the guardian never runs Haskell in a fork child.
    void createSession
    let listener = Fd value
    setFdOption listener CloseOnExec True
    guardCapsule workspace script (C.pack token) listener
  _ -> protocolFailure "invalid capsule guardian descriptor"
dispatchCapsuleGuardian _ = protocolFailure "invalid capsule guardian arguments"

withCapsule :: FilePath -> B.ByteString -> (FilePath -> IO a) -> IO a
withCapsule workspace token action = do
  path <- nativeBytes (workspace <> "/lease")
  (lease, response) <- requestSessionLease path (encodeFrames [token])
  bracket (pure lease) closeSession $ \_ -> do
    frames <- either (ioError . userError . C.unpack) pure (decodeFrames response)
    case frames of
      ["ok", script] -> do
        encoding <- getFileSystemEncoding
        scriptPath <- B.useAsCString script (Foreign.peekCString encoding)
        action scriptPath
      _ -> ioError (userError "capsule takeover rejected")

guardCapsule :: FilePath -> FilePath -> B.ByteString -> Fd -> IO ()
guardCapsule workspace script token listener = do
  let owned = bracket leasePipe closeLease $ \(reader, writer, writerFd) -> do
        transferred <- timeout 60000000 (takeover script token listener writerFd)
        hClose writer
        case transferred of
          Just True -> void (B.hGet reader 1)
          _ -> pure ()
  (owned `catch` (\(_ :: IOException) -> pure ())) `finally` (closeSession listener >> removeDirectoryRecursive workspace)
  exitImmediately ExitSuccess
  where
    closeLease (reader, writer, _) = hClose reader >> hClose writer

-- Authentication may reject an unrelated connection without consuming the
-- capsule; only an authenticated caller receives its lease and script path.
takeover :: FilePath -> B.ByteString -> Fd -> Fd -> IO Bool
takeover script token listener writerFd = do
  connection <- acceptSession listener
  request <- receiveSession connection `onException` closeSession connection
  valid <- bracket (pure request) (\(handle, fds, _) -> hClose handle >> mapM_ closeSession fds) $ \(handle, fds, bytes) ->
    if null fds && decodeFrames bytes == Right [token]
      then do
        scriptBytes <- nativeBytes script
        replySessionLease connection handle writerFd (encodeFrames ["ok", scriptBytes])
        pure True
      else pure False
  if valid then pure True else takeover script token listener writerFd

leasePipe :: IO (Handle, Handle, Fd)
leasePipe = bracket createPipe (\(r, w) -> closeSession r >> closeSession w) $ \(r, w) -> do
  ownedRead <- duplicatePrivate r
  ownedWrite <- duplicatePrivate w `onException` closeSession ownedRead
  reader <- fdToHandle ownedRead `onException` (closeSession ownedRead >> closeSession ownedWrite)
  writer <- fdToHandle ownedWrite `onException` (hClose reader >> closeSession ownedWrite)
  pure (reader, writer, ownedWrite)

nativeBytes :: FilePath -> IO B.ByteString
nativeBytes path = getFileSystemEncoding >>= \encoding -> Foreign.withCString encoding path B.packCString

randomToken :: IO B.ByteString
randomToken = do
  bytes <- withBinaryFile "/dev/urandom" ReadMode (`B.hGet` 32)
  unless (B.length bytes == 32) (ioError (userError "missing capsule entropy"))
  pure (C.pack (concatMap (\byte -> let value = fromIntegral byte :: Int in ["0123456789abcdef" !! (value `div` 16), "0123456789abcdef" !! (value `mod` 16)]) (B.unpack bytes)))

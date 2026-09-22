{-# LANGUAGE OverloadedStrings #-}

-- | Native entry for compiled Fish artifacts. Capture the original standard
-- descriptors before Fish repairs them, without evaluating Bash source.
module Monk.Runtime.Launch (dispatchLaunch) where

import Control.Concurrent (myThreadId, threadDelay, throwTo)
import Control.Exception (Exception, IOException, bracket, catch, mask, onException)
import Control.Monad (filterM, forM, forM_, unless, void, when)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.Map.Strict qualified as M
import GHC.Foreign qualified as Foreign
import GHC.IO.Encoding (getFileSystemEncoding)
import Monk.Runtime.Descriptors (initialDescriptorOpen)
import Monk.Runtime.Spawn (initialSignalIgnored, spawnProcess)
import System.Directory (getTemporaryDirectory, makeAbsolute, removeDirectoryRecursive)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hClose, hFlush, openBinaryTempFile, stderr)
import System.Posix.Env.ByteString (getEnv, getEnvironment)
import System.Posix.Process (ProcessStatus (..), exitImmediately, getProcessID, getProcessStatus)
import System.Posix.Signals (Handler (Catch, Default), Signal, addSignal, emptySignalSet, installHandler, sigHUP, sigINT, sigKILL, sigPIPE, sigQUIT, sigTERM, signalProcess, unblockSignals)
import System.Posix.Temp (mkdtemp)
import System.Posix.Types (Fd (..), ProcessID)

newtype LaunchSignal = LaunchSignal Signal
  deriving stock (Show)

instance Exception LaunchSignal

originalVariable, wrapperVariable :: B.ByteString
originalVariable = "MONK_LAUNCH_ORIGINAL"
wrapperVariable = "MONK_LAUNCH_WRAPPER"

dispatchLaunch :: [String] -> IO ()
dispatchLaunch (file : arguments) = do
  result <- (Right <$> launch) `catch` (\(LaunchSignal signal) -> pure (Left signal)) `catch` failure
  case result of
    Left signal -> terminateWith signal
    Right (Terminated signal _) -> terminateWith signal
    Right (Exited code) -> exitImmediately code
    Right (Stopped _) -> exitWith (ExitFailure 125)
  where
    launch = do
      markers <- mapM getEnv [originalVariable, wrapperVariable]
      unless (all (== Nothing) markers) (ioError (userError "reserved launcher markers already present"))
      original <- makeAbsolute file
      originalBytes <- nativeBytes original
      values <- mapM nativeBytes arguments
      environment <- getEnvironment
      closed <- filterM (fmap not . initialDescriptorOpen) [0 :: Int .. 2]
      let streams = M.fromList [(number, Fd (fromIntegral number)) | number <- [0 .. 2], number `notElem` closed]
      if null closed
        then ownedFish streams environment originalBytes values
        else do
          temporary <- getTemporaryDirectory
          bracket (mkdtemp (temporary <> "/monk-launch-XXXXXX")) removeDirectoryRecursive $ \workspace -> do
            body <- B.readFile original
            wrapper <- bracket (openBinaryTempFile workspace "entry.fish") (hClose . snd) $ \(path, handle) -> do
              let closures = B.concat [" " <> C.pack (show number) <> ">&-" | number <- closed]
              B.hPut handle ("begin\n" <> body <> "\nend" <> closures <> "\n")
              hFlush handle
              nativeBytes path
            ownedFish streams ((originalVariable, originalBytes) : (wrapperVariable, wrapper) : environment) wrapper values
    failure (_ :: IOException) = launchFailure "native launch failed"
dispatchLaunch _ = launchFailure "launch needs a compiled Fish file"

-- Only the launched evaluator is waited for. Its asynchronous children retain
-- their own resources and survive ordinary evaluator/launcher completion.
ownedFish :: M.Map Int Fd -> [(B.ByteString, B.ByteString)] -> B.ByteString -> [B.ByteString] -> IO ProcessStatus
ownedFish streams environment script arguments = mask $ \restore -> do
  owner <- myThreadId
  signals <- filterM (fmap not . initialSignalIgnored) [sigHUP, sigINT, sigQUIT, sigTERM, sigPIPE]
  let install signal = do
        previous <- installHandler signal (Catch (throwTo owner (LaunchSignal signal))) Nothing
        pure (signal, previous)
      reset handlers = forM_ handlers $ \(signal, previous) -> void (installHandler signal previous Nothing)
  bracket (forM signals install) reset $ \_ -> do
    child <- spawnProcess streams Nothing environment "fish" (["--no-config", script] <> arguments)
    restore (waitChild child) `onException` terminateChild child

waitChild :: ProcessID -> IO ProcessStatus
waitChild child = do
  status <- getProcessStatus False False child
  maybe (threadDelay 1000 >> waitChild child) pure status

terminateChild :: ProcessID -> IO ()
terminateChild child = do
  signalProcess sigKILL child `catch` ignore
  void (waitChild child) `catch` ignore

terminateWith :: Signal -> IO a
terminateWith signal = do
  void (installHandler signal Default Nothing)
  unblockSignals (addSignal signal emptySignalSet)
  getProcessID >>= signalProcess signal
  exitImmediately (ExitFailure (128 + fromIntegral signal))

launchFailure :: B.ByteString -> IO a
launchFailure message = do
  visible <- initialDescriptorOpen (2 :: Int)
  when visible ((B.hPut stderr ("monk-runtime: " <> message <> "\n") >> hFlush stderr) `catch` ignore)
  exitWith (ExitFailure 125)

ignore :: IOException -> IO ()
ignore _ = pure ()

nativeBytes :: FilePath -> IO B.ByteString
nativeBytes path = getFileSystemEncoding >>= \encoding -> Foreign.withCString encoding path B.packCString

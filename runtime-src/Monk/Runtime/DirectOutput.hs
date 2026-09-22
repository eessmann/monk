{-# LANGUAGE OverloadedStrings #-}

-- | Bounded builtin writers over fully read byte frames. Semantic descriptors
-- are never installed or closed here; startup presence guards RTS fd reuse.
module Monk.Runtime.DirectOutput (dispatchWriteBuiltin, dispatchRaiseSignal) where

import Control.Exception (IOException, catch)
import Control.Monad (unless, void, when)
import Data.ByteString qualified as B
import Foreign.C.Error (eBADF, eIO, errnoToIOError)
import Foreign.Ptr (castPtr, plusPtr)
import Monk.Runtime.Descriptors (initialDescriptorOpen, nativeErrorMessage)
import Monk.Runtime.Fields (echoBytes)
import Monk.Runtime.Printf (printfBytes)
import Monk.Runtime.Protocol (decodeFrames)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.Posix.IO (fdWriteBuf)
import System.Posix.Process (exitImmediately, getProcessID)
import System.Posix.Signals (Handler (Default), addSignal, emptySignalSet, installHandler, sigPIPE, signalProcess, unblockSignals)
import System.Posix.Types (Fd)

-- | Origin, positive source line, builtin name and byte arguments. Echo-bytes
-- is a single already-proved literal payload, with echo diagnostic identity.
dispatchWriteBuiltin :: IO ()
dispatchWriteBuiltin = do
  frames <- B.getContents >>= either runtimeFailure pure . decodeFrames
  case frames of
    origin : line : name : arguments
      | not (B.null origin),
        validLine line -> do
          bytes <- either runtimeFailure pure $ case name of
            "echo" -> Right (echoBytes arguments)
            "printf" -> printfBytes arguments
            "echo-bytes" -> case arguments of [value] -> Right value; _ -> Left "echo-bytes needs one payload"
            _ -> Left "write-builtin needs an admitted builtin"
          writeOutput origin line (if name == "echo-bytes" then "echo" else name) bytes
    _ -> runtimeFailure "write-builtin needs origin, positive source line, builtin and arguments"
  exitImmediately ExitSuccess
  where
    validLine value = not (B.null value) && B.head value /= 48 && B.all (\byte -> byte >= 48 && byte <= 57) value

writeOutput :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString -> IO ()
writeOutput origin line name bytes = unless (B.null bytes) $ do
  outputOpen <- initialDescriptorOpen 1
  let diagnostic failure = do
        message <- nativeErrorMessage failure
        writeDiagnostic (origin <> ": line " <> line <> ": " <> name <> ": write error: " <> message <> "\n")
        exitImmediately (ExitFailure 1)
      output = do
        unless outputOpen (ioError (errnoToIOError "write" eBADF Nothing Nothing))
        writeBytes 1 bytes
  output `catch` diagnostic

-- No Handle buffering can repeat a failed write during shutdown. SIGPIPE has
-- its default disposition at the runtime entry, so a broken pipe kills us.
writeBytes :: Fd -> B.ByteString -> IO ()
writeBytes descriptor bytes = B.useAsCStringLen bytes $ \(pointer, count) -> do
  let loop offset
        | offset == count = pure ()
        | otherwise = do
            written <- fdWriteBuf descriptor (castPtr (pointer `plusPtr` offset)) (fromIntegral (count - offset))
            when (written == 0) (ioError (errnoToIOError "write" eIO Nothing Nothing))
            loop (offset + fromIntegral written)
  loop 0

writeDiagnostic :: B.ByteString -> IO ()
writeDiagnostic bytes = do
  visible <- initialDescriptorOpen 2
  when visible (writeBytes 2 bytes `catch` ignore)
  where
    ignore :: IOException -> IO ()
    ignore _ = pure ()

runtimeFailure :: B.ByteString -> IO a
runtimeFailure message = do
  writeDiagnostic ("monk-runtime: " <> message <> "\n")
  exitImmediately (ExitFailure 125)

-- | Reproduce a writer's actual SIGPIPE termination in its owning evaluator.
-- This fixed operation consumes no stdin and admits no arbitrary signal value.
dispatchRaiseSignal :: [String] -> IO ()
dispatchRaiseSignal ["13"] = do
  void (installHandler sigPIPE Default Nothing)
  unblockSignals (addSignal sigPIPE emptySignalSet)
  getProcessID >>= signalProcess sigPIPE
  exitWith (ExitFailure 141)
dispatchRaiseSignal _ = runtimeFailure "raise-signal needs signal 13"

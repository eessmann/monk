{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Bounded executable invocation and source-aware kernel exec diagnostics.
-- There is deliberately no ENOEXEC fallback to a shell source interpreter.
module Monk.Runtime.Exec (dispatchExecSite, executionFailure) where

import Control.Exception (IOException, catch)
import Control.Monad (when)
import Data.ByteString qualified as B
import Foreign.C.Error (Errno, eACCES, eNOENT, eNOEXEC, eNOTDIR, errnoToIOError)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import GHC.Foreign qualified as Foreign
import GHC.IO.Encoding (getFileSystemEncoding)
import Monk.Runtime.Descriptors (initialDescriptorOpen, nativeErrorMessage)
import Monk.Runtime.Protocol (protocolFailure)
import Monk.Runtime.Spawn (execProcess)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hFlush, stderr)
import System.Posix.Env.ByteString (getEnvironment)
import System.Posix.Types (Fd (..))

foreign import ccall unsafe "monk_directory_at" directoryAt :: CInt -> CString -> IO CInt

executionFailure :: Maybe Fd -> B.ByteString -> B.ByteString -> B.ByteString -> Errno -> IO (Int, B.ByteString)
executionFailure cwd origin line command failure = do
  directory <- if failure == eACCES then (/= 0) <$> B.useAsCString command (directoryAt (maybe (-1) (\(Fd fd) -> fd) cwd)) else pure False
  message <-
    if directory
      then pure "Is a directory"
      else
        if failure == eNOEXEC
          then pure "cannot execute binary file: Exec format error"
          else
            if not (B.elem 47 command) && failure `elem` [eNOENT, eNOTDIR]
              then pure "command not found"
              else nativeErrorMessage (errnoToIOError "execute" failure Nothing Nothing)
  let code = if failure == eNOENT || (not (B.elem 47 command) && failure == eNOTDIR) then 127 else 126
  pure (code, origin <> ": line " <> line <> ": " <> command <> ": " <> message <> "\n")

dispatchExecSite :: [String] -> IO ()
dispatchExecSite (origin : line : command : arguments) = do
  source <- nativeBytes origin
  row <- nativeBytes line
  executable <- nativeBytes command
  values <- mapM nativeBytes arguments
  environment <- filter (\(name, _) -> name `notElem` ["MONK_LAUNCH_ORIGINAL", "MONK_LAUNCH_WRAPPER"]) <$> getEnvironment
  failure <- execProcess environment executable values
  (code, diagnostic) <- executionFailure Nothing source row executable failure
  visible <- initialDescriptorOpen (2 :: Int)
  when visible ((B.hPut stderr diagnostic >> hFlush stderr) `catch` (\(_ :: IOException) -> pure ()))
  exitWith (ExitFailure code)
dispatchExecSite _ = protocolFailure "exec-site needs origin, line and executable"

nativeBytes :: FilePath -> IO B.ByteString
nativeBytes path = getFileSystemEncoding >>= \encoding -> Foreign.withCString encoding path B.packCString

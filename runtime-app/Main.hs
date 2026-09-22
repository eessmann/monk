{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM_, unless, void, when)
import Data.Bits ((.&.))
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.Char (digitToInt)
import Monk.Runtime.Child (dispatchChild, dispatchChildInSession)
import Monk.Runtime.Descriptors (descriptorMask)
import Monk.Runtime.DirectOutput (dispatchRaiseSignal, dispatchWriteBuiltin)
import Monk.Runtime.Directory (directoryDiagnostic, directoryPathBound, directoryStack, initialOldpwdValid, physicalDirectory, validateDirectories)
import Monk.Runtime.Exec (dispatchExecSite)
import Monk.Runtime.Expansion (expandWords)
import Monk.Runtime.Fields
import Monk.Runtime.Integer
import Monk.Runtime.Launch (dispatchLaunch)
import Monk.Runtime.NativeTarget
import Monk.Runtime.Pattern
import Monk.Runtime.PipePaths (probePipePaths)
import Monk.Runtime.Printf (printfBytes)
import Monk.Runtime.Protocol
import Monk.Runtime.Session (dispatchSession, dispatchSessionClient, dispatchSessionDirectoryDiagnostic, dispatchSessionExecError, dispatchSessionWrite)
import Monk.Runtime.Session.Capsule (dispatchCapsuleGuardian, prepareCapsule)
import Monk.Runtime.Spawn (initialSignalIgnored)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.IO (hFlush, hSetBinaryMode, stdin, stdout)
import System.Info (os)
import System.Posix.Process (exitImmediately)
import System.Posix.Signals (Handler (Default, Ignore), installHandler, sigINT, sigPIPE, sigQUIT)

main :: IO ()
main = do
  forM_ [sigINT, sigQUIT] $ \signal -> do
    ignored <- initialSignalIgnored signal
    when ignored (void (installHandler signal Ignore Nothing))
  unless supportedNativeTarget (protocolFailure "unsupported native OS or architecture")
  _ <- installHandler sigPIPE Default Nothing
  args <- getArgs
  case args of
    ["--describe"] -> do
      C.putStr (C.pack runtimeDescriptionHeader <> "\nargv bytes-platform child-capture child-run descriptor-state directory echo exec-site expansion glob integer launch pattern pattern-parts pipe-paths printf session split write-builtin\ntarget " <> C.pack runtimeTarget <> "\n")
      hFlush stdout
      exitImmediately ExitSuccess
    "--abi" : "2" : "session-guardian" : arguments -> dispatchCapsuleGuardian arguments
    "--abi" : "2" : "launch" : arguments -> dispatchLaunch arguments
    "--abi" : "2" : "exec-site" : arguments -> dispatchExecSite arguments
    ["--abi", "2", "write-builtin"] -> dispatchWriteBuiltin
    "--abi" : "2" : "raise-signal" : arguments -> dispatchRaiseSignal arguments
    ["--abi", "2", "session-prepare"] -> prepareCapsule
    "--abi" : "2" : "session-exec-error" : arguments -> dispatchSessionExecError arguments
    "--abi" : "2" : "session-write" : arguments -> dispatchSessionWrite arguments
    "--abi" : "2" : "session-run" : arguments -> dispatchSession arguments
    ["--abi", "2", "session-directory-diagnostic"] -> dispatchSessionDirectoryDiagnostic
    ["--abi", "2", "session-client"] -> dispatchSessionClient False
    ["--abi", "2", "session-client", "--reply"] -> dispatchSessionClient True
    ["--abi", "2", "pipe-paths"] -> probePipePaths >>= \available -> exitWith (if available then ExitSuccess else ExitFailure 125)
    ["--abi", "2", "descriptor-state"] -> do
      mask <- (.&. 7) <$> descriptorMask
      exitWith (if mask == 0 then ExitSuccess else ExitFailure mask)
    ["--abi", "2", "directory-diagnostic"] -> B.getContents >>= directoryDiagnostic
    ["--abi", "2", "directory-physical"] -> physicalDirectory
    ["--abi", "2", "child-run-session"] -> dispatchChildInSession False
    ["--abi", "2", "child-capture-session"] -> dispatchChildInSession True
    ["--abi", "2", "child-run"] -> dispatchChild False
    ["--abi", "2", "child-capture"] -> dispatchChild True
    ["--abi", "2", op] -> do
      hSetBinaryMode stdin True
      hSetBinaryMode stdout True
      frames <- B.getContents >>= either protocolFailure pure . decodeFrames
      case (op, frames) of
        ("bytes-platform", [darwin, linux]) -> do
          darwinValue <- either protocolFailure pure (decodeHex darwin)
          linuxValue <- either protocolFailure pure (decodeHex linux)
          B.putStr (encodeFrames [if os == "darwin" then darwinValue else linuxValue])
        ("directory-path-bound", [cwd, operand]) -> unless (directoryPathBound cwd operand) (exitWith (ExitFailure 125))
        ("directory-initial-oldpwd", [path]) -> initialOldpwdValid path >>= \valid -> unless valid (exitWith (ExitFailure 1))
        ("directory-validate", _) -> validateDirectories frames >>= \valid -> unless valid (exitWith (ExitFailure 125))
        ("directory-stack", _) -> either protocolFailure B.putStr (directoryStack frames)
        ("integer", _) -> either protocolFailure B.putStr (integerOperation frames)
        ("split", [ifs, value]) -> B.putStr (encodeFrames (splitFields ifs value))
        ("argv", _) -> either protocolFailure (B.putStr . encodeFrames) (argvFields frames)
        ("printf", _) -> either protocolFailure B.putStr (printfBytes frames)
        ("expansion", _) -> expandWords frames >>= either protocolFailure (B.putStr . encodeFrames)
        ("echo", _) -> B.putStr (echoBytes frames)
        ("pattern", "match" : subject : rest) -> do parts <- either protocolFailure pure (patternParts rest); if matches subject parts then pure () else exitWith (ExitFailure 1)
        ("pattern-parts", operation : subject : rest)
          | operation `elem` ["trim-prefix-short", "trim-prefix-long", "trim-suffix-short", "trim-suffix-long"] -> do
              parts <- either protocolFailure pure (patternParts rest)
              B.putStr (encodeFrames [trimPatternParts (operation `elem` ["trim-prefix-short", "trim-prefix-long"]) (operation `elem` ["trim-prefix-long", "trim-suffix-long"]) subject parts])
        ("pattern", [operation, subject, patternBytes])
          | operation `elem` ["trim-prefix-short", "trim-prefix-long", "trim-suffix-short", "trim-suffix-long"] ->
              B.putStr (encodeFrames [trimPattern (operation `elem` ["trim-prefix-short", "trim-prefix-long"]) (operation `elem` ["trim-prefix-long", "trim-suffix-long"]) subject patternBytes])
        ("pattern", [operation, subject, needle, replacement])
          | operation `elem` ["replace-first", "replace-all"] ->
              B.putStr (encodeFrames [replaceLiteral (operation == "replace-all") subject needle replacement])
        ("glob", _) -> do parts <- either protocolFailure pure (patternParts frames); globPaths parts >>= B.putStr . encodeFrames
        _ -> protocolFailure "unknown operation or invalid frame count"
    _ -> protocolFailure "expected --abi 2 OPERATION or --describe"

-- Platform-tagged literals carry hex rather than encoding arbitrary bytes in
-- Fish source. Both alternatives are validated before selecting the target.
decodeHex :: B.ByteString -> Either B.ByteString B.ByteString
decodeHex bytes
  | odd (B.length bytes) || not (B.all valid bytes) = Left "invalid platform byte hex"
  | otherwise = Right (B.pack (decode (C.unpack bytes)))
  where
    valid byte = (byte >= 48 && byte <= 57) || (byte >= 65 && byte <= 70) || (byte >= 97 && byte <= 102)
    decode (high : low : rest) = fromIntegral (16 * digitToInt high + digitToInt low) : decode rest
    decode _ = []

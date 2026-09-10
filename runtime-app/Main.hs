{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import Data.Bits (finiteBitSize, (.&.))
import Data.ByteString qualified as B
import Monk.Runtime.Child (dispatchChild)
import Monk.Runtime.Descriptors (descriptorMask)
import Monk.Runtime.Directory (directoryDiagnostic, directoryPathBound, directoryStack, initialOldpwdValid, physicalDirectory, validateDirectories)
import Monk.Runtime.Fields
import Monk.Runtime.Integer
import Monk.Runtime.Pattern
import Monk.Runtime.Protocol
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.IO (hSetBinaryMode, stdin, stdout)
import System.Info (os)
import System.Posix.Signals (Handler (Default), installHandler, sigPIPE)

main :: IO ()
main = do
  unless (os == "linux" && finiteBitSize (0 :: Int) == 64) (protocolFailure "this runtime profile requires 64-bit Linux")
  _ <- installHandler sigPIPE Default Nothing
  args <- getArgs
  case args of
    ["--describe"] -> B.putStr "monk-runtime 1 bash53-i64-linux64\nargv child-capture child-run descriptor-state directory echo glob integer pattern split\n"
    ["--abi", "1", "descriptor-state"] -> do
      mask <- (.&. 7) <$> descriptorMask
      exitWith (if mask == 0 then ExitSuccess else ExitFailure mask)
    ["--abi", "1", "directory-diagnostic"] -> B.getContents >>= directoryDiagnostic
    ["--abi", "1", "directory-physical"] -> physicalDirectory
    ["--abi", "1", "child-run"] -> dispatchChild False
    ["--abi", "1", "child-capture"] -> dispatchChild True
    ["--abi", "1", op] -> do
      hSetBinaryMode stdin True
      hSetBinaryMode stdout True
      frames <- B.getContents >>= either protocolFailure pure . decodeFrames
      case (op, frames) of
        ("directory-path-bound", [cwd, operand]) -> unless (directoryPathBound cwd operand) (exitWith (ExitFailure 125))
        ("directory-initial-oldpwd", [path]) -> initialOldpwdValid path >>= \valid -> unless valid (exitWith (ExitFailure 1))
        ("directory-validate", _) -> validateDirectories frames >>= \valid -> unless valid (exitWith (ExitFailure 125))
        ("directory-stack", _) -> either protocolFailure B.putStr (directoryStack frames)
        ("integer", _) -> either protocolFailure B.putStr (integerOperation frames)
        ("split", [ifs, value]) -> B.putStr (encodeFrames (splitFields ifs value))
        ("argv", _) -> either protocolFailure (B.putStr . encodeFrames) (argvFields frames)
        ("echo", _) -> B.putStr (echoBytes frames)
        ("pattern", "match" : subject : rest) -> do parts <- either protocolFailure pure (patternParts rest); if matches subject parts then pure () else exitWith (ExitFailure 1)
        ("pattern", [operation, subject, patternBytes])
          | operation `elem` ["trim-prefix-short", "trim-prefix-long", "trim-suffix-short", "trim-suffix-long"] ->
              B.putStr (encodeFrames [trimPattern (operation `elem` ["trim-prefix-short", "trim-prefix-long"]) (operation `elem` ["trim-prefix-long", "trim-suffix-long"]) subject patternBytes])
        ("pattern", [operation, subject, needle, replacement])
          | operation `elem` ["replace-first", "replace-all"] ->
              B.putStr (encodeFrames [replaceLiteral (operation == "replace-all") subject needle replacement])
        ("glob", _) -> do parts <- either protocolFailure pure (patternParts frames); globPaths parts >>= B.putStr . encodeFrames
        _ -> protocolFailure "unknown operation or invalid frame count"
    _ -> protocolFailure "expected --abi 1 OPERATION or --describe"

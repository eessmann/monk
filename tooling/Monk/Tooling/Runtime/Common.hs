{-# LANGUAGE OverloadedStrings #-}

-- | Shared byte-precise process and fixture helpers for native-runtime checks.
module Monk.Tooling.Runtime.Common
  ( Context (..),
    check,
    checkResult,
    code,
    frames,
    invoke,
    invokeWith,
    referenceEnvironment,
    sessionEnvironment,
    withWorkspace,
    writeScript,
    rpcHeader,
    quoteFish,
    shellBytes,
  )
where

import Control.Exception (bracket)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Monk.Tooling.Process (ProcessResult (..), ProcessSpec (..), runProcess)
import System.Directory (createDirectory, getTemporaryDirectory, removePathForcibly)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.IO (hClose, openTempFile)

data Context = Context
  { runtime :: FilePath,
    monk :: Maybe FilePath
  }
  deriving stock (Eq, Show)

check :: String -> Bool -> IO ()
check label ok = unless ok (fail label)

code :: ProcessResult -> Int
code result = case processExit result of
  ExitSuccess -> 0
  ExitFailure value -> value

checkResult :: String -> (Int, B.ByteString, B.ByteString) -> ProcessResult -> IO ()
checkResult label expected actual =
  check (label <> ": expected " <> show expected <> ", got " <> show actual) $
    not (processTimedOut actual)
      && (code actual, processStdout actual, processStderr actual) == expected

frames :: [B.ByteString] -> B.ByteString
frames = B.concat . map (<> "\0")

invoke :: FilePath -> [String] -> B.ByteString -> IO ProcessResult
invoke binary args input = invokeWith binary args input Nothing Nothing 10000000

invokeWith :: FilePath -> [String] -> B.ByteString -> Maybe FilePath -> Maybe [(String, String)] -> Int -> IO ProcessResult
invokeWith binary args input directory env timeout =
  runProcess
    ProcessSpec
      { executable = binary,
        arguments = args,
        workingDirectory = directory,
        environment = env,
        stdinBytes = input,
        timeoutMicros = timeout
      }

referenceEnvironment :: IO [(String, String)]
referenceEnvironment = do
  inherited <- getEnvironment
  pure $ ("LC_ALL", "C") : ("LANG", "C") : filter (\(name, _) -> name `notElem` ["LC_ALL", "LANG", "BASH_ENV", "ENV", "BASHOPTS", "SHELLOPTS", "MONK_SESSION_SOCKET", "MONK_SESSION_TOKEN", "MONK_SESSION_REPLY", "MONK_SESSION_FDS", "MONK_LAUNCH_ORIGINAL", "MONK_LAUNCH_WRAPPER"]) inherited

sessionEnvironment :: FilePath -> IO [(String, String)]
sessionEnvironment binary = (("MONK_RUNTIME", binary) :) . filter ((/= "MONK_RUNTIME") . fst) <$> referenceEnvironment

withWorkspace :: (FilePath -> IO a) -> IO a
withWorkspace = bracket acquire removePathForcibly
  where
    acquire = do
      temporary <- getTemporaryDirectory
      (path, handle) <- openTempFile temporary "monk-runtime-hs-"
      hClose handle
      removePathForcibly path
      createDirectory path
      pure path

writeScript :: FilePath -> String -> IO FilePath
writeScript path source = do
  C.writeFile path (C.pack source)
  pure path

rpcHeader :: String
rpcHeader =
  intercalate
    "\n"
    [ "function rpc",
      " begin",
      " printf '%s\\0' $argv | command \"$MONK_RUNTIME\" --abi 2 session-client --reply",
      " end 3<&0 4>&1 5>&2",
      " set -g response (string split0 < \"$MONK_SESSION_REPLY\")",
      "end"
    ]
    <> "\n"

quoteFish :: String -> String
quoteFish value = "'" <> concatMap escape value <> "'"
  where
    escape '\\' = "\\\\"
    escape '\'' = "\\'"
    escape character = [character]

shellBytes :: B.ByteString -> String
shellBytes = intercalate "" . map render . B.unpack
  where
    render byte
      | byte >= 32 && byte <= 126 && byte /= 39 && byte /= 92 = [toEnum (fromIntegral byte)]
      | otherwise = "\\" <> showOctal byte
    showOctal byte = let n = fromIntegral byte :: Int; digit i = toEnum (fromEnum '0' + i) in [digit (n `div` 64), digit ((n `div` 8) `mod` 8), digit (n `mod` 8)]

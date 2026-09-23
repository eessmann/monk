{-# LANGUAGE OverloadedStrings #-}

-- | Entry point wired by @monk-tool runtime check@.
module Monk.Tooling.Runtime.Checks (runChecks, runSuite, suiteNames) where

import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Monk.Host.Process qualified as Host
import Monk.Runtime.Digest (sha256)
import Monk.Tooling.Runtime.Common (Context (..))
import Monk.Tooling.Runtime.Ownership
import Monk.Tooling.Runtime.Semantics
import Monk.Tooling.Runtime.Sessions
import Monk.Tooling.Runtime.Suite
import System.Directory (canonicalizePath)
import System.Exit (ExitCode (ExitSuccess))
import System.IO qualified as IO
import System.Process (proc)

suiteNames :: [String]
suiteNames = map suiteName allSuites

runChecks :: String -> FilePath -> Maybe FilePath -> IO ()
runChecks name runtimePath monkPath = either fail (\suite -> runSuite suite runtimePath monkPath) (parseSuite name)

runSuite :: Suite -> FilePath -> Maybe FilePath -> IO ()
runSuite suite runtimePath monkPath = do
  when (suiteNeedsTranslator suite && isNothing monkPath) $
    fail ("runtime suite " <> suiteName suite <> " requires --monk")
  binary <- canonicalizePath runtimePath
  translator <- traverse canonicalizePath monkPath
  let ctx = Context binary translator
  case suite of
    CallbackDiagnostics -> runCallbackDiagnostics ctx
    Descriptors -> runDescriptors ctx
    Digest -> runDigest ctx
    DirectOutput -> runDirectOutput ctx
    DirectorySignals -> runDirectorySignals ctx
    Exec -> runExec ctx
    Expansion -> runExpansion ctx
    NativeLauncher -> runNativeLauncher ctx
    PatternParts -> runPatternParts ctx
    Portable -> runPortable ctx
    Printf -> runPrintf ctx
    ProcessSubstitution -> runProcessSubstitution ctx
    Protocol -> runProtocol ctx
    Read -> runRead ctx
    Session -> runSession ctx
    Signals -> runSignals ctx
    ChildTransport -> runChildTransport binary
  putStrLn ("runtime " <> suiteName suite <> " checks passed")

runChildTransport :: FilePath -> IO ()
runChildTransport binary = do
  let script = "test/native/child-transport.sh"
      expectedScriptHash = "5fa3e5f7af8745522c230564b6b70d3587df9f49e338d81509e8a09f5aee5a85"
  scriptBytes <- B.readFile script
  unless (C.unpack (sha256 scriptBytes) == expectedScriptHash) $
    fail "child transport check script differs from the version embedded in monk-tool"
  result <- Host.runCreateProcess Nothing (proc "bash" [script, binary]) ""
  B.hPut IO.stdout (Host.processStdout result)
  B.hPut IO.stderr (Host.processStderr result)
  unless (Host.processExit result == ExitSuccess) (fail "child transport checks failed")

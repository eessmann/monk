{-# LANGUAGE OverloadedStrings #-}

-- | Entry point wired by @monk-tool runtime check@.
module Monk.Tooling.Runtime.Checks (runChecks, suiteNames) where

import Monk.Tooling.Runtime.Common (Context (..))
import Monk.Tooling.Runtime.Ownership
import Monk.Tooling.Runtime.Semantics
import Monk.Tooling.Runtime.Sessions
import System.Directory (canonicalizePath)

suiteNames :: [String]
suiteNames =
  [ "callback-diagnostics",
    "descriptors",
    "digest",
    "direct-output",
    "directory-signals",
    "exec",
    "expansion",
    "native-launcher",
    "pattern-parts",
    "portable",
    "printf",
    "process-substitution",
    "protocol",
    "read",
    "session",
    "signals"
  ]

runChecks :: String -> FilePath -> Maybe FilePath -> IO ()
runChecks suite runtimePath monkPath = do
  binary <- canonicalizePath runtimePath
  translator <- traverse canonicalizePath monkPath
  let ctx = Context binary translator
  case suite of
    "callback-diagnostics" -> runCallbackDiagnostics ctx
    "descriptors" -> runDescriptors ctx
    "digest" -> runDigest ctx
    "direct-output" -> runDirectOutput ctx
    "directory-signals" -> runDirectorySignals ctx
    "exec" -> runExec ctx
    "expansion" -> runExpansion ctx
    "native-launcher" -> runNativeLauncher ctx
    "pattern-parts" -> runPatternParts ctx
    "portable" -> runPortable ctx
    "printf" -> runPrintf ctx
    "process-substitution" -> runProcessSubstitution ctx
    "protocol" -> runProtocol ctx
    "read" -> runRead ctx
    "session" -> runSession ctx
    "signals" -> runSignals ctx
    _ -> fail ("unknown runtime check suite: " <> suite)
  putStrLn ("runtime " <> suite <> " checks passed")

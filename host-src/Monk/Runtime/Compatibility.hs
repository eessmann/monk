-- | Validate a captured executable against the fixed public runtime protocol.
module Monk.Runtime.Compatibility (checkRuntimeFile) where

import Control.Exception (IOException, try)
import Data.ByteString.Char8 qualified as B
import Monk.Host.Process
import Monk.Runtime.NativeTarget (runtimeDescriptionHeader, runtimeTarget, supportedNativeTarget)
import System.Exit (ExitCode (ExitSuccess))

checkRuntimeFile :: FilePath -> [String] -> IO (Either String ())
checkRuntimeFile path operations = do
  observed <- try @IOException $ runProcess (ProcessSpec path ["--describe"] Nothing Nothing "" 5000000)
  pure $ case observed of
    Left failure -> Left ("cannot execute native runtime: " <> show failure)
    Right result
      | processTimedOut result -> Left "native runtime compatibility check timed out"
      | processExit result == ExitSuccess && B.null (processStderr result) ->
          case map B.unpack (B.lines (processStdout result)) of
            [header, capabilities, target]
              | supportedNativeTarget && header == runtimeDescriptionHeader && target == "target " <> runtimeTarget && all (`elem` map B.unpack (B.words (B.pack capabilities))) operations -> Right ()
            _ -> Left "native runtime ABI, profile, or operation capabilities do not match"
      | otherwise -> Left "native runtime compatibility check failed"

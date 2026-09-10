-- | Validate a captured executable against the fixed public runtime protocol.
module Monk.Runtime.Compatibility (checkRuntimeFile) where

import Control.Exception (IOException, try)
import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)

checkRuntimeFile :: FilePath -> [String] -> IO (Either String ())
checkRuntimeFile path operations = do
  observed <- try @IOException (timeout 5000000 (readProcessWithExitCode path ["--describe"] ""))
  pure $ case observed of
    Left failure -> Left ("cannot execute native runtime: " <> show failure)
    Right Nothing -> Left "native runtime compatibility check timed out"
    Right (Just (ExitSuccess, output, "")) -> case lines output of
      ["monk-runtime 1 bash53-i64-linux64", capabilities]
        | all (`elem` words capabilities) operations -> Right ()
      _ -> Left "native runtime ABI, profile, or operation capabilities do not match"
    Right (Just _) -> Left "native runtime compatibility check failed"

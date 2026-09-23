-- | Identify Bash and Fish providers by bytes and observable behavior.
module Monk.Tooling.Evidence.Profile (runProfile) where

import Data.Aeson (Value, object, (.=))
import Data.ByteString.Char8 qualified as C
import Monk.Tooling.Evidence.Common (Observation (..), base64, digestFile, hostPlatform, runObservation)
import System.Directory (canonicalizePath, findExecutable, getCurrentDirectory)
import System.Environment (getEnvironment)

runProfile :: String -> String -> IO Value
runProfile bashName fishName = do
  cwd <- getCurrentDirectory
  inherited <- getEnvironment
  let env = [("LC_ALL", "C"), ("LANG", "C")] <> filter (\(key, _) -> key `notElem` ["LC_ALL", "LANG", "BASH_ENV", "ENV", "SHELLOPTS", "BASHOPTS"]) inherited
  providers <- forM [("bash", bashName), ("fish", fishName)] $ \(name, command) -> do
    located <- findExecutable command >>= maybe (fail ("Executable not found: " <> command)) canonicalizePath
    hash <- digestFile located
    version <- runObservation [located, "--version"] "" cwd env 30
    unless (status version == "completed" && exit version == 0) $ fail (name <> " version probe failed")
    let versionLine = case C.lines (observedStdout version) of line : _ -> C.unpack line; [] -> ""
    pure (name, located, object ["path" .= located, "sha256" .= hash, "version" .= versionLine])
  let bash = case providers of (_, path, _) : _ -> path; _ -> bashName
  probes <- forM ["a\\uD800b", "a\\U00110000b", "a\\U7fffffffb", "a\\U80000000b", "a\\Uffffffffb"] $ \value -> do
    observed <- runObservation [bash, "--noprofile", "--norc", "-c", "echo -e \"$1\"", "reference-probe", value] "" cwd env 30
    unless (status observed == "completed" && exit observed == 0) $ fail ("Bash probe failed: " <> value)
    pure (value, object ["stdout_base64" .= base64 (observedStdout observed), "stderr_base64" .= base64 (observedStderr observed), "exit" .= exit observed])
  platform <- hostPlatform
  pure $
    object
      [ "platform" .= platform,
        "locale" .= ("C" :: Text),
        "executables" .= object [fromString name .= value | (name, _, value) <- providers],
        "bash_probes" .= object [fromString key .= value | (key, value) <- probes]
      ]

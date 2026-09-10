module SourceTestSupport (withSources, requireGraph, renderGraph, assertSourceEquivalent) where

import Monk.Output
import Monk.Source
import Monk.Translation
import Path (toFilePath)
import Path.IO qualified as PathIO
import ShellSupport (prepareEnv, shouldRunIntegration)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode)
import System.FilePath (takeDirectory, (</>))
import System.Process (CreateProcess (cwd, env), proc, readCreateProcessWithExitCode)
import Test.Tasty.HUnit

withSources :: Text -> [(FilePath, Text)] -> (FilePath -> SourceEnvironment -> IO ()) -> IO ()
withSources root dependencies action = PathIO.withSystemTempDir "monk-owned-source" $ \directory -> do
  let base = toFilePath directory
      path = base </> "root.bash"
  forM_ (("root.bash", root) : dependencies) $ \(relative, text) -> do
    createDirectoryIfMissing True (takeDirectory (base </> relative))
    writeFileText (base </> relative) text
  action path (MkSourceEnvironment base [] True)

requireGraph :: TranslateConfig -> SourceEnvironment -> FilePath -> IO SourceGraph
requireGraph cfg environment path = do
  result <- translateSourceGraphWithEnvironment cfg environment True path
  case result of
    Left failure -> assertFailure (show failure) >> fail "source graph rejected"
    Right graph -> pure graph

renderGraph :: SourceGraph -> IO Text
renderGraph graph = do
  planned <- planCombinedOutputBundle OutputStdout graph
  case planned of
    Left diagnostic -> assertFailure (show diagnostic) >> fail "combined planning failed"
    Right bundle -> case renderOutputBundle bundle of
      [(OutputStdout, text)] -> pure text
      other -> assertFailure (show other) >> fail "unexpected output layout"

assertSourceEquivalent :: FilePath -> SourceEnvironment -> SourceGraph -> [String] -> IO ()
assertSourceEquivalent path environment graph arguments = do
  readiness <- shouldRunIntegration
  case readiness of
    Left reason -> putStrLn ("skipped source differential: " <> reason)
    Right () -> do
      generated <- renderGraph graph
      baseEnv <- prepareEnv
      let run command args =
            readCreateProcessWithExitCode
              (proc command args) {cwd = Just (sourceWorkingDirectory environment), env = Just baseEnv}
              ""
      bash <- run "bash" (["--noprofile", "--norc", path] <> arguments)
      fish <- run "fish" (["--no-config", "-c", toString generated] <> arguments)
      assertEqual "source stdout/stderr/status" (bash :: (ExitCode, String, String)) fish

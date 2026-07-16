module Bakeoff.Execution.Runtime
  ( buildRuntimeReport,
    runRuntimeBenchmarkEntry,
  )
where

import Bakeoff.Artifacts
  ( FixtureArtifacts (..),
    writeTextFile,
  )
import Bakeoff.Execution.Shared (exitCodeToInt)
import Bakeoff.Fixture (FixtureMetadata (..))
import Bakeoff.Process
  ( ProcessOutput (..),
    runProcessText,
  )
import Bakeoff.Shell (ShellRunMode (..))
import Bakeoff.Types
import Control.Exception (IOException, try)
import Data.Text qualified as T
import Path
  ( Abs,
    File,
    Path,
    toFilePath,
  )
import Path.IO qualified as PathIO
import System.Process (CreateProcess (env), proc)

buildRuntimeReport ::
  BakeoffConfig ->
  ResolvedTools ->
  FixtureSpec ->
  FixtureArtifacts ->
  ToolName ->
  TranslationReport ->
  [(String, String)] ->
  IO RuntimeReport
buildRuntimeReport cfg tools fixture artifacts tool translationReport processEnv =
  case translationStatus translationReport of
    CommandSucceeded -> do
      let scriptPath =
            case tool of
              ToolMonk -> faMonkFish artifacts
              ToolBabelfish -> faBabelfishFish artifacts
          stdoutPath =
            case tool of
              ToolMonk -> faMonkStdout artifacts
              ToolBabelfish -> faBabelfishStdout artifacts
          stderrPath =
            case tool of
              ToolMonk -> faMonkRuntimeStderr artifacts
              ToolBabelfish -> faBabelfishRuntimeStderr artifacts
          exitCodePath =
            case tool of
              ToolMonk -> faMonkExitCode artifacts
              ToolBabelfish -> faBabelfishExitCode artifacts
          metadata = specMetadata fixture
      result <-
        runProcessText
          (Just (bakeoffRuntimeTimeoutSeconds cfg))
          (runtimeProcess tools processEnv (fmMode metadata) scriptPath (fmArgs metadata))
          (fmStdin metadata)
      case result of
        Nothing -> do
          writeTextFile stderrPath "runtime timed out\n"
          pure
            MkRuntimeReport
              { runtimeTool = tool,
                runtimeStatus = CommandTimedOut,
                runtimeExitCode = Nothing,
                runtimeStdoutPath = Nothing,
                runtimeStderrPath = Just stderrPath,
                runtimeErrorMessage = Just "runtime timed out"
              }
        Just MkProcessOutput {..} -> do
          let exitCodeInt = exitCodeToInt poExitCode
          writeTextFile stdoutPath poStdout
          writeTextFile stderrPath poStderr
          writeTextFile exitCodePath (show exitCodeInt <> "\n")
          pure
            MkRuntimeReport
              { runtimeTool = tool,
                runtimeStatus = CommandSucceeded,
                runtimeExitCode = Just exitCodeInt,
                runtimeStdoutPath = Just stdoutPath,
                runtimeStderrPath = Just stderrPath,
                runtimeErrorMessage = Nothing
              }
    _ ->
      pure
        MkRuntimeReport
          { runtimeTool = tool,
            runtimeStatus = CommandSkipped,
            runtimeExitCode = Nothing,
            runtimeStdoutPath = Nothing,
            runtimeStderrPath = Nothing,
            runtimeErrorMessage = Just "translation did not succeed"
          }

runtimeProcess :: ResolvedTools -> [(String, String)] -> ShellRunMode -> Path Abs File -> [Text] -> CreateProcess
runtimeProcess tools processEnv runMode scriptPath args =
  case runMode of
    ShellRunExec ->
      (proc (toFilePath (toolsFishPath tools)) ("--no-config" : toFilePath scriptPath : map toString args)) {env = Just processEnv}
    ShellRunAuto ->
      runtimeProcess tools processEnv ShellRunSource scriptPath args
    _ ->
      (proc (toFilePath (toolsFishPath tools)) ["--no-config", "-c", T.unpack sourceCommand]) {env = Just processEnv}
  where
    quotedArgs = T.intercalate " " (map quoteArg args)
    sourceCommand =
      if null args
        then "source " <> quoteArg (toText (toFilePath scriptPath))
        else "source " <> quoteArg (toText (toFilePath scriptPath)) <> " " <> quotedArgs

runRuntimeBenchmarkEntry ::
  RuntimeShell ->
  Path Abs File ->
  Int ->
  [(String, String)] ->
  RuntimeBenchmarkEntry ->
  IO Bool
runRuntimeBenchmarkEntry runtimeShell fishPath timeoutSeconds processEnv entry = do
  exists <- PathIO.doesFileExist selectedPath
  if not exists
    then pure False
    else do
      attempted <-
        try
          ( runProcessText
              (Just timeoutSeconds)
              (benchmarkProcess runtimeShell fishPath processEnv entry)
              (runtimeBenchmarkStdin entry)
          ) ::
          IO (Either IOException (Maybe ProcessOutput))
      pure $
        case attempted of
          Right (Just _) -> True
          _ -> False
  where
    selectedPath =
      case runtimeShell of
        RuntimeBash -> runtimeBenchmarkBashPath entry
        RuntimeFish -> runtimeBenchmarkFishPath entry

benchmarkProcess :: RuntimeShell -> Path Abs File -> [(String, String)] -> RuntimeBenchmarkEntry -> CreateProcess
benchmarkProcess runtimeShell fishPath processEnv entry =
  case (runtimeShell, runtimeBenchmarkMode entry) of
    (RuntimeBash, ShellRunExec) ->
      withEnv (proc "bash" (scriptPath : args))
    (RuntimeFish, ShellRunExec) ->
      withEnv (proc (toFilePath fishPath) ("--no-config" : scriptPath : args))
    (RuntimeBash, _) ->
      withEnv (proc "bash" ["-c", T.unpack sourceCommand])
    (RuntimeFish, _) ->
      withEnv (proc (toFilePath fishPath) ["--no-config", "-c", T.unpack sourceCommand])
  where
    selectedPath =
      case runtimeShell of
        RuntimeBash -> runtimeBenchmarkBashPath entry
        RuntimeFish -> runtimeBenchmarkFishPath entry
    scriptPath = toFilePath selectedPath
    args = map toString (runtimeBenchmarkArgs entry)
    quotedArgs = T.intercalate " " (map quoteArg (runtimeBenchmarkArgs entry))
    sourceCommand =
      if null args
        then "source " <> quoteArg (toText scriptPath)
        else "source " <> quoteArg (toText scriptPath) <> " " <> quotedArgs
    withEnv process = process {env = Just processEnv}

quoteArg :: Text -> Text
quoteArg txt =
  "'" <> T.replace "'" "'\\''" txt <> "'"

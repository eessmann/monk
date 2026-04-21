module Bakeoff.Execution.Runtime
  ( buildRuntimeReport,
  )
where

import Data.Text qualified as T
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
import Path
  ( Abs,
    File,
    Path,
    toFilePath,
  )
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

quoteArg :: Text -> Text
quoteArg txt =
  "'" <> T.replace "'" "'\\''" txt <> "'"

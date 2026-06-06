{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Monk.Internal.Shell
  ( Shell (..),
    ShellRunMode (..),
    ShellRunTimeout (..),
    RunResult (..),
    EnvDelta (..),
    shouldRunIntegration,
    prepareEnv,
    readCreateProcessWithTimeout,
    runShell,
    runShellWith,
    runShellWithMode,
    runShellFileWithMode,
    diffEnv,
  )
where

import Control.Exception (bracket, throwIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isAlpha, toLower)
import Data.List (elemIndices)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Path (Abs, File, Path, parseRelFile, toFilePath)
import Path.IO qualified as PathIO
import System.Environment qualified as Env
import System.Exit (ExitCode)
import System.IO qualified as IO
import System.Process (CreateProcess (env), proc, readCreateProcessWithExitCode)
import System.Timeout qualified as Timeout

data Shell
  = ShellBash
  | ShellFish
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ShellRunMode
  = ShellRunAuto
  | ShellRunSource
  | ShellRunExec
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype ShellRunTimeout = MkShellRunTimeout Int
  deriving stock (Eq, Show)

instance Exception ShellRunTimeout where
  displayException (MkShellRunTimeout timeoutMicros) =
    "shell command timed out after " <> showSeconds timeoutMicros <> "s"
    where
      showSeconds micros =
        show (fromIntegral micros / (1000000 :: Double) :: Double)

data RunResult = MkRunResult
  { rrExit :: ExitCode,
    rrStdout :: Text,
    rrStderr :: Text,
    rrEnv :: Map.Map Text Text
  }
  deriving stock (Eq, Show)

data EnvDelta = MkEnvDelta
  { envAddedOrChanged :: Map.Map Text Text,
    envRemoved :: Set.Set Text
  }
  deriving stock (Eq, Show)

shouldRunIntegration :: IO (Either String ())
shouldRunIntegration = do
  enabled <- fmap isEnabled (Env.lookupEnv "MONK_INTEGRATION")
  if not enabled
    then pure (Left "MONK_INTEGRATION not set")
    else do
      bashOk <- findExecutablePath "bash"
      fishOk <- findExecutablePath "fish"
      pure $ case (bashOk, fishOk) of
        (Nothing, _) -> Left "bash not found in PATH"
        (_, Nothing) -> Left "fish not found in PATH"
        _ -> Right ()
  where
    isEnabled = \case
      Nothing -> False
      Just raw ->
        let val = map toLower raw
         in val `elem` ["1", "true", "yes", "on"]

prepareEnv :: IO [(String, String)]
prepareEnv = do
  setVars [("LC_ALL", "C"), ("LANG", "C")] <$> Env.getEnvironment
  where
    setVars vars env0 = foldl' (\acc (k, v) -> (k, v) : filter ((/= k) . fst) acc) env0 vars

runShell :: Shell -> [(String, String)] -> Text -> IO RunResult
runShell shell env0 script = runShellWith shell env0 script [] ""

runShellWith :: Shell -> [(String, String)] -> Text -> [Text] -> Text -> IO RunResult
runShellWith = runShellWithMode ShellRunAuto

runShellWithMode :: ShellRunMode -> Shell -> [(String, String)] -> Text -> [Text] -> Text -> IO RunResult
runShellWithMode runMode shell env0 script args stdinInput =
  withTempScript script $ \scriptPath ->
    runShellFileWithMode' (effectiveRunMode runMode script) shell env0 scriptPath args stdinInput

runShellFileWithMode :: ShellRunMode -> Shell -> [(String, String)] -> Path Abs File -> [Text] -> Text -> IO RunResult
runShellFileWithMode =
  runShellFileWithMode'

runShellFileWithMode' :: ShellRunMode -> Shell -> [(String, String)] -> Path Abs File -> [Text] -> Text -> IO RunResult
runShellFileWithMode' runMode shell env0 scriptPath args stdinInput = do
  let wrapped = wrapScriptPath runMode shell scriptPath args
      (cmd, cmdArgs) = shellCommand shell wrapped
      process = (proc cmd cmdArgs) {env = Just env0}
  (exitCode, out, err) <- readCreateProcessWithTimeout shellRunTimeoutMicros process (T.unpack stdinInput)
  let (stdoutPart, envPart) = splitEnv marker (T.pack out)
  pure
    MkRunResult
      { rrExit = exitCode,
        rrStdout = stdoutPart,
        rrStderr = T.pack err,
        rrEnv = parseEnv envPart
      }

shellRunTimeoutMicros :: Int
shellRunTimeoutMicros = 5 * 1000 * 1000

readCreateProcessWithTimeout :: Int -> CreateProcess -> String -> IO (ExitCode, String, String)
readCreateProcessWithTimeout timeoutMicros process stdinInput =
  Timeout.timeout timeoutMicros (readCreateProcessWithExitCode process stdinInput) >>= \case
    Just result -> pure result
    Nothing -> throwIO (MkShellRunTimeout timeoutMicros)

withTempScript :: Text -> (Path Abs File -> IO a) -> IO a
withTempScript script action = do
  tmpDir <- PathIO.getTempDir
  bracket (IO.openTempFile (toFilePath tmpDir) "monk-script-") cleanup $ \(path, handle) -> do
    TIO.hPutStr handle script
    IO.hFlush handle
    action =<< PathIO.resolveFile' path
  where
    cleanup (path, handle) = do
      IO.hClose handle
      PathIO.removeFile =<< PathIO.resolveFile' path

shellCommand :: Shell -> Text -> (FilePath, [String])
shellCommand shell script =
  case shell of
    ShellBash -> ("bash", ["-c", T.unpack script])
    ShellFish -> ("fish", ["--no-config", "-c", T.unpack script])

marker :: Text
marker = "__MONK_ENV_BEGIN__"

wrapScriptPath :: ShellRunMode -> Shell -> Path Abs File -> [Text] -> Text
wrapScriptPath runMode shell scriptPath args =
  let markerLine = "printf '\\n%s\\n' '" <> marker <> "'"
      (statusLine, exitLine) =
        case shell of
          ShellBash -> ("monk_status=$?", "exit $monk_status")
          ShellFish -> ("set -l monk_status $status", "exit $monk_status")
      bodyLines = scriptPathLines runMode shell scriptPath args
      footer = [statusLine, markerLine, "env", exitLine]
   in T.intercalate "\n" (bodyLines <> footer)

scriptPathLines :: ShellRunMode -> Shell -> Path Abs File -> [Text] -> [Text]
scriptPathLines runMode shell scriptPath args =
  let pathText = T.pack (toFilePath scriptPath)
      argsText = T.intercalate " " (map quoteArg args)
      hasArgs = not (null args)
      runChild cmd =
        if hasArgs then cmd <> " " <> argsText else cmd
   in case shell of
        ShellBash ->
          case runMode of
            ShellRunExec -> [runChild ("bash " <> quoteArg pathText)]
            _ -> [runChild ("source " <> quoteArg pathText)]
        ShellFish ->
          case runMode of
            ShellRunExec -> [runChild ("fish --no-config " <> quoteArg pathText)]
            _ -> [runChild ("source " <> quoteArg pathText)]

quoteArg :: Text -> Text
quoteArg txt =
  "'" <> T.replace "'" "'\\''" txt <> "'"

effectiveRunMode :: ShellRunMode -> Text -> ShellRunMode
effectiveRunMode runMode script =
  case runMode of
    ShellRunAuto ->
      if scriptMayExit script
        then ShellRunExec
        else ShellRunSource
    mode -> mode

scriptMayExit :: Text -> Bool
scriptMayExit script =
  any ((`elem` ["exit", "exec"]) . normalizeToken) (T.words script)
  where
    normalizeToken = T.takeWhile isAlpha . T.dropWhile (not . isAlpha)

splitEnv :: Text -> Text -> (Text, Text)
splitEnv markerText output =
  let ls = T.splitOn "\n" output
      idxs = elemIndices markerText ls
   in case NE.nonEmpty idxs of
        Nothing -> (output, "")
        Just neIdxs ->
          let idx = NE.last neIdxs
              outLines = take idx ls
              envLines = drop (idx + 1) ls
           in (T.intercalate "\n" outLines, T.intercalate "\n" envLines)

parseEnv :: Text -> Map.Map Text Text
parseEnv = Map.fromList . mapMaybe parseLine . filter (not . T.null) . T.lines
  where
    parseLine line =
      case T.breakOn "=" line of
        (key, rest) | not (T.null rest) -> Just (key, T.drop 1 rest)
        _ -> Nothing

diffEnv :: Map.Map Text Text -> Map.Map Text Text -> EnvDelta
diffEnv baseEnv newEnv =
  let ignored = Set.fromList ["OLDPWD", "_"]
      stripIgnored = Map.filterWithKey (\k _ -> not (Set.member k ignored))
      baseFiltered = stripIgnored baseEnv
      newFiltered = stripIgnored newEnv
   in MkEnvDelta
        { envAddedOrChanged =
            Map.differenceWith
              (\newVal oldVal -> if newVal == oldVal then Nothing else Just newVal)
              newFiltered
              baseFiltered,
          envRemoved = Map.keysSet (Map.difference baseFiltered newFiltered)
        }

findExecutablePath :: FilePath -> IO (Maybe (Path Abs File))
findExecutablePath toolName = do
  relTool <- parseRelFile toolName
  PathIO.findExecutable relTool

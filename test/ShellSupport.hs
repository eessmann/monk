{-# LANGUAGE OverloadedStrings #-}

module ShellSupport
  ( Shell (..),
    RunResult (..),
    EnvDelta (..),
    shouldRunIntegration,
    prepareEnv,
    runShell,
    runShellWith,
    diffEnv,
  )
where

import Control.Exception (bracket)
import Data.Char (isAlpha, toLower)
import Data.List (findIndices)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (findExecutable, getTemporaryDirectory, removeFile)
import System.Environment qualified as Env
import System.Exit (ExitCode)
import System.IO qualified as IO
import System.Process (CreateProcess (env), proc, readCreateProcessWithExitCode)

data Shell
  = ShellBash
  | ShellFish
  deriving stock (Eq, Show)

data RunResult = RunResult
  { rrExit :: ExitCode,
    rrStdout :: Text,
    rrStderr :: Text,
    rrEnv :: Map.Map Text Text
  }
  deriving stock (Eq, Show)

data EnvDelta = EnvDelta
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
      bashOk <- findExecutable "bash"
      fishOk <- findExecutable "fish"
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
  env <- Env.getEnvironment
  pure (setVars [("LC_ALL", "C"), ("LANG", "C")] env)
  where
    setVars vars env0 = foldl' (\acc (k, v) -> (k, v) : filter ((/= k) . fst) acc) env0 vars

runShell :: Shell -> [(String, String)] -> Text -> IO RunResult
runShell shell env0 script = runShellWith shell env0 script [] ""

runShellWith :: Shell -> [(String, String)] -> Text -> [Text] -> Text -> IO RunResult
runShellWith shell env0 script args stdinInput = do
  withTempScript script $ \scriptPath -> do
    let wrapped = wrapScript shell script args scriptPath
        (cmd, cmdArgs) = shellCommand shell wrapped
        process = (proc cmd cmdArgs) {env = Just env0}
    (exitCode, out, err) <- readCreateProcessWithExitCode process (T.unpack stdinInput)
    let (stdoutPart, envPart) = splitEnv marker (T.pack out)
    pure
      RunResult
        { rrExit = exitCode,
          rrStdout = stdoutPart,
          rrStderr = T.pack err,
          rrEnv = parseEnv envPart
        }

withTempScript :: Text -> (FilePath -> IO a) -> IO a
withTempScript script action = do
  tmpDir <- getTemporaryDirectory
  bracket (IO.openTempFile tmpDir "monk-script-") cleanup $ \(path, handle) -> do
    TIO.hPutStr handle script
    IO.hFlush handle
    action path
  where
    cleanup (path, handle) = do
      IO.hClose handle
      removeFile path

shellCommand :: Shell -> Text -> (FilePath, [String])
shellCommand shell script =
  case shell of
    ShellBash -> ("bash", ["-c", T.unpack script])
    ShellFish -> ("fish", ["--no-config", "-c", T.unpack script])

marker :: Text
marker = "__MONK_ENV_BEGIN__"

wrapScript :: Shell -> Text -> [Text] -> FilePath -> Text
wrapScript shell script args scriptPath =
  let markerLine = "printf '\\n%s\\n' '" <> marker <> "'"
      (statusLine, exitLine) =
        case shell of
          ShellBash -> ("monk_status=$?", "exit $monk_status")
          ShellFish -> ("set -l monk_status $status", "exit $monk_status")
      bodyLines = scriptLines shell script args scriptPath
      footer = [statusLine, markerLine, "env", exitLine]
   in T.intercalate "\n" (bodyLines <> footer)

scriptLines :: Shell -> Text -> [Text] -> FilePath -> [Text]
scriptLines shell script args scriptPath =
  let pathText = T.pack scriptPath
      argsText = T.intercalate " " (map quoteArg args)
      hasArgs = not (null args)
      runChild cmd =
        if hasArgs then cmd <> " " <> argsText else cmd
   in case shell of
        ShellBash ->
          if scriptMayExit script
            then [runChild ("bash " <> quoteArg pathText)]
            else [runChild ("source " <> quoteArg pathText)]
        ShellFish ->
          if scriptMayExit script
            then [runChild ("fish --no-config " <> quoteArg pathText)]
            else [runChild ("source " <> quoteArg pathText)]

quoteArg :: Text -> Text
quoteArg txt =
  "'" <> T.replace "'" "'\\''" txt <> "'"

scriptMayExit :: Text -> Bool
scriptMayExit script =
  any (`elem` ["exit", "exec"]) (map normalizeToken (T.words script))
  where
    normalizeToken = T.takeWhile isAlpha . T.dropWhile (not . isAlpha)

splitEnv :: Text -> Text -> (Text, Text)
splitEnv markerText output =
  let ls = T.splitOn "\n" output
      idxs = findIndices (== markerText) ls
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

-- Only compare deltas to avoid shell-specific baseline differences.
diffEnv :: Map.Map Text Text -> Map.Map Text Text -> EnvDelta
diffEnv baseEnv newEnv =
  let ignored = Set.fromList ["OLDPWD"]
      stripIgnored = Map.filterWithKey (\k _ -> not (Set.member k ignored))
      baseFiltered = stripIgnored baseEnv
      newFiltered = stripIgnored newEnv
   in EnvDelta
        { envAddedOrChanged =
            Map.differenceWith
              (\newVal oldVal -> if newVal == oldVal then Nothing else Just newVal)
              newFiltered
              baseFiltered,
          envRemoved = Map.keysSet (Map.difference baseFiltered newFiltered)
        }

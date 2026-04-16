module Bakeoff.Tools
  ( resolveTools,
    collectGitMetadata,
    configReport,
  )
where

import Data.Text qualified as T
import Bakeoff.Process (ProcessOutput (..), runProcessText)
import Bakeoff.Selection (defaultGroups)
import Bakeoff.Types
import Path (Abs, Dir, File, Path, parseRelFile, toFilePath)
import Path.IO qualified as PathIO
import System.Exit (ExitCode (..))
import System.Process (proc)

resolveTools :: Path Abs File -> BakeoffConfig -> IO ResolvedTools
resolveTools monkExecutable cfg = do
  babelfishPath <- resolveRequiredTool (bakeoffBabelfishPathHint cfg) "babelfish"
  fishPath <- resolveRequiredTool (bakeoffFishPathHint cfg) "fish"
  hyperfinePath <- resolveOptionalTool (bakeoffHyperfinePathHint cfg) "hyperfine"
  babelfishVersion <- resolveBabelfishVersion babelfishPath (bakeoffBabelfishVersionOverride cfg)
  fishVersion <- toolVersion fishPath ["--version"] "unknown"
  hyperfineVersion <- traverse (\path -> toolVersion path ["--version"] "unknown") hyperfinePath
  pure
    MkResolvedTools
      { toolsMonkExecutable = monkExecutable,
        toolsBabelfishPath = babelfishPath,
        toolsFishPath = fishPath,
        toolsHyperfinePath = hyperfinePath,
        toolsBabelfishVersion = babelfishVersion,
        toolsFishVersion = fishVersion,
        toolsHyperfineVersion = hyperfineVersion
      }

resolveRequiredTool :: Maybe (Path Abs File) -> String -> IO (Path Abs File)
resolveRequiredTool hint toolName =
  case hint of
    Just path -> validateResolved path
    Nothing -> do
      mPath <- findExecutablePath toolName
      case mPath of
        Just path -> pure path
        Nothing -> fail ("tool not found: " <> toolName)
  where
    validateResolved path = do
      exists <- PathIO.doesFileExist path
      if exists
        then pure path
        else fail ("tool not found: " <> toFilePath path)

resolveOptionalTool :: Maybe (Path Abs File) -> String -> IO (Maybe (Path Abs File))
resolveOptionalTool hint toolName =
  case hint of
    Just path -> Just <$> resolveRequiredTool (Just path) toolName
    Nothing -> findExecutablePath toolName

resolveBabelfishVersion :: Path Abs File -> Maybe Text -> IO Text
resolveBabelfishVersion _ (Just version) = pure version
resolveBabelfishVersion babelfishPath Nothing = do
  version <- toolVersionMaybe babelfishPath ["--version"]
  case version of
    Just resolved -> pure resolved
    Nothing -> do
      brew <- findExecutablePath "brew"
      case brew of
        Nothing -> pure "unknown"
        Just brewPath -> do
          brewVersion <- toolVersionMaybe brewPath ["list", "--versions", "babelfish"]
          pure (fromMaybe "unknown" (extractBrewVersion =<< brewVersion))

toolVersion :: Path Abs File -> [String] -> Text -> IO Text
toolVersion path args fallback =
  fromMaybe fallback <$> toolVersionMaybe path args

toolVersionMaybe :: Path Abs File -> [String] -> IO (Maybe Text)
toolVersionMaybe path args = do
  result <- runProcessText (Just 5) (proc (toFilePath path) args) ""
  pure $
    case result of
      Just MkProcessOutput {poExitCode = ExitSuccess, poStdout, poStderr} ->
        firstNonEmptyLine [poStdout, poStderr]
      _ -> Nothing

firstNonEmptyLine :: [Text] -> Maybe Text
firstNonEmptyLine =
  listToMaybe
    . map T.strip
    . filter (not . T.null . T.strip)
    . concatMap T.lines

extractBrewVersion :: Text -> Maybe Text
extractBrewVersion raw =
  case T.words raw of
    (_ : version : _) -> Just version
    _ -> Nothing

collectGitMetadata :: Path Abs Dir -> IO GitMetadata
collectGitMetadata cwd = do
  sha <- gitOutput cwd ["rev-parse", "HEAD"]
  dirtyOutput <- runProcessText (Just 5) (proc "git" ["-C", toFilePath cwd, "diff", "--quiet", "--ignore-submodules", "--"]) ""
  let dirty =
        case dirtyOutput of
          Just MkProcessOutput {poExitCode = ExitSuccess} -> False
          Just MkProcessOutput {poExitCode = ExitFailure 1} -> True
          _ -> False
  pure
    MkGitMetadata
      { gitSha = sha,
        gitDirty = dirty
      }

gitOutput :: Path Abs Dir -> [String] -> IO (Maybe Text)
gitOutput cwd args = do
  result <- runProcessText (Just 5) (proc "git" ("-C" : toFilePath cwd : args)) ""
  pure $
    case result of
      Just MkProcessOutput {poExitCode = ExitSuccess, poStdout} ->
        firstNonEmptyLine [poStdout]
      _ -> Nothing

configReport :: BakeoffConfig -> ConfigReport
configReport cfg =
  MkConfigReport
    { configTranslationTimeoutSeconds = bakeoffTranslationTimeoutSeconds cfg,
      configRuntimeTimeoutSeconds = bakeoffRuntimeTimeoutSeconds cfg,
      configBenchmarksEnabled = bakeoffBenchmarksEnabled cfg,
      configHyperfineRuns = bakeoffHyperfineRuns cfg,
      configHyperfineWarmup = bakeoffHyperfineWarmup cfg,
      configJobs = bakeoffJobs cfg,
      configGroups = if null (bakeoffGroups cfg) then defaultGroups else bakeoffGroups cfg,
      configFiles = bakeoffFiles cfg,
      configFileLists = bakeoffFileLists cfg,
      configCompatibleFileLists = bakeoffCompatibleFileLists cfg
    }

findExecutablePath :: FilePath -> IO (Maybe (Path Abs File))
findExecutablePath toolName = do
  relTool <- parseRelFile toolName
  PathIO.findExecutable relTool

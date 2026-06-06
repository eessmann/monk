module Bakeoff.Tools
  ( ToolPreflightFailure (..),
    ToolPreflightIssue (..),
    resolveTools,
    toolPreflightWarnings,
    renderToolPreflightFailure,
    collectGitMetadata,
    configReport,
  )
where

import Bakeoff.Process (ProcessOutput (..), runProcessText)
import Bakeoff.Selection (defaultGroups)
import Bakeoff.Types
import Data.Text qualified as T
import Path (Abs, Dir, File, Path, parseRelFile, toFilePath)
import Path.IO qualified as PathIO
import System.Exit (ExitCode (..))
import System.Process (proc)

data ToolPreflightIssue = MkToolPreflightIssue
  { preflightToolName :: Text,
    preflightMessage :: Text,
    preflightAction :: Text
  }
  deriving stock (Eq, Show)

newtype ToolPreflightFailure = MkToolPreflightFailure
  { preflightIssues :: [ToolPreflightIssue]
  }
  deriving stock (Eq, Show)

resolveTools :: Path Abs File -> BakeoffConfig -> IO (Either ToolPreflightFailure ResolvedTools)
resolveTools monkExecutable cfg = do
  babelfishE <- resolveRequiredTool (bakeoffBabelfishPathHint cfg) "babelfish"
  fishE <- resolveRequiredTool (bakeoffFishPathHint cfg) "fish"
  hyperfineE <- resolveOptionalTool (bakeoffHyperfinePathHint cfg) "hyperfine"
  let issues =
        catMaybes
          [ either Just (const Nothing) babelfishE,
            either Just (const Nothing) fishE,
            either Just (const Nothing) hyperfineE
          ]
  case (babelfishE, fishE, hyperfineE, issues) of
    (Right babelfishPath, Right fishPath, Right hyperfinePath, []) -> do
      babelfishVersion <- resolveBabelfishVersion babelfishPath (bakeoffBabelfishVersionOverride cfg)
      fishVersion <- toolVersion fishPath ["--version"] (MkToolVersion "unknown")
      hyperfineVersion <- traverse (\path -> toolVersion path ["--version"] (MkToolVersion "unknown")) hyperfinePath
      pure . Right $
        MkResolvedTools
          { toolsMonkExecutable = monkExecutable,
            toolsBabelfishPath = babelfishPath,
            toolsFishPath = fishPath,
            toolsHyperfinePath = hyperfinePath,
            toolsBabelfishVersion = babelfishVersion,
            toolsFishVersion = fishVersion,
            toolsHyperfineVersion = hyperfineVersion
          }
    (_, _, _, preflightIssues) ->
      pure (Left (MkToolPreflightFailure preflightIssues))

toolPreflightWarnings :: BakeoffConfig -> ResolvedTools -> [Text]
toolPreflightWarnings cfg tools
  | bakeoffBenchmarksEnabled cfg && isNothing (toolsHyperfinePath tools) =
      [ "hyperfine was not found, so benchmark runs will be skipped. Install hyperfine or rerun with --no-benchmark."
      ]
  | otherwise = []

renderToolPreflightFailure :: ToolPreflightFailure -> Text
renderToolPreflightFailure (MkToolPreflightFailure issues) =
  T.unlines $
    [ "Bake-off tool preflight failed.",
      ""
    ]
      <> map renderIssue issues
      <> [ "",
           "Install the missing tools or pass the explicit path flags above, then rerun the bake-off."
         ]
  where
    renderIssue issue =
      "- "
        <> preflightToolName issue
        <> ": "
        <> preflightMessage issue
        <> " "
        <> preflightAction issue

resolveRequiredTool :: Maybe (Path Abs File) -> String -> IO (Either ToolPreflightIssue (Path Abs File))
resolveRequiredTool hint toolName =
  case hint of
    Just path -> validateResolved path
    Nothing -> do
      mPath <- findExecutablePath toolName
      case mPath of
        Just path -> pure (Right path)
        Nothing -> pure (Left (missingPathIssue toolName))
  where
    validateResolved path = do
      exists <- PathIO.doesFileExist path
      if exists
        then pure (Right path)
        else pure (Left (invalidHintIssue toolName path))

resolveOptionalTool :: Maybe (Path Abs File) -> String -> IO (Either ToolPreflightIssue (Maybe (Path Abs File)))
resolveOptionalTool hint toolName =
  case hint of
    Just path ->
      fmap Just <$> resolveRequiredTool (Just path) toolName
    Nothing ->
      Right <$> findExecutablePath toolName

resolveBabelfishVersion :: Path Abs File -> Maybe Text -> IO ToolVersion
resolveBabelfishVersion _ (Just version) = pure (MkToolVersion version)
resolveBabelfishVersion babelfishPath Nothing = do
  version <- toolVersionMaybe babelfishPath ["--version"]
  case version of
    Just resolved -> pure resolved
    Nothing -> do
      brew <- findExecutablePath "brew"
      case brew of
        Nothing -> pure (MkToolVersion "unknown")
        Just brewPath -> do
          brewVersion <- toolVersionMaybe brewPath ["list", "--versions", "babelfish"]
          pure $
            case brewVersion >>= (extractBrewVersion . unToolVersion) of
              Just resolved -> MkToolVersion resolved
              Nothing -> MkToolVersion "unknown"

toolVersion :: Path Abs File -> [String] -> ToolVersion -> IO ToolVersion
toolVersion path args fallback =
  fromMaybe fallback <$> toolVersionMaybe path args

toolVersionMaybe :: Path Abs File -> [String] -> IO (Maybe ToolVersion)
toolVersionMaybe path args = do
  result <- runProcessText (Just 5) (proc (toFilePath path) args) ""
  pure $
    case result of
      Just MkProcessOutput {poExitCode = ExitSuccess, poStdout, poStderr} ->
        MkToolVersion <$> firstNonEmptyLine [poStdout, poStderr]
      _ -> Nothing

missingPathIssue :: String -> ToolPreflightIssue
missingPathIssue toolName =
  MkToolPreflightIssue
    { preflightToolName = toText toolName,
      preflightMessage = "not found on PATH.",
      preflightAction = "Install it or pass " <> quotedFlag toolName <> " /path/to/" <> toText toolName <> "."
    }

invalidHintIssue :: String -> Path Abs File -> ToolPreflightIssue
invalidHintIssue toolName path =
  MkToolPreflightIssue
    { preflightToolName = toText toolName,
      preflightMessage = "configured path does not exist: `" <> toText (toFilePath path) <> "`.",
      preflightAction = "Fix that path or pass " <> quotedFlag toolName <> " /path/to/" <> toText toolName <> "."
    }

quotedFlag :: String -> Text
quotedFlag toolName =
  "`" <> flagName <> "`"
  where
    flagName =
      case toolName of
        "babelfish" -> "--babelfish"
        "fish" -> "--fish"
        "hyperfine" -> "--hyperfine"
        other -> "--" <> toText other

firstNonEmptyLine :: [Text] -> Maybe Text
firstNonEmptyLine =
  listToMaybe
    . map T.strip
    . concatMap (filter (not . T.null . T.strip) . T.lines)

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

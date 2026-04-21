module Bakeoff.Selection
  ( defaultGroups,
    discoverFixtures,
    resolveFixtureSelection,
    summarizeFixtureMetadata,
    makeFixtureSelectionReport,
    fixtureArtifactDir,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Bakeoff.Fixture (FixtureMetadata (..), loadFixtureMetadata)
import Bakeoff.Types
import Path (Abs, Dir, File, Path, Rel, parseRelDir, parseRelFile, stripProperPrefix, toFilePath, (</>))
import Path.IO qualified as PathIO
import System.FilePath qualified as FP
import System.Info qualified as SysInfo

defaultGroups :: [FixtureGroup]
defaultGroups =
  [ FixtureGroupCorpus,
    FixtureGroupBenchmark,
    FixtureGroupIntegration,
    FixtureGroupGolden,
    FixtureGroupRealWorld
  ]

discoverFixtures :: Path Abs Dir -> IO (M.Map FixtureGroup [Path Abs File])
discoverFixtures cwd =
  M.fromList <$> traverse discoverFixtureGroup defaultGroups
  where
    discoverFixtureGroup fixtureGroup = do
      files <- listGroupFixtures cwd fixtureGroup
      pure (fixtureGroup, files)

resolveFixtureSelection ::
  Path Abs Dir ->
  [FixtureGroup] ->
  [Path Abs File] ->
  [Path Abs File] ->
  [Path Abs File] ->
  IO [FixtureSpec]
resolveFixtureSelection cwd groups files fileLists compatibleLists = do
  discovered <- discoverFixtures cwd
  let selectedByGroup =
        if not selectorsPresent
          then groupSelections discovered defaultGroups SelectionDefault
          else
            groupSelections
              discovered
              (if null groups then [] else expandGroups groups)
              SelectionGroup
  selectedByFile <- traverse (\path -> mkExplicitSelection cwd (SelectionFile path) path) files
  selectedByFileList <- concat <$> traverse (loadSelectionFile cwd SelectionFileList) fileLists
  selectedByCompatible <- concat <$> traverse (loadSelectionFile cwd SelectionCompatible) compatibleLists
  let merged = foldl' insertSelection M.empty (selectedByGroup <> selectedByFile <> selectedByFileList <> selectedByCompatible)
  traverse (finalizeSelection cwd) (sortOn fst (M.toList merged))
  where
    selectorsPresent =
      not (null groups && null files && null fileLists && null compatibleLists)
    groupSelections discovered requestedGroups mkSource =
      concat
        [ [ (path, (fixtureGroup, [mkSource fixtureGroup]))
            | path <- fromMaybe [] (M.lookup fixtureGroup discovered)
          ]
          | fixtureGroup <- requestedGroups
        ]

insertSelection ::
  M.Map (Path Abs File) (FixtureGroup, [SelectionSource]) ->
  (Path Abs File, (FixtureGroup, [SelectionSource])) ->
  M.Map (Path Abs File) (FixtureGroup, [SelectionSource])
insertSelection acc (path, (fixtureGroup, sources)) =
  M.insertWith merge path (fixtureGroup, ordNub sources) acc
  where
    merge (newGroup, newSources) (oldGroup, oldSources) =
      (preferGroup oldGroup newGroup, ordNub (oldSources <> newSources))

    preferGroup oldGroup newGroup =
      if oldGroup == FixtureGroupCustom then newGroup else oldGroup

finalizeSelection :: Path Abs Dir -> (Path Abs File, (FixtureGroup, [SelectionSource])) -> IO FixtureSpec
finalizeSelection cwd (path, (fixtureGroup, sources)) = do
  metadata <- loadFixtureMetadata path
  relativePath <- stripProperPrefix cwd path
  artifactDir <- fixtureArtifactDir relativePath
  skipReason <- determineSkip metadata
  pure
    MkFixtureSpec
      { specPath = path,
        specRelativePath = relativePath,
        specGroup = fixtureGroup,
        specMetadata = metadata,
        specSelectionSources = ordNub sources,
        specArtifactDir = artifactDir,
        specSkipReason = skipReason
      }
  where
    determineSkip metadata = do
      let platformSkip =
            case fmPlatforms metadata of
              Nothing -> Nothing
              Just allowed
                | toText SysInfo.os `elem` allowed -> Nothing
                | otherwise -> Just (SkipPlatformMismatch (toText SysInfo.os) allowed)
      case platformSkip of
        Just reason -> pure (Just reason)
        Nothing -> do
          missing <- filterM (fmap isNothing . findExecutablePath) (fmPrereqs metadata)
          pure $
            if null missing
              then Nothing
              else Just (SkipMissingPrereqs missing)

mkExplicitSelection ::
  Path Abs Dir ->
  SelectionSource ->
  Path Abs File ->
  IO (Path Abs File, (FixtureGroup, [SelectionSource]))
mkExplicitSelection cwd source path = do
  exists <- PathIO.doesFileExist path
  unless exists $
    fail ("fixture not found: " <> toFilePath path)
  pure (path, (classifyFixtureGroup cwd path, [source]))

loadSelectionFile ::
  Path Abs Dir ->
  (Path Abs File -> SelectionSource) ->
  Path Abs File ->
  IO [(Path Abs File, (FixtureGroup, [SelectionSource]))]
loadSelectionFile cwd mkSource path = do
  entries <- loadFileList path
  traverse (mkExplicitSelection cwd (mkSource path)) entries

expandGroups :: [FixtureGroup] -> [FixtureGroup]
expandGroups groups
  | null groups = defaultGroups
  | otherwise = ordNub (concatMap expand groups)
  where
    expand FixtureGroupCustom = []
    expand fixtureGroup = [fixtureGroup]

listGroupFixtures :: Path Abs Dir -> FixtureGroup -> IO [Path Abs File]
listGroupFixtures cwd fixtureGroup = do
  relDir <- groupDirectory fixtureGroup
  let dir = cwd </> relDir
  exists <- PathIO.doesDirExist dir
  if not exists
    then pure []
    else do
      (_, files) <- PathIO.listDir dir
      pure
        [ entry
          | entry <- sort files,
            FP.isExtensionOf ".bash" (toFilePath entry)
        ]

groupDirectory :: FixtureGroup -> IO (Path Rel Dir)
groupDirectory = \case
  FixtureGroupCorpus -> parseRelDir "test/fixtures/corpus/"
  FixtureGroupBenchmark -> parseRelDir "benchmark/fixtures/"
  FixtureGroupIntegration -> parseRelDir "test/fixtures/integration/"
  FixtureGroupGolden -> parseRelDir "test/fixtures/golden/"
  FixtureGroupRealWorld -> parseRelDir "test/fixtures/realworld/"
  FixtureGroupCustom -> parseRelDir "./"

classifyFixtureGroup :: Path Abs Dir -> Path Abs File -> FixtureGroup
classifyFixtureGroup cwd path =
  case stripProperPrefix cwd path :: Either SomeException (Path Rel File) of
    Right relPath ->
      case NE.nonEmpty (FP.splitDirectories (FP.normalise (toFilePath relPath))) of
        Just ("test" :| "fixtures" : "corpus" : _) -> FixtureGroupCorpus
        Just ("benchmark" :| "fixtures" : _) -> FixtureGroupBenchmark
        Just ("test" :| "fixtures" : "integration" : _) -> FixtureGroupIntegration
        Just ("test" :| "fixtures" : "golden" : _) -> FixtureGroupGolden
        Just ("test" :| "fixtures" : "realworld" : _) -> FixtureGroupRealWorld
        _ -> FixtureGroupCustom
    Left _ -> FixtureGroupCustom

fixtureArtifactDir :: Path Rel File -> IO (Path Rel Dir)
fixtureArtifactDir relativePath =
  parseRelDir (FP.joinPath ("fixtures" : safeSegments (FP.dropExtension (toFilePath relativePath))) <> "/")

safeSegments :: FilePath -> [FilePath]
safeSegments =
  map sanitize . FP.splitDirectories . FP.normalise
  where
    sanitize segment =
      case segment of
        "." -> "_"
        ".." -> "__up__"
        "" -> "_"
        other -> other

loadFileList :: Path Abs File -> IO [Path Abs File]
loadFileList path = do
  exists <- PathIO.doesFileExist path
  unless exists $
    fail ("fixture file list not found: " <> toFilePath path)
  contents <- TIO.readFile (toFilePath path)
  traverse resolveLine (filter isEntryLine (lines contents))
  where
    baseDir = FP.takeDirectory (toFilePath path)

    isEntryLine line =
      let stripped = T.strip line
       in not (T.null stripped) && not ("#" `T.isPrefixOf` stripped)
    resolveLine line =
      let raw = toString (T.strip line)
          candidate =
            if FP.isRelative raw
              then FP.combine baseDir raw
              else raw
       in PathIO.resolveFile' candidate

summarizeFixtureMetadata :: FixtureMetadata -> FixtureMetadataSummary
summarizeFixtureMetadata MkFixtureMetadata {..} =
  MkFixtureMetadataSummary
    { fixtureMetaArgs = fmArgs,
      fixtureMetaMode = fmMode,
      fixtureMetaPlatforms = fmPlatforms,
      fixtureMetaPrereqs = fmPrereqs,
      fixtureMetaRecursive = fmRecursive,
      fixtureMetaHasStdin = not (T.null fmStdin)
    }

makeFixtureSelectionReport :: FixtureSpec -> FixtureSelectionReport
makeFixtureSelectionReport fixture =
  MkFixtureSelectionReport
    { selectionPath = specPath fixture,
      selectionRelativePath = specRelativePath fixture,
      selectionGroup = specGroup fixture,
      selectionSources = specSelectionSources fixture,
      selectionArtifactDir = specArtifactDir fixture,
      selectionSkipReason = specSkipReason fixture
    }

findExecutablePath :: Text -> IO (Maybe (Path Abs File))
findExecutablePath toolName = do
  relTool <- parseRelFile (toString toolName)
  PathIO.findExecutable relTool

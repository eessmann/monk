{-# LANGUAGE OverloadedStrings #-}

module Monk.Internal.Fixture
  ( FixtureMetadata (..),
    loadFixtureMetadata,
    loadFixtureArgs,
    loadFixtureMode,
    loadFixturePlatforms,
    loadFixtureStdin,
    loadFixturePrereqs,
    loadFixtureRecursive,
  )
where

import Data.Aeson (FromJSON (..), eitherDecodeFileStrict', withObject, (.!=), (.:), (.:?))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Monk.Internal.Shell (ShellRunMode (..))
import Path (Abs, File, Path, parseAbsFile, splitExtension, toFilePath)
import Path.IO qualified as PathIO

data FixtureMetadata = MkFixtureMetadata
  { fmArgs :: [Text],
    fmMode :: ShellRunMode,
    fmPlatforms :: Maybe [Text],
    fmStdin :: Text,
    fmPrereqs :: [Text],
    fmRecursive :: Bool
  }
  deriving stock (Eq, Show)

-- | A versioned sidecar owns the whole fixture. Mixing formats is rejected so
-- a misspelled or stale legacy field cannot silently change execution.
loadFixtureMetadata :: Path Abs File -> IO FixtureMetadata
loadFixtureMetadata path = do
  jsonPath <- sidecarPath path "fixture.json"
  exists <- PathIO.doesFileExist jsonPath
  if exists
    then do
      legacy <- filterM (sidecarPath path >=> PathIO.doesFileExist) legacyExtensions
      unless (null legacy) (fail ("fixture JSON conflicts with legacy sidecars: " <> intercalate ", " legacy))
      decoded <- eitherDecodeFileStrict' (toFilePath jsonPath)
      either (fail . ((toFilePath jsonPath <> ": ") <>)) pure decoded
    else loadLegacyMetadata path

instance FromJSON FixtureMetadata where
  parseJSON = withObject "fixture metadata" $ \fields -> do
    let allowed = ["version", "args", "mode", "platforms", "stdin", "prerequisites", "recursive"]
        unknown = filter (`notElem` allowed) (KeyMap.keys fields)
    unless (null unknown) (fail ("unknown fixture metadata fields: " <> show unknown))
    version <- fields .: "version"
    unless (version == (1 :: Int)) (fail "unsupported fixture metadata version")
    args <- fields .:? "args" .!= []
    when (any (T.any (== '\0')) args) (fail "fixture arguments cannot contain NUL")
    rawMode <- fields .:? "mode" .!= "source"
    mode <- either fail pure (parseMode rawMode)
    MkFixtureMetadata args mode
      <$> fields .:? "platforms"
      <*> fields .:? "stdin" .!= ""
      <*> fields .:? "prerequisites" .!= []
      <*> fields .:? "recursive" .!= False

legacyExtensions :: [String]
legacyExtensions = ["args", "mode", "platforms", "stdin", "prereqs", "recursive"]

parseMode :: Text -> Either String ShellRunMode
parseMode "exec" = Right ShellRunExec
parseMode "source" = Right ShellRunSource
parseMode other = Left ("unknown fixture mode: " <> toString other)

loadLegacyMetadata :: Path Abs File -> IO FixtureMetadata
loadLegacyMetadata path = do
  args <- maybe [] T.words <$> legacyText "args"
  mode <- maybe (pure ShellRunSource) (either fail pure . parseMode . T.strip) =<< legacyText "mode"
  platforms <- fmap T.words <$> legacyText "platforms"
  input <- fromMaybe "" <$> legacyText "stdin"
  prereqs <- maybe [] (filter (not . T.null) . map T.strip . T.lines) <$> legacyText "prereqs"
  recursive <- sidecarPath path "recursive" >>= PathIO.doesFileExist
  pure (MkFixtureMetadata args mode platforms input prereqs recursive)
  where
    legacyText extension = do
      sidecar <- sidecarPath path extension
      exists <- PathIO.doesFileExist sidecar
      if exists then Just <$> TIO.readFile (toFilePath sidecar) else pure Nothing

loadFixtureArgs :: Path Abs File -> IO [Text]
loadFixtureArgs = fmap fmArgs . loadFixtureMetadata

loadFixtureMode :: Path Abs File -> IO ShellRunMode
loadFixtureMode = fmap fmMode . loadFixtureMetadata

loadFixturePlatforms :: Path Abs File -> IO (Maybe [Text])
loadFixturePlatforms = fmap fmPlatforms . loadFixtureMetadata

loadFixtureStdin :: Path Abs File -> IO Text
loadFixtureStdin = fmap fmStdin . loadFixtureMetadata

loadFixturePrereqs :: Path Abs File -> IO [Text]
loadFixturePrereqs = fmap fmPrereqs . loadFixtureMetadata

loadFixtureRecursive :: Path Abs File -> IO Bool
loadFixtureRecursive = fmap fmRecursive . loadFixtureMetadata

sidecarPath :: Path Abs File -> String -> IO (Path Abs File)
sidecarPath path extension = do
  (basePath, _) <- splitExtension path
  parseAbsFile (toFilePath basePath <> "." <> extension)

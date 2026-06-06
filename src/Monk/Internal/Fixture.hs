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

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Monk.Internal.Shell (ShellRunMode (..))
import Path (Abs, File, Path, addExtension, splitExtension, toFilePath)
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

loadFixtureMetadata :: Path Abs File -> IO FixtureMetadata
loadFixtureMetadata path =
  MkFixtureMetadata
    <$> loadFixtureArgs path
    <*> loadFixtureMode path
    <*> loadFixturePlatforms path
    <*> loadFixtureStdin path
    <*> loadFixturePrereqs path
    <*> loadFixtureRecursive path

loadFixtureArgs :: Path Abs File -> IO [Text]
loadFixtureArgs path = do
  argsPath <- sidecarPath path "args"
  exists <- PathIO.doesFileExist argsPath
  if not exists
    then pure []
    else do
      content <- TIO.readFile (toFilePath argsPath)
      pure (T.words content)

loadFixtureStdin :: Path Abs File -> IO Text
loadFixtureStdin path = do
  stdinPath <- sidecarPath path "stdin"
  exists <- PathIO.doesFileExist stdinPath
  if not exists
    then pure ""
    else TIO.readFile (toFilePath stdinPath)

loadFixtureMode :: Path Abs File -> IO ShellRunMode
loadFixtureMode path = do
  modePath <- sidecarPath path "mode"
  exists <- PathIO.doesFileExist modePath
  if not exists
    then pure ShellRunSource
    else do
      raw <- T.strip <$> TIO.readFile (toFilePath modePath)
      pure $
        case raw of
          "exec" -> ShellRunExec
          "source" -> ShellRunSource
          _ -> ShellRunSource

loadFixturePlatforms :: Path Abs File -> IO (Maybe [Text])
loadFixturePlatforms path = do
  platformsPath <- sidecarPath path "platforms"
  exists <- PathIO.doesFileExist platformsPath
  if not exists
    then pure Nothing
    else Just . filter (not . T.null) . map T.strip . T.words <$> TIO.readFile (toFilePath platformsPath)

loadFixturePrereqs :: Path Abs File -> IO [Text]
loadFixturePrereqs path = do
  prereqPath <- sidecarPath path "prereqs"
  exists <- PathIO.doesFileExist prereqPath
  if not exists
    then pure []
    else do
      content <- TIO.readFile (toFilePath prereqPath)
      pure
        [ cmd
        | cmd <- map T.strip (T.lines content),
          not (T.null cmd)
        ]

loadFixtureRecursive :: Path Abs File -> IO Bool
loadFixtureRecursive path = do
  recursivePath <- sidecarPath path "recursive"
  PathIO.doesFileExist recursivePath

sidecarPath :: Path Abs File -> String -> IO (Path Abs File)
sidecarPath path extension = do
  (basePath, _) <- splitExtension path
  addExtension ("." <> extension) basePath

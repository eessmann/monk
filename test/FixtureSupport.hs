{-# LANGUAGE OverloadedStrings #-}

module FixtureSupport
  ( loadFixtureArgs,
    loadFixtureMode,
    loadFixturePlatforms,
    loadFixtureStdin,
    loadFixturePrereqs,
    loadFixtureRecursive,
  )
where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import ShellSupport (ShellRunMode (..))
import System.Directory (doesFileExist)
import System.FilePath (replaceExtension)

loadFixtureArgs :: FilePath -> IO [T.Text]
loadFixtureArgs path = do
  let argsPath = replaceExtension path "args"
  exists <- doesFileExist argsPath
  if not exists
    then pure []
    else do
      content <- TIO.readFile argsPath
      pure (T.words content)

loadFixtureStdin :: FilePath -> IO T.Text
loadFixtureStdin path = do
  let stdinPath = replaceExtension path "stdin"
  exists <- doesFileExist stdinPath
  if not exists
    then pure ""
    else TIO.readFile stdinPath

loadFixtureMode :: FilePath -> IO ShellRunMode
loadFixtureMode path = do
  let modePath = replaceExtension path "mode"
  exists <- doesFileExist modePath
  if not exists
    then pure ShellRunSource
    else do
      raw <- T.strip <$> TIO.readFile modePath
      pure $
        case raw of
          "exec" -> ShellRunExec
          "source" -> ShellRunSource
          _ -> ShellRunSource

loadFixturePlatforms :: FilePath -> IO (Maybe [T.Text])
loadFixturePlatforms path = do
  let platformsPath = replaceExtension path "platforms"
  exists <- doesFileExist platformsPath
  if not exists
    then pure Nothing
    else Just . filter (not . T.null) . map T.strip . T.words <$> TIO.readFile platformsPath

loadFixturePrereqs :: FilePath -> IO [FilePath]
loadFixturePrereqs path = do
  let prereqPath = replaceExtension path "prereqs"
  exists <- doesFileExist prereqPath
  if not exists
    then pure []
    else do
      content <- TIO.readFile prereqPath
      pure
        [ toString cmd
          | cmd <- map T.strip (T.lines content),
            not (T.null cmd)
        ]

loadFixtureRecursive :: FilePath -> IO Bool
loadFixtureRecursive path =
  doesFileExist (replaceExtension path "recursive")

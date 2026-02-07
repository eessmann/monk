{-# LANGUAGE OverloadedStrings #-}

module FixtureSupport
  ( loadFixtureArgs,
    loadFixtureStdin,
  )
where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
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

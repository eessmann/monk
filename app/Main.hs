{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (IOException, try)
import Data.Set qualified as Set
import Data.Text.IO qualified as TIO
import GHC.IO.Encoding (setFileSystemEncoding, setForeignEncoding, setLocaleEncoding)
import Monk.Diagnostics (renderDiagnostic, renderRuntimeRequirement, renderTranslationNotes)
import Monk.Output
  ( OutputBundle,
    OutputTarget (..),
    outputFailureDiagnostic,
    planCombinedOutputBundle,
    planManagedOutputBundle,
    planSeparateOutputBundle,
    publishOutputBundle,
    renderOutputBundle,
  )
import Monk.Source
  ( SourceGraph,
    SourceGraphFailure (..),
    SourceMode (..),
    sourceGraphDiagnostics,
    sourceGraphRuntimeRequirements,
    sourceRoot,
    translateSourceGraph,
  )
import Monk.Translation
  ( Approximation,
    CallerContract,
    Diagnostic,
    DirectoryContract (..),
    EntryMode (..),
    RuntimeRequirement,
    RuntimeSelection (..),
    TargetProfile (..),
    TranslateConfig (..),
    TranslationFailure (..),
    TranslationPolicy (..),
    defaultConfig,
    emptyCallerContract,
    parseApproximation,
    parseCallerContract,
  )
import Options.Applicative
import System.FilePath qualified as FP
import System.IO (hPutStrLn, hSetEncoding, utf8)

data Options = MkOptions
  { optInput :: FilePath,
    optOutput :: Maybe FilePath,
    optStrict :: Bool,
    optApproximations :: [Approximation],
    optTargetProfile :: TargetProfile,
    optEntryMode :: EntryMode,
    optCallerContract :: Maybe FilePath,
    optDirectoryContract :: DirectoryContract,
    optRuntime :: Maybe FilePath,
    optManaged :: Bool,
    optQuietWarnings :: Bool,
    optRecursive :: Bool,
    optSourceMode :: SourceMode
  }
  deriving stock (Show, Eq)

main :: IO ()
main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8
  mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr]
  opts <- execParser (info (optionsParser <**> helper) (fullDesc <> progDesc "Translate bash scripts to fish"))
  runWithOptions opts

optionsParser :: Parser Options
optionsParser =
  MkOptions
    <$> strArgument (metavar "FILE" <> help "Bash script to translate")
    <*> optional (strOption (short 'o' <> long "output" <> metavar "FILE" <> help "Write output to file"))
    <*> switch (long "strict" <> help "Require exact translation; incompatible with approximation opt-ins")
    <*> many (option (eitherReader readApproximation) (long "allow-approximation" <> metavar "NAME" <> help "Permit one named approximation (repeatable)"))
    <*> option (eitherReader readTargetProfile) (long "target-profile" <> metavar "PROFILE" <> value Bash53Signed64Fish46 <> showDefaultWith (const "bash-5.3-fish-4.6") <> help "Versioned execution profile")
    <*> option (eitherReader readEntryMode) (long "entry" <> metavar "MODE" <> value Standalone <> showDefaultWith (const "standalone") <> help "Output entry mode (standalone|sourceable)")
    <*> optional (strOption (long "caller-contract" <> metavar "FILE" <> help "Version 1 or 2 caller-contract JSON; required for sourceable output"))
    <*> option (eitherReader readDirectoryContract) (long "directory-contract" <> metavar "stable" <> value NoDirectoryContract <> help "Require stable directory ancestry and empty CDPATH (standalone)")
    <*> optional (strOption (long "runtime" <> metavar "FILE" <> help "Native runtime provider; defaults to monk-runtime on PATH"))
    <*> switch (long "managed" <> help "Publish immutable output with its required native runtime")
    <*> switch (short 'q' <> long "quiet-warnings" <> help "Suppress warnings")
    <*> switch (long "recursive" <> help "Recursively translate sourced scripts")
    <*> option
      (eitherReader parseSourceMode)
      ( long "sources"
          <> metavar "MODE"
          <> value SourceSeparate
          <> showDefaultWith renderSourceMode
          <> help "Source handling mode when --recursive is set (inline|separate)"
      )

readApproximation :: String -> Either String Approximation
readApproximation name = maybe (Left ("unknown approximation: " <> name)) Right (parseApproximation (toText name))

readTargetProfile :: String -> Either String TargetProfile
readTargetProfile "bash-5.3-fish-4.6" = Right Bash53Signed64Fish46
readTargetProfile name = Left ("unsupported execution profile: " <> name)

readDirectoryContract :: String -> Either String DirectoryContract
readDirectoryContract "stable" = Right StableDirectoryContract
readDirectoryContract name = Left ("unsupported directory contract: " <> name)

readEntryMode :: String -> Either String EntryMode
readEntryMode "standalone" = Right Standalone
readEntryMode "sourceable" = Right Sourceable
readEntryMode name = Left ("unknown entry mode: " <> name)

parseSourceMode :: String -> Either String SourceMode
parseSourceMode = \case
  "inline" -> Right SourceInline
  "separate" -> Right SourceSeparate
  other -> Left ("invalid source mode: " <> other)

renderSourceMode :: SourceMode -> String
renderSourceMode = \case
  SourceInline -> "inline"
  SourceSeparate -> "separate"

runWithOptions :: Options -> IO ()
runWithOptions opts = do
  cfg <- loadConfig opts
  graphE <- translateSourceGraph cfg (optRecursive opts) (optInput opts)
  case graphE of
    Left failure -> do
      emitSourceGraphFailure failure
      exitFailure
    Right graph -> do
      let rootPath = sourceRoot graph
      planned <-
        if optManaged opts
          then planManagedOutputBundle (fromMaybe (FP.replaceExtension rootPath "fish") (optOutput opts)) graph
          else
            if optRecursive opts && optSourceMode opts == SourceSeparate
              then planSeparateOutputBundle (fromMaybe (FP.replaceExtension rootPath "fish") (optOutput opts)) graph
              else planCombinedOutputBundle (maybe OutputStdout OutputPath (optOutput opts)) graph
      case planned of
        Left diagnostic -> emitDiagnostics [diagnostic] >> exitFailure
        Right bundle -> emitSourceGraphWarnings opts graph >> outputBundle bundle

loadConfig :: Options -> IO TranslateConfig
loadConfig opts = do
  when (optStrict opts && not (null (optApproximations opts))) $
    configFailure "--strict cannot be combined with --allow-approximation"
  when (optEntryMode opts == Sourceable && optDirectoryContract opts /= NoDirectoryContract) $
    configFailure "sourceable directory permissions belong in a version 2 caller contract"
  contract <- case (optEntryMode opts, optCallerContract opts) of
    (Standalone, Nothing) -> pure emptyCallerContract
    (Standalone, Just _) -> configFailure "--caller-contract requires --entry sourceable"
    (Sourceable, Nothing) -> configFailure "--entry sourceable requires --caller-contract FILE"
    (Sourceable, Just path) -> loadCallerContract path
  pure
    defaultConfig
      { translationPolicy = if optStrict opts then ExactOnly else Migration (Set.fromList (optApproximations opts)),
        targetProfile = optTargetProfile opts,
        entryMode = optEntryMode opts,
        callerContract = contract,
        translationRuntime = maybe RuntimeOnPath RuntimePath (optRuntime opts),
        directoryContract = optDirectoryContract opts
      }

loadCallerContract :: FilePath -> IO CallerContract
loadCallerContract path = do
  result <- try @IOException (TIO.readFile path)
  case result of
    Left err -> configFailure ("cannot read caller contract: " <> show err)
    Right input -> either configFailure pure (parseCallerContract input)

configFailure :: Text -> IO a
configFailure message = do
  hPutStrLn stderr ("monk.config: " <> toString message)
  exitFailure

emitSourceGraphWarnings :: Options -> SourceGraph -> IO ()
emitSourceGraphWarnings opts graph = unless (optQuietWarnings opts) $ do
  emitDiagnostics (sourceGraphDiagnostics graph)
  emitTranslateNotes (sourceRoot graph) (sourceGraphDiagnostics graph)
  emitRuntimeRequirements (sourceGraphRuntimeRequirements graph)

emitSourceGraphFailure :: SourceGraphFailure -> IO ()
emitSourceGraphFailure (MkSourceGraphFailure _ failure) = emitTranslationFailure failure

outputBundle :: OutputBundle -> IO ()
outputBundle bundle = case renderOutputBundle bundle of
  [(OutputStdout, contents)] -> putText contents
  _ ->
    publishOutputBundle bundle >>= \case
      Left failure -> emitDiagnostics [outputFailureDiagnostic failure] >> exitFailure
      Right _ -> pure ()

emitDiagnostics :: [Diagnostic] -> IO ()
emitDiagnostics = mapM_ (hPutStrLn stderr . toString . renderDiagnostic)

emitTranslationFailure :: TranslationFailure -> IO ()
emitTranslationFailure = emitDiagnostics . toList . failureDiagnostics

emitTranslateNotes :: FilePath -> [Diagnostic] -> IO ()
emitTranslateNotes path diagnostics =
  mapM_ (hPutStrLn stderr . toString) (renderTranslationNotes path diagnostics)

emitRuntimeRequirements :: [RuntimeRequirement] -> IO ()
emitRuntimeRequirements =
  mapM_ (hPutStrLn stderr . toString . renderRuntimeRequirement)

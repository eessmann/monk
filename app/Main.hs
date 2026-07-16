{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Monk.AST (renderScript)
import Monk.Diagnostics (renderDiagnostic, renderRuntimeRequirement, renderTranslationNotes)
import Monk.Output
  ( GeneratedFile (..),
    OutputBundle (bundleUserFiles),
    OutputTarget (OutputPath, OutputStdout),
    planCombinedOutputBundle,
    planSeparateOutputBundle,
    renderOutputBundle,
  )
import Monk.Source
  ( SourceGraph (..),
    SourceGraphFailure (..),
    SourceMode (..),
    Translation (trDiagnostics, trRuntimeRequirements),
    rewriteSources,
    translateSourceGraph,
  )
import Monk.Translation
  ( Diagnostic,
    RuntimeRequirement,
    TranslationFailure (..),
    defaultConfig,
    strictConfig,
  )
import Options.Applicative
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import System.FilePath qualified as FP
import System.IO (hPutStrLn)

data Options = MkOptions
  { optInput :: FilePath,
    optOutput :: Maybe FilePath,
    optStrict :: Bool,
    optQuietWarnings :: Bool,
    optRecursive :: Bool,
    optSourceMode :: SourceMode
  }
  deriving stock (Show, Eq)

main :: IO ()
main = do
  opts <- execParser (info (optionsParser <**> helper) (fullDesc <> progDesc "Translate bash scripts to fish"))
  runWithOptions opts

optionsParser :: Parser Options
optionsParser =
  MkOptions
    <$> strArgument (metavar "FILE" <> help "Bash script to translate")
    <*> optional (strOption (short 'o' <> long "output" <> metavar "FILE" <> help "Write output to file"))
    <*> switch (long "strict" <> help "Fail on unsupported constructs")
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
  let cfg = if optStrict opts then strictConfig else defaultConfig
  rootPath <- canonicalizePath (optInput opts)
  graphE <- translateSourceGraph cfg (optRecursive opts) rootPath
  case graphE of
    Left failure -> do
      emitSourceGraphFailure failure
      exitFailure
    Right graph -> do
      emitSourceGraphWarnings opts graph
      case optSourceMode opts of
        SourceInline ->
          outputInline opts graph rootPath
        SourceSeparate ->
          outputSeparate opts graph rootPath

emitSourceGraphWarnings :: Options -> SourceGraph -> IO ()
emitSourceGraphWarnings opts graph =
  unless (optQuietWarnings opts) $
    forM_ (sgOrder graph) $ \path -> do
      case M.lookup path (sgTranslations graph) of
        Nothing -> pure ()
        Just translation -> do
          emitDiagnostics (trDiagnostics translation)
          emitTranslateNotes path (trDiagnostics translation)
          when (optSourceMode opts == SourceSeparate) $
            emitRuntimeRequirements (trRuntimeRequirements translation)

emitSourceGraphFailure :: SourceGraphFailure -> IO ()
emitSourceGraphFailure = \case
  SourceGraphFailure _ failure -> emitTranslationFailure failure

outputInline :: Options -> SourceGraph -> FilePath -> IO ()
outputInline opts graph rootPath = do
  let target = maybe OutputStdout OutputPath (optOutput opts)
  planned <- planCombinedOutputBundle target rootPath graph
  case planned of
    Left diagnostic -> do
      emitDiagnostics [diagnostic]
      exitFailure
    Right bundle -> do
      let generated = NE.head (bundleUserFiles bundle)
          translatedDiagnostics =
            concat
              [ trDiagnostics translation
              | path <- sgOrder graph,
                Just translation <- [M.lookup path (sgTranslations graph)]
              ]
          inlineDiagnostics = generatedDiagnostics generated L.\\ translatedDiagnostics
      unless (optQuietWarnings opts) $ do
        emitDiagnostics inlineDiagnostics
        emitRuntimeRequirements (generatedRuntimeRequirements generated)
      writeOutput opts (renderScript (generatedScript generated))

outputSeparate :: Options -> SourceGraph -> FilePath -> IO ()
outputSeparate opts graph rootPath =
  case M.lookup rootPath (sgTranslations graph) of
    Nothing -> emitWarn opts "warning: no translation output"
    Just rootTranslation
      | not (optRecursive opts) ->
          writeOutput opts (renderScript (rewriteSources (sgTranslations graph) rootTranslation))
      | otherwise -> do
          let rootOutput = fromMaybe (FP.replaceExtension rootPath "fish") (optOutput opts)
          case planSeparateOutputBundle rootOutput rootPath graph of
            Left diagnostic -> do
              emitDiagnostics [diagnostic]
              exitFailure
            Right bundle -> do
              let rendered = renderOutputBundle bundle
                  rootTarget = OutputPath rootOutput
              case L.lookup rootTarget rendered of
                Nothing -> emitWarn opts "warning: no root translation output"
                Just rootText -> writeOutput opts rootText
              forM_ rendered $ \(target, contents) ->
                case target of
                  OutputPath path ->
                    when (isNothing (optOutput opts) || path /= rootOutput) $
                      writeFileTextEnsuringDir path contents
                  _ -> pure ()

writeOutput :: Options -> Text -> IO ()
writeOutput opts output =
  case optOutput opts of
    Nothing -> putText output
    Just path -> writeFileTextEnsuringDir path output

writeFileTextEnsuringDir :: FilePath -> Text -> IO ()
writeFileTextEnsuringDir path contents = do
  createDirectoryIfMissing True (FP.takeDirectory path)
  writeFileText path contents

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

emitWarn :: Options -> Text -> IO ()
emitWarn opts msg =
  unless (optQuietWarnings opts) $
    hPutStrLn stderr (toString msg)

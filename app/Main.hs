{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Monk.Diagnostics
  ( renderParseComment,
    renderTranslateError,
    renderTranslationNotes,
    renderWarning,
  )
import Monk.Source
  ( SourceGraph (..),
    SourceGraphFailure (..),
    SourceMode (..),
    rewriteSources,
    translateSourceGraph,
  )
import Monk.Translation
  ( Translation (..),
    TranslateError,
    Warning,
    defaultConfig,
    inlineStatements,
    renderFish,
    stateWarnings,
    strictConfig,
  )
import Options.Applicative
import ShellCheck.Interface (PositionedComment)
import System.Directory (canonicalizePath)
import System.FilePath (replaceExtension)
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
      emitParseWarnings (M.findWithDefault [] path (sgParseComments graph))
      case M.lookup path (sgTranslations graph) of
        Nothing -> pure ()
        Just translation -> do
          let warns = stateWarnings (trState translation)
          emitTranslateWarnings warns
          emitTranslateNotes path warns

emitSourceGraphFailure :: SourceGraphFailure -> IO ()
emitSourceGraphFailure = \case
  SourceGraphParseErrors _ errs -> emitParseErrors errs
  SourceGraphTranslateFailure _ err -> emitTranslateError err

outputInline :: Options -> SourceGraph -> FilePath -> IO ()
outputInline opts graph rootPath =
  case M.lookup rootPath (sgTranslations graph) of
    Nothing -> emitWarn opts "warning: no translation output"
    Just _ -> do
      stmts <- inlineStatements (emitWarn opts) (sgTranslations graph) Set.empty rootPath
      writeOutput opts (renderFish stmts)

outputSeparate :: Options -> SourceGraph -> FilePath -> IO ()
outputSeparate opts graph rootPath =
  case M.lookup rootPath (sgTranslations graph) of
    Nothing -> emitWarn opts "warning: no translation output"
    Just rootTr -> do
      writeOutput opts (renderFish (rewriteSources (sgTranslations graph) rootTr))
      when (optRecursive opts) $ do
        let writeRoot = isNothing (optOutput opts)
        forM_ (M.elems (sgTranslations graph)) $ \tr -> do
          let outPath = replaceExtension (trPath tr) "fish"
              rendered = renderFish (rewriteSources (sgTranslations graph) tr)
              shouldWrite = writeRoot || trPath tr /= rootPath
          when shouldWrite $
            writeFileText outPath rendered

writeOutput :: Options -> Text -> IO ()
writeOutput opts output =
  case optOutput opts of
    Nothing -> putText output
    Just path -> writeFileText path output

emitParseWarnings :: [PositionedComment] -> IO ()
emitParseWarnings = mapM_ (hPutStrLn stderr . toString . renderParseComment)

emitParseErrors :: [PositionedComment] -> IO ()
emitParseErrors = mapM_ (hPutStrLn stderr . toString . renderParseComment)

emitTranslateWarnings :: [Warning] -> IO ()
emitTranslateWarnings = mapM_ (hPutStrLn stderr . toString . renderWarning)

emitTranslateError :: TranslateError -> IO ()
emitTranslateError err =
  hPutStrLn stderr (toString (renderTranslateError err))

emitTranslateNotes :: FilePath -> [Warning] -> IO ()
emitTranslateNotes path warns =
  mapM_ (hPutStrLn stderr . toString) (renderTranslationNotes path warns)

emitWarn :: Options -> Text -> IO ()
emitWarn opts msg =
  unless (optQuietWarnings opts) $
    hPutStrLn stderr (toString msg)

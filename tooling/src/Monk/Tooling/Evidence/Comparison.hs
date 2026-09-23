-- | Direct Bash comparison of successful bakeoff translations.
module Monk.Tooling.Evidence.Comparison (runComparison) where

import Data.Aeson (Value (..), eitherDecodeStrict', object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.List (nub)
import Data.Text qualified as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Monk.Runtime.Digest (sha256)
import Monk.Tooling.Evidence.Common (Observation (..), arrayField, base64, cleanEnvironment, commandEnvironment, compareObservations, digestFile, field, numberField, observationRecord, readJson, runObservation, textField, writeJson)
import System.Directory (canonicalizePath, createDirectory, doesFileExist, findExecutable)
import System.FilePath (replaceExtension, (</>))

runComparison :: FilePath -> String -> String -> Int -> IO Value
runComparison runDirectory bashCommand fishCommand timeout = do
  runPath <- canonicalizePath runDirectory
  raw <- B.readFile (runPath </> "report.json")
  fixtures <- case eitherDecodeReport raw of
    Right (Array values) -> pure (toList values)
    Right _ -> fail "bakeoff report must be a top-level fixture array"
    Left reason -> fail reason
  meta <- readJson (runPath </> "meta.json")
  cwd <- T.unpack <$> either fail pure (textField "metaCwd" meta)
  forM_ fixtures $ \fixture -> do
    metadata <- either fail pure (field "fixtureReportMetadata" fixture)
    mode <- either fail pure (textField "fixtureMetaMode" metadata)
    unless (mode == "ShellRunExec") $ fail "Rerun monk-bakeoff with the standalone selection fix first"
  let output = runPath </> "bash-comparison"
  createDirectory output
  bash <- resolveExecutable bashCommand
  fish <- resolveExecutable fishCommand
  env <- cleanEnvironment output
  timestamp <- formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q+00:00" <$> getCurrentTime
  rows <- forM (zip [0 :: Int ..] fixtures) $ \(index, fixture) -> do
    fixturePath <- T.unpack <$> either fail pure (textField "fixtureReportPath" fixture)
    relative <- either fail pure (textField "fixtureReportRelativePath" fixture)
    fixtureGroup <- either fail pure (field "fixtureReportGroup" fixture)
    metadata <- either fail pure (field "fixtureReportMetadata" fixture)
    hash <- digestFile fixturePath
    let translations = [(name, fromRight Null (field key fixture)) | (name, key) <- [("monk", "fixtureReportMonkTranslation"), ("babelfish", "fixtureReportBabelfishTranslation")]]
        admitted = [(name, translation) | (name, translation) <- translations, field "translationStatus" translation == Right (String "CommandSucceeded")]
        rejected = [(name, object ["status" .= case translation of Null -> String "MissingReport"; _ -> fromRight Null (field "translationStatus" translation)]) | (name, translation) <- translations, name `notElem` map fst admitted]
    if null admitted
      then pure $ object ["fixture" .= relative, "group" .= fixtureGroup, "input_sha256" .= hash, "metadata" .= metadata, "tools" .= object [fromString name .= value | (name, value) <- rejected]]
      else do
        let directory = output </> pad3 index
        createDirectory directory
        let stdinPath = replaceExtension fixturePath "stdin"
        present <- doesFileExist stdinPath
        input <- if present then B.readFile stdinPath else pure ""
        argv <- either fail pure (arrayField "fixtureMetaArgs" metadata >>= traverse asString)
        let bashArgv = [bash, "--noprofile", "--norc", fixturePath] <> argv
        baseline <- runObservation bashArgv input cwd env timeout
        reference <- observationRecord directory "bash" bashArgv baseline
        observed <- forM admitted $ \(name, translation) -> do
          generated <- T.unpack <$> either fail pure (textField "translationOutputPath" translation)
          let command = [fish, "--no-config", generated] <> argv
          candidate <- runObservation command input cwd env timeout
          execution <- observationRecord directory name command candidate
          generatedHash <- digestFile generated
          diagnostics <- object <$> forM ["translationErrorCount", "translationWarningCount", "translationNotesCount"] (\key -> do value <- either fail pure (field key translation); pure (fromString (T.unpack key), value))
          let compared = compareObservations baseline candidate
              value = case compared of
                Object fields -> Object $ KM.insert "diagnostics" diagnostics $ KM.insert "generated_sha256" (String (T.pack generatedHash)) $ KM.insert "execution" execution fields
                _ -> compared
          pure (name, value)
        pure $
          object
            [ "fixture" .= relative,
              "group" .= fixtureGroup,
              "input_sha256" .= hash,
              "metadata" .= metadata,
              "tools" .= object [fromString name .= value | (name, value) <- rejected <> observed],
              "stdin_base64" .= base64 input,
              "bash" .= reference
            ]
  tools <- forM [("bash", bash), ("fish", fish)] $ \(name, path) -> do
    hash <- digestFile path
    version <- runObservation [path, "--version"] "" cwd env timeout
    unless (status version == "completed" && exit version == 0) $ fail (name <> " version probe failed")
    let firstLine = case C.lines (observedStdout version) of line : _ -> C.unpack line; [] -> ""
    pure (name, object ["path" .= path, "sha256" .= hash, "version" .= firstLine])
  let totals = object [fromString name .= counts name rows | name <- ["monk", "babelfish"]]
      report =
        object
          [ "schema" .= (1 :: Int),
            "timestamp" .= timestamp,
            "scope" .= (["stdout bytes", "stderr bytes", "exit status"] :: [Text]),
            "entry_mode" .= ("standalone" :: Text),
            "cwd" .= cwd,
            "timeout_seconds" .= timeout,
            "environment" .= commandEnvironment env,
            "bakeoff_report_sha256" .= C.unpack (sha256 raw),
            "tools" .= object [fromString name .= value | (name, value) <- tools],
            "totals" .= totals,
            "fixtures" .= rows
          ]
  writeJson (output </> "report.json") report
  pure totals
  where
    eitherDecodeReport = eitherDecodeStrict'
    asString (String value) = Right (T.unpack value)
    asString _ = Left "expected argument string"
    pad3 index = let digits = show index in replicate (max 0 (3 - length digits)) '0' <> digits

counts :: String -> [Value] -> Value
counts name rows = object [fromString statusName .= length (filter (== statusName) statuses) | statusName <- nub statuses]
  where
    statuses = map outcome rows <> concatMap mismatchNames rows
    outcome row = case field "tools" row >>= field (T.pack name) >>= textField "status" of
      Right value -> T.unpack value
      _ -> "MissingReport"
    mismatchNames row
      | outcome row /= "mismatch" = []
      | otherwise = case field "tools" row >>= field (T.pack name) >>= field "diagnostics" of
          Right diagnostics ->
            let errors = fromRight 0 (numberField "translationErrorCount" diagnostics)
                warnings = fromRight 0 (numberField "translationWarningCount" diagnostics)
                notes = fromRight 0 (numberField "translationNotesCount" diagnostics)
             in ["zero_diagnostic_mismatches" | errors == 0 && warnings == 0 && notes == 0]
                  <> ["zero_warning_error_mismatches" | errors == 0 && warnings == 0]
          _ -> []

resolveExecutable :: String -> IO FilePath
resolveExecutable name = findExecutable name >>= maybe (fail ("Executable not found: " <> name)) canonicalizePath

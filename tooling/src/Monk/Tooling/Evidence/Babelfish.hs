-- | Fresh Babelfish translations against the frozen historical cohort.
module Monk.Tooling.Evidence.Babelfish (runBabelfish, common16Timing) where

import Data.Aeson (Result (..), Value (..), fromJSON, object, toJSON, (.=))
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.Text qualified as T
import Monk.Tooling.Evidence.Common (Observation (..), arrayField, digestFile, field, nativeRecord, numberField, readJson, textField, unbase64, writeJson)
import Monk.Tooling.Evidence.Native (nativeEnvironment, nativeExecute, nativeSame, validateCommon16, validateHistoric95)
import System.Directory (canonicalizePath, createDirectory, doesFileExist)
import System.FilePath (replaceExtension, takeDirectory, (</>))

runBabelfish :: FilePath -> FilePath -> FilePath -> FilePath -> Bool -> String -> IO Value
runBabelfish output babelfish bash fish timeCommon runName = do
  cohort <- readJson (output </> "baseline-cohort.json")
  cwd <- T.unpack <$> require (textField "cwd" cohort)
  allFixtures <- require (arrayField "fixtures" cohort)
  frozenCount <- require (numberField "historic_count" cohort)
  let fixtures = take 95 allFixtures
  unless (frozenCount == 95) $ fail "frozen95 denominator changed"
  validateHistoric95 cwd fixtures
  indices <- require (traverse (numberField "index") fixtures)
  unless (indices == [0 .. 94]) $ fail "frozen95 indices changed"
  common <- require (arrayField "common16" cohort)
  validateCommon16 cwd common
  env <- nativeEnvironment output
  let measured = output </> runName
  createDirectory measured
  binary <- canonicalizePath babelfish
  version <- nativeExecute [binary, "--version"] cwd "" env
  let receipt = takeDirectory (takeDirectory binary) </> "INSTALL_RECEIPT.json"
  exists <- doesFileExist receipt
  receiptValue <- if exists then readJson receipt else pure Null
  receiptHash <- if exists then Just <$> digestFile receipt else pure Nothing
  binaryHash <- digestFile binary
  bashHash <- digestFile bash
  fishHash <- digestFile fish
  adapterHash <- digestFile (output </> "providers/python3")
  let provenance =
        object
          [ "path" .= binary,
            "sha256" .= binaryHash,
            "version_probe" .= object ["exit" .= exit version, "stdout" .= C.unpack (observedStdout version), "stderr" .= C.unpack (observedStderr version)],
            "homebrew_receipt" .= receiptValue,
            "homebrew_receipt_sha256" .= receiptHash,
            "bash_sha256" .= bashHash,
            "fish_sha256" .= fishHash,
            "historical_python_adapter" .= object ["path" .= (output </> "providers/python3"), "sha256" .= adapterHash, "scope" .= ("Frozen background-jobs.bash command only" :: Text)]
          ]
  rows <- forM fixtures $ \fixture -> do
    name <- T.unpack <$> require (textField "fixture" fixture)
    expected <- require (textField "input_sha256" fixture)
    actual <- digestFile (sourceFor cwd name)
    unless (expected == T.pack actual) $ fail ("Frozen fixture changed: " <> name)
    index <- require (numberField "index" fixture)
    metadata <- require (field "metadata" fixture)
    argv <- require (arrayField "fixtureMetaArgs" metadata >>= traverse asString)
    input <- require (textField "stdin_base64" fixture >>= unbase64)
    modes <- forM (["default", "stable"] :: [Text]) $ \mode -> do
      let generated = measured </> show index <> "-" <> T.unpack mode <> ".fish"
      source <- B.readFile (sourceFor cwd name)
      translated <- nativeExecute [binary] cwd source env
      B.writeFile generated (observedStdout translated)
      B.writeFile (replaceExtension generated "diagnostics") (observedStderr translated)
      generatedHash <- digestFile generated
      let translation = ["translation_exit" .= exit translated, "generated_bytes" .= B.length (observedStdout translated), "generated_sha256" .= generatedHash]
      if exit translated /= 0
        then pure (mode, object translation)
        else do
          reference <- nativeExecute ([bash, "--noprofile", "--norc", sourceFor cwd name] <> argv) cwd input env
          let command = [fish, "--no-config", generated] <> argv
          observed <- nativeExecute command cwd input env
          let matched = nativeSame reference observed
          samples <-
            if timeCommon && mode == "default" && String (T.pack name) `elem` common
              then do
                unless matched $ fail ("Babelfish common16 mismatch: " <> name)
                replicateM_ 3 $ nativeExecute command cwd input env >>= \warm -> unless (nativeSame reference warm) (fail ("Babelfish warmup mismatch: " <> name))
                forM [1 :: Int .. 20] $ \_ -> do
                  sample <- nativeExecute command cwd input env
                  unless (nativeSame reference sample) $ fail ("Babelfish timed sample mismatch: " <> name)
                  pure (fromIntegral (elapsedNs sample) :: Integer)
              else pure []
          pure (mode, object (translation <> ["bash" .= nativeRecord reference, "fish" .= nativeRecord observed, "matched" .= matched] <> ["samples_ns" .= samples | not (null samples)]))
    let row = object ["fixture" .= name, "input_sha256" .= expected, "modes" .= object [fromString (T.unpack mode) .= value | (mode, value) <- modes]]
    pure row
  writeJson (measured </> "partial.json") (toJSON rows)
  let totals = object [fromString (T.unpack mode) .= modeTotals mode rows | mode <- (["default", "stable"] :: [Text])]
      reportFields =
        [ "provenance" .= provenance,
          "cwd" .= cwd,
          "historic_denominator" .= (95 :: Int),
          "contract_note" .= ("Babelfish has no directory contract selector; both labels are fresh independent reruns under the corresponding comparison scope, with empty CDPATH." :: Text),
          "totals" .= totals,
          "fixtures" .= rows
        ]
  timing <- if timeCommon then Just <$> require (common16Timing common rows) else pure Nothing
  writeJson (measured </> "report.json") $ object (reportFields <> maybe [] (pure . ("common16_timing" .=)) timing)
  pure totals

common16Timing :: [Value] -> [Value] -> Either String Value
common16Timing common rows = do
  unless (length common == 16) $ Left "common16 timing requires exactly 16 fixtures"
  cohorts <- forM common $ \name -> do
    row <- case [row | row <- rows, field "fixture" row == Right name] of
      [matching] -> Right matching
      _ -> Left "common16 timing fixture is missing or duplicated"
    samples <- field "modes" row >>= field "default" >>= arrayField "samples_ns"
    unless (length samples == 20) $ Left "common16 timing requires 20 samples per fixture"
    traverse positiveSample samples
  totals <- forM [0 .. 19] $ \index -> sum <$> forM cohorts (\samples -> case drop index samples of sample : _ -> Right sample; _ -> Left "common16 timing sample is missing")
  pure $ object ["warmups" .= (3 :: Int), "samples" .= (20 :: Int), "sample_totals_ns" .= totals, "median_ns" .= median totals, "protocol" .= ("serial Babelfish-only follow-up; separate from alternating baseline/candidate acceptance measurements" :: Text)]
  where
    positiveSample (Number number) = case fromJSON (Number number) of
      Success (sample :: Integer) | sample > 0 -> Right sample
      _ -> Left "common16 timing sample is not a positive integer"
    positiveSample _ = Left "common16 timing sample is not numeric"

modeTotals :: Text -> [Value] -> Value
modeTotals mode rows =
  let observations = [value | row <- rows, Right value <- [field "modes" row >>= field mode]]
      admitted = length [() | value <- observations, field "translation_exit" value == Right (Number 0)]
      matched = length [() | value <- observations, field "matched" value == Right (Bool True)]
      mismatched = length [() | value <- observations, field "matched" value == Right (Bool False)]
   in object ["admitted" .= admitted, "matched" .= matched, "admitted_mismatches" .= mismatched]

median :: [Integer] -> Double
median values = case drop 9 (sort values) of left : right : _ -> (fromIntegral left + fromIntegral right) / 2; _ -> 0

require :: Either String a -> IO a
require = either fail pure

asString :: Value -> Either String String
asString (String value) = Right (T.unpack value)
asString _ = Left "expected argument string"

sourceFor :: FilePath -> FilePath -> FilePath
sourceFor cwd name
  | name == "test/fixtures/integration/background-jobs.bash" = cwd </> "test/evidence/frozen95/background-jobs.bash"
  | otherwise = cwd </> name

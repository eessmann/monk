-- | Immutable performance cohorts and strict aggregate gates.
module Monk.Tooling.Evidence.Performance (aggregate, freezePerformance, measurePerformance) where

import Control.Monad (foldM)
import Data.Aeson (Result (..), Value (..), fromJSON, object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.List (lookup)
import Data.Text qualified as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Monk.Runtime.Digest (sha256)
import Monk.Tooling.Evidence.Common (Observation (..), arrayField, cleanEnvironment, compareObservations, digestFile, field, observationRecord, provenance, readJson, runObservation, snapshot, textField, writeJson)
import Monk.Tooling.Evidence.Freeze (freezeWithShake)
import Monk.Tooling.Evidence.Portable (insertFields, portableExecute, portableTranslate)
import System.Directory (canonicalizePath, copyFileWithMetadata, createDirectory, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, doesPathExist, listDirectory)
import System.FilePath (takeDirectory, (</>))

aggregate :: [Value] -> Value
aggregate rows
  | null rows || not (null missing) = object ["status" .= ("unverified" :: Text), "unavailable" .= missing, "expected" .= length rows, "measured" .= (length rows - length missing)]
  | otherwise =
      let totals :: [(Text, [Integer])]
          totals = [(name, [sum [sample row name index | row <- rows] | index <- [0 .. 19]]) | name <- ["baseline", "candidate"]]
          baseline = median (fromMaybe [] (lookup "baseline" totals))
          candidate = median (fromMaybe [] (lookup "candidate" totals))
          ratio = candidate / baseline
       in object
            [ "status" .= ("measured" :: Text),
              "sample_totals_ns" .= object ["baseline" .= fromMaybe [] (lookup "baseline" totals), "candidate" .= fromMaybe [] (lookup "candidate" totals)],
              "median_ns" .= object ["baseline" .= baseline, "candidate" .= candidate],
              "candidate_over_baseline" .= ratio,
              "at_most_10_percent_regression" .= (ratio <= 1.1),
              "lower_time" .= (ratio < 1.0)
            ]
  where
    missing = [name | row <- rows, let name = fromRight "<unknown>" (textField "fixture" row), field "status" row /= Right (String "measured") || not (valid row)]
    valid row = all (\name -> case field "samples_ns" row >>= arrayField name of Right values -> length values == 20 && all positiveInteger values; _ -> False) ["baseline", "candidate"]
    positiveInteger (Number n) = case fromJSON (Number n) of Success (value :: Integer) -> value > 0; Error _ -> False
    positiveInteger _ = False
    sample :: Value -> Text -> Int -> Integer
    sample row name index = case field "samples_ns" row >>= arrayField name of
      Right values -> case drop index values of
        Number n : _ -> case fromJSON (Number n) of Success value -> value; Error _ -> 0
        _ -> 0
      _ -> 0
    median :: [Integer] -> Double
    median values = case drop 9 (sort values) of
      middle1 : middle2 : _ -> (fromIntegral middle1 + fromIntegral middle2) / (2 :: Double)
      _ -> 0

freezePerformance :: FilePath -> FilePath -> IO ()
freezePerformance frozen output = do
  occupied <- doesPathExist output
  when occupied $ fail ("immutable evidence output already exists: " <> output)
  freezeWithShake
    (output </> "manifest.json")
    [(frozen, ["inputs//*"])]
    [frozen </> "manifest.json"]
    (freezePerformanceIO frozen output)

freezePerformanceIO :: FilePath -> FilePath -> IO ()
freezePerformanceIO frozen output = do
  original <- readJson (frozen </> "manifest.json")
  copyTree (frozen </> "inputs") (output </> "inputs")
  fixtures <- either fail pure (arrayField "fixtures" original)
  common <- either fail pure (arrayField "common16" original)
  let selected = [insertField "performance_cohort" (String "common16") row | name <- common, row <- fixtures, field "fixture" row == Right name]
  definitions <- either fail pure (arrayField "arithmetic3" original)
  arithmetic <- forM definitions $ \definition -> do
    path <- T.unpack <$> either fail pure (textField "fixture" definition)
    expected <- either fail pure (textField "input_sha256" definition)
    exists <- doesFileExist path
    available <- if exists then (== T.unpack expected) <$> digestFile path else pure False
    when available $ do
      createDirectoryIfMissing True (takeDirectory (output </> "inputs" </> path))
      copyFileWithMetadata path (output </> "inputs" </> path)
    metadata <- either fail pure (field "metadata" definition)
    inputBase64 <- either fail pure (field "stdin_base64" definition)
    pure $
      object $
        [ "fixture" .= path,
          "performance_cohort" .= ("arithmetic3" :: Text),
          "cohort" .= ("arithmetic" :: Text),
          "metadata" .= metadata,
          "stdin_base64" .= inputBase64,
          "input_sha256" .= expected
        ]
          <> ["unavailable_reason" .= ("Original frozen arithmetic input is absent locally; hash retained, no substitute generated." :: Text) | not available]
  targeted <- forM targetedSources $ \(name, source) -> do
    let path = "performance" </> name <> ".bash"
        destination = output </> "inputs" </> path
    createDirectoryIfMissing True (takeDirectory destination)
    B.writeFile destination (C.pack source)
    hash <- digestFile destination
    pure $
      object
        [ "fixture" .= path,
          "performance_cohort" .= ("targeted-native" :: Text),
          "cohort" .= ("targeted-native" :: Text),
          "metadata" .= object ["fixtureMetaArgs" .= ([] :: [Text]), "fixtureMetaRecursive" .= False],
          "stdin_base64" .= ("" :: Text),
          "input_sha256" .= hash
        ]
  sourceHash <- digestFile (frozen </> "manifest.json")
  tree <- snapshot (output </> "inputs")
  writeJson (output </> "manifest.json") $
    object
      [ "schema" .= (1 :: Int),
        "fixtures" .= (selected <> arithmetic <> targeted),
        "comparison_manifest_sha256" .= sourceHash,
        "historical_denominator" .= (95 :: Int),
        "input_tree" .= tree
      ]

measurePerformance :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> IO Value
measurePerformance frozen output baseline candidate baselineRuntime runtime bash fish receiptPath = do
  manifestBytes <- B.readFile (frozen </> "manifest.json")
  manifest <- readJson (frozen </> "manifest.json")
  let inputs = frozen </> "inputs"
  tree <- snapshot inputs
  unless (field "input_tree" manifest == Right tree) $ fail "Frozen inputs changed"
  receiptBytes <- B.readFile receiptPath
  receipt <- readJson receiptPath
  unless (field "schema" receipt == Right (Number 2) && field "successful_stable_command" receipt == Right (Bool True)) $ fail "A successful command with stable build inputs is required"
  verified <- require (arrayField "binaries" receipt)
  forM_ [candidate, runtime] $ \path -> do
    canonical <- canonicalizePath path
    hash <- digestFile path
    unless (any (\row -> field "path" row == Right (String (T.pack canonical)) && field "sha256" row == Right (String (T.pack hash))) verified) $ fail ("Candidate/runtime not verified by build receipt: " <> path)
  createDirectoryIfMissing True (takeDirectory output)
  createDirectory output
  let providers = output </> "providers"
  createDirectory providers
  supplied <- forM
    [("baseline", baseline), ("candidate", candidate), ("baseline_runtime", baselineRuntime), ("runtime", runtime), ("bash", bash), ("fish", fish)]
    $ \(name, raw) -> do
      source <- canonicalizePath raw
      copyFileWithMetadata source (providers </> name)
      pure (name, source)
  forM_ ["candidate", "runtime"] $ \name -> do
    source <- maybe (fail "missing provider") pure (lookup name supplied)
    copied <- digestFile (providers </> name)
    sourceHash <- digestFile source
    unless (copied == sourceHash) $ fail ("Binary changed while capturing provider: " <> name)
  env0 <- cleanEnvironment output
  let pathValue = takeDirectory bash <> ":" <> takeDirectory fish <> ":" <> fromMaybe "" (lookup "PATH" env0)
      env = ("PATH", pathValue) : filter ((/= "PATH") . fst) env0
      cwd = output </> "execution-cwd"
  providerRows <- forM supplied $ \(name, _) -> do
    value <- provenance (providers </> name)
    pure (name, value)
  baselineProbe <- runObservation [providers </> "baseline_runtime", "--describe"] "" inputs env 30
  baselineProbeRecord <- observationRecord output "baseline-probe" [providers </> "baseline_runtime", "--describe"] baselineProbe
  timestamp <- formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q+00:00" <$> getCurrentTime
  fixtures <- require (arrayField "fixtures" manifest)
  production <- require (field "production_after" receipt >>= textField "sha256")
  verification <- require (field "after" receipt >>= textField "sha256")
  let reportBase rows =
        object
          [ "schema" .= (1 :: Int),
            "timestamp" .= timestamp,
            "build_receipt_sha256" .= C.unpack (sha256 receiptBytes),
            "source_fingerprint" .= production,
            "verification_input_fingerprint" .= verification,
            "manifest_sha256" .= C.unpack (sha256 manifestBytes),
            "providers" .= object [fromString name .= value | (name, value) <- providerRows],
            "candidate_entry" .= ("runtime --abi 2 launch SCRIPT ARGS" :: Text),
            "protocol" .= object ["warmups" .= (3 :: Int), "samples" .= (20 :: Int), "alternating_order" .= True, "serial" .= True],
            "process_launches" .= object ["status" .= ("unverified" :: Text), "reason" .= ("No process tracing collected; static call counts are not launches." :: Text)],
            "baseline_probe" .= baselineProbeRecord,
            "fixtures" .= rows
          ]
  rows <-
    foldM
      ( \prior (index, fixture) -> do
          name <- T.unpack <$> require (textField "fixture" fixture)
          cohort <- require (textField "performance_cohort" fixture)
          expected <- require (textField "input_sha256" fixture)
          let baseRow = object ["fixture" .= name, "cohort" .= cohort, "input_sha256" .= expected]
          row <- case field "unavailable_reason" fixture of
            Right (String reason) -> pure $ insertFields baseRow ["status" .= ("missing-frozen-input" :: Text), "reason" .= reason]
            _ -> do
              hash <- digestFile (inputs </> name)
              unless (expected == T.pack hash) $ fail ("Frozen fixture hash differs: " <> name)
              let directory = output </> pad3 index
              createDirectory directory
              (reference, bashCommand, _) <- portableExecute (providers </> "bash") False Nothing fixture (inputs </> name) inputs cwd env 30
              referenceRecord <- observationRecord directory "bash" bashCommand reference
              let withReference = insertFields baseRow ["reference" .= referenceRecord]
              runProviders providers inputs cwd env baselineProbe reference fixture directory withReference
          let updated = prior <> [row]
          writeJson (output </> "partial.json") (reportBase updated)
          pure updated
      )
      []
      (zip [0 :: Int ..] fixtures)
  let cohorts = object [fromString (T.unpack name) .= aggregate [row | row <- rows, field "cohort" row == Right (String name)] | name <- ["common16", "arithmetic3", "targeted-native"]]
      acceptance = object ["status" .= ("unverified" :: Text), "reason" .= ("Timing cohorts must all be complete and targeted process-launch reduction must be measured." :: Text)]
      report = insertFields (reportBase rows) ["cohorts" .= cohorts, "acceptance" .= acceptance]
  writeJson (output </> "report.json") report
  pure cohorts
  where
    runProviders providers inputs cwd env baselineProbe reference fixture directory initial = do
      providerResults <- forM ["baseline", "candidate"] $ \provider -> do
        let generated = directory </> provider <> ".fish"
            providerRuntime = providers </> if provider == "baseline" then "baseline_runtime" else "runtime"
            tool = if provider == "baseline" then "current" else "candidate"
        (translated, translateCommand) <- portableTranslate tool (providers </> provider) providerRuntime fixture inputs generated env 30 "default"
        translationRecord <- observationRecord directory (provider <> "-translation") translateCommand translated
        if status translated /= "completed" || exit translated /= 0
          then pure (provider, insertFields initial [fromString (provider <> "_translation") .= translationRecord, "status" .= ("translation-unavailable" :: Text), "provider" .= provider], Nothing)
          else do
            let launcher = if provider == "candidate" then Just providerRuntime else Nothing
            (observed, command, _) <- portableExecute (providers </> "fish") True launcher fixture generated inputs cwd env 30
            firstRecord <- observationRecord directory (provider <> "-first") command observed
            let base = insertFields initial [fromString (provider <> "_translation") .= translationRecord, "first_run" .= object [fromString provider .= firstRecord]]
                unsupported = provider == "baseline" && "requires 64-bit Linux" `B.isInfixOf` observedStderr baselineProbe && "missing or incompatible native runtime" `B.isInfixOf` observedStderr observed
                compared = compareObservations reference observed
            if unsupported
              then pure (provider, insertFields base ["status" .= ("unsupported-native-platform" :: Text), "reason" .= ("Unchanged baseline requires Linux; user deferred Linux execution." :: Text)], Nothing)
              else
                if field "status" compared /= Right (String "match")
                  then pure (provider, insertFields base ["status" .= ("execution-" <> case field "status" compared of Right (String value) -> value; _ -> "unavailable" :: Text), "provider" .= provider, "differences" .= getValue "differences" compared], Nothing)
                  else pure (provider, base, Just command)
      case providerResults of
        [("baseline", baselineValue, Just baselineCommand), ("candidate", candidateValue, Just candidateCommand)] -> do
          let firstRun = object ["baseline" .= getValue "baseline" (getValue "first_run" baselineValue), "candidate" .= getValue "candidate" (getValue "first_run" candidateValue)]
              ready = insertFields candidateValue ["baseline_translation" .= getValue "baseline_translation" baselineValue, "first_run" .= firstRun]
          sampleProviders providers inputs cwd env reference fixture directory baselineCommand candidateCommand ready
        (_, failure, Nothing) : _ | field "status" failure /= Left "missing JSON key: status" -> pure failure
        _ -> pure initial

    sampleProviders providers inputs cwd env reference fixture directory baselineCommand candidateCommand ready = do
      let commands = [("baseline", baselineCommand), ("candidate", candidateCommand)]
      observations <-
        foldM
          ( \prior iteration -> do
              let order = if iteration < (3 :: Int) || even (iteration - 3) then commands else reverse commands
              foldM
                ( \saved (provider, _) -> do
                    let generated = directory </> provider <> ".fish"
                        providerRuntime = providers </> if provider == "baseline" then "baseline_runtime" else "runtime"
                        launcher = if provider == "candidate" then Just providerRuntime else Nothing
                    (observed, command, _) <- portableExecute (providers </> "fish") True launcher fixture generated inputs cwd env 30
                    record <- observationRecord directory (provider <> "-" <> pad2 iteration) command observed
                    let savedRecord = insertFields record ["provider" .= provider, "warmup" .= (iteration < 3), "elapsed_ns" .= elapsedNs observed]
                    pure (saved <> [(provider, iteration, observed, savedRecord)])
                )
                prior
                order
          )
          []
          [0 :: Int .. 22]
      let valid = all (\(_, _, observed, _) -> field "status" (compareObservations reference observed) == Right (String "match")) observations
          samples name = [elapsedNs observed | (provider, iteration, observed, _) <- observations, provider == name, iteration >= 3]
          sampled = insertFields ready ["status" .= (if valid then "measured" else "sample-mismatch" :: Text), "samples_ns" .= object ["baseline" .= samples "baseline", "candidate" .= samples "candidate"], "observations" .= [value | (_, _, _, value) <- observations]]
      pure sampled

    getValue key value = fromRight Null (field key value)
    pad3 index = let digits = show index in replicate (max 0 (3 - length digits)) '0' <> digits
    pad2 index = let digits = show index in replicate (max 0 (2 - length digits)) '0' <> digits

require :: Either String a -> IO a
require = either fail pure

insertField :: Text -> Value -> Value -> Value
insertField key value (Object fields) = Object (KM.insert (fromString (T.unpack key)) value fields)
insertField _ _ value = value

copyTree :: FilePath -> FilePath -> IO ()
copyTree source target = do
  createDirectoryIfMissing True target
  names <- listDirectory source
  forM_ names $ \name -> do
    let from = source </> name
        to = target </> name
    directory <- doesDirectoryExist from
    if directory then copyTree from to else copyFileWithMetadata from to

targetedSources :: [(String, String)]
targetedSources =
  [ ("field-splitting", "x=\"one two three\"\nprintf \"<%s>\\n\" $x\n"),
    ("escaped-echo", "echo -e \"a\\tb\\nend\"\n"),
    ("composed-words", "x=\"one two\"\nprintf \"<%s>\\n\" pre${x}post\n"),
    ("checked-printf", "printf \"%08d:%s\\n\" 42 value\n")
  ]

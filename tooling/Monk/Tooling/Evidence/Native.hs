-- | Frozen native runtime coverage and performance evidence.
module Monk.Tooling.Evidence.Native
  ( freezeNative,
    measureNative,
    nativeEnvironment,
    nativeExecute,
    nativeSame,
    validateHistoric95,
    validateCommon16,
  )
where

import Control.Monad (foldM)
import Data.Aeson (Result (..), Value (..), fromJSON, object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Pair)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.List (lookup)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Monk.Tooling.Evidence.Common (Observation (..), arrayField, base64, boolField, cleanEnvironment, digestFile, field, hostPlatform, nativeRecord, numberField, readJson, runObservation, textField, unbase64, writeJson)
import Monk.Tooling.Evidence.Freeze (freezeWithShake)
import System.Directory (canonicalizePath, createDirectory, createDirectoryIfMissing, doesFileExist, getCurrentDirectory)
import System.Environment (getExecutablePath)
import System.FilePath (replaceExtension, (</>))
import System.Posix.Files (createSymbolicLink, readSymbolicLink)

nativeEnvironment :: FilePath -> IO [(String, String)]
nativeEnvironment output = do
  let directory = output </> "providers"
      adapter = directory </> "python3"
  createDirectoryIfMissing True directory
  executable <- getExecutablePath >>= canonicalizePath
  exists <- doesFileExist adapter
  if exists
    then do
      target <- readSymbolicLink adapter
      unless (target == executable) $ fail "historical Python adapter changed"
    else createSymbolicLink executable adapter
  env <- cleanEnvironment output
  pure $ ("PATH", directory <> ":" <> fromMaybe "" (lookup "PATH" env)) : filter ((/= "PATH") . fst) env

nativeExecute :: [String] -> FilePath -> B.ByteString -> [(String, String)] -> IO Observation
nativeExecute command cwd input env = do
  result <- runObservation command input cwd env 60
  when (status result == "timeout") $ fail ("Measurement timed out: " <> show command)
  pure result

nativeSame :: Observation -> Observation -> Bool
nativeSame left right = exit left == exit right && observedStdout left == observedStdout right && observedStderr left == observedStderr right

freezeNative :: FilePath -> FilePath -> FilePath -> FilePath -> IO Value
freezeNative output baseline bash fish = do
  cwd <- getCurrentDirectory >>= canonicalizePath
  freezeWithShake
    (output </> "baseline-cohort.json")
    [(cwd, ["test/fixtures//*", "benchmark/fixtures//*"])]
    [cwd </> "docs/evidence/bakeoff-2026-09-09.json", cwd </> "docs/evidence/portable-exact-cohorts-2026-09-22.json", cwd </> "docs/evidence/frozen95/background-jobs.bash", output </> "frozen-arithmetic.json", baseline, bash, fish]
    (void (freezeNativeIO output baseline bash fish))
  manifest <- readJson (output </> "baseline-cohort.json")
  rows <- either fail pure (arrayField "fixtures" manifest)
  pure $ object ["frozen" .= length rows, "historic_denominator" .= (95 :: Int)]

freezeNativeIO :: FilePath -> FilePath -> FilePath -> FilePath -> IO Value
freezeNativeIO output baseline bash fish = do
  cwd <- getCurrentDirectory >>= canonicalizePath
  previous <- readJson (cwd </> "docs/evidence/bakeoff-2026-09-09.json")
  historical <- require (arrayField "fixtures" previous)
  validateHistoric95 cwd historical
  arithmetic <- readJson (output </> "frozen-arithmetic.json") >>= require . arrayField "cases"
  let extras = map arithmeticFixture arithmetic
      fixtures = historical <> extras
      frozen = output </> "baseline"
  createDirectory frozen
  env <- nativeEnvironment output
  rows <- forM (zip [0 :: Int ..] fixtures) $ \(index, fixture) -> do
    name <- T.unpack <$> require (textField "fixture" fixture)
    expected <- require (textField "input_sha256" fixture)
    actual <- digestFile (nativeSource cwd name)
    unless (expected == T.pack actual) $ fail ("Frozen input changed: " <> name)
    let inputPath = replaceExtension (cwd </> name) "stdin"
    exists <- doesFileExist inputPath
    input <- if exists then B.readFile inputPath else pure ""
    translation <- nativeTranslation baseline fixture (frozen </> show index <> ".fish") cwd env []
    pure $ insertFields fixture ["index" .= index, "stdin_base64" .= base64 input, "baseline_translation" .= translation]
  common <- forM historical $ \fixture -> do
    name <- require (textField "fixture" fixture)
    tools <- require (field "tools" fixture)
    matched <- and <$> traverse (\tool -> do value <- require (field tool tools); pure (field "status" value == Right (String "match"))) ["monk", "babelfish"]
    pure [name | matched]
  validateCommon16 cwd (map String (concat common))
  baselineHash <- digestFile baseline
  bashHash <- digestFile bash
  fishHash <- digestFile fish
  adapterHash <- digestFile (output </> "providers/python3")
  let manifest =
        object
          [ "cwd" .= cwd,
            "baseline_binary_sha256" .= baselineHash,
            "bash_sha256" .= bashHash,
            "fish_sha256" .= fishHash,
            "historic_count" .= (95 :: Int),
            "common16" .= concat common,
            "fixtures" .= rows,
            "historical_python_adapter" .= object ["path" .= (output </> "providers/python3"), "sha256" .= adapterHash, "scope" .= ("Frozen background-jobs.bash command only" :: Text)]
          ]
  writeJson (output </> "baseline-cohort.json") manifest
  pure $ object ["frozen" .= length rows, "historic_denominator" .= (95 :: Int)]
  where
    arithmeticFixture caseValue =
      object
        [ "fixture" .= getValue "input" caseValue,
          "metadata" .= object ["fixtureMetaArgs" .= getValue "argv" caseValue, "fixtureMetaRecursive" .= False, "fixtureMetaMode" .= ("ShellRunExec" :: Text)],
          "input_sha256" .= getValue "input_sha256" caseValue,
          "extra_arithmetic" .= True
        ]
    getValue key value = fromRight Null (field key value)

nativeTranslation :: FilePath -> Value -> FilePath -> FilePath -> [(String, String)] -> [String] -> IO Value
nativeTranslation binary fixture generated cwd env extra = do
  name <- T.unpack <$> require (textField "fixture" fixture)
  metadata <- require (field "metadata" fixture)
  recursive <- require (boolField "fixtureMetaRecursive" metadata)
  let command = [binary, nativeSource cwd name] <> (if recursive then ["--recursive", "--sources", "inline"] else []) <> extra
  observed <- nativeExecute command cwd "" env
  B.writeFile generated (observedStdout observed)
  B.writeFile (replaceExtension generated "diagnostics") (observedStderr observed)
  hash <- digestFile generated
  pure $ object ["exit" .= exit observed, "bytes" .= B.length (observedStdout observed), "sha256" .= hash, "command" .= command]

measureNative :: Bool -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> Text -> IO Value
measureNative coverage output baseline candidate runtime bash fish fingerprint = do
  cwd <- getCurrentDirectory >>= canonicalizePath
  manifest <- readJson (output </> "baseline-cohort.json")
  rows <- require (arrayField "fixtures" manifest)
  validateHistoric95 cwd (take 95 rows)
  forM_ [(baseline, "baseline_binary_sha256"), (bash, "bash_sha256"), (fish, "fish_sha256")] $ \(path, key) -> do
    expected <- require (textField key manifest)
    actual <- digestFile path
    unless (expected == T.pack actual) $ fail ("Frozen provider changed: " <> path)
  let directory = output </> if coverage then "coverage" else "measurement"
  createDirectory directory
  env <- nativeEnvironment output
  common <- require (arrayField "common16" manifest)
  validateCommon16 cwd common
  measured <-
    foldM
      ( \prior fixture -> do
          result <- measureFixture coverage cwd output directory baseline candidate runtime bash fish env common fixture
          let updated = prior <> [result]
          writeJson (directory </> "partial.json") (toJSON updated)
          pure updated
      )
      []
      rows
  totals <- nativeTotals measured
  candidateHash <- digestFile candidate
  runtimeHash <- digestFile runtime
  if coverage
    then do
      executable <- getExecutablePath >>= canonicalizePath
      collectorHash <- digestFile executable
      let report =
            object
              [ "source_fingerprint" .= fingerprint,
                "candidate_sha256" .= candidateHash,
                "runtime_sha256" .= runtimeHash,
                "measurement_script_sha256" .= collectorHash,
                "note" .= ("Preliminary coverage executions under concurrent build load; elapsed values are not acceptance or first-run performance measurements." :: Text),
                "totals" .= totals,
                "fixtures" .= measured
              ]
      writeJson (directory </> "report.json") report
      pure totals
    else do
      let timedNames = [getValue "fixture" fixture | fixture <- rows, getValue "fixture" fixture `elem` common || field "extra_arithmetic" fixture == Right (Bool True)]
          completed row = all (\provider -> case field "samples_ns" row >>= arrayField provider of Right samples -> length samples == 20; _ -> False) ["baseline", "candidate"]
          measuredTimed = [row | row <- measured, getValue "fixture" row `elem` timedNames, completed row]
      unless (length measuredTimed == length timedNames) $ fail "Timed native cohorts are incomplete; no aggregate or acceptance gate can be reported"
      timestamp <- formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q+00:00" <$> getCurrentTime
      report <- nativeMeasurementReport cwd timestamp output baseline candidate runtime fingerprint rows measured common totals env
      writeJson (directory </> "report.json") report
      pure $ object ["totals" .= totals, "gates" .= getValue "gates" report]
  where
    getValue key value = fromRight Null (field key value)

validateHistoric95 :: FilePath -> [Value] -> IO ()
validateHistoric95 cwd fixtures = do
  durable <- readJson (cwd </> "docs/evidence/portable-exact-cohorts-2026-09-22.json")
  historicDenominator <- require (numberField "historical_denominator" durable)
  expectedRows <- require (arrayField "historic95" durable)
  let expectedSignature row = (,,,) <$> textField "fixture" row <*> textField "input_sha256" row <*> field "metadata" row <*> textField "stdin_base64" row
  expected <- require (traverse expectedSignature expectedRows)
  actual <- forM fixtures $ \row -> do
    name <- require (textField "fixture" row)
    hash <- require (textField "input_sha256" row)
    metadata <- require (field "metadata" row)
    input <- case field "stdin_base64" row of
      Right (String value) -> pure value
      _ -> do
        let inputPath = replaceExtension (cwd </> T.unpack name) "stdin"
        exists <- doesFileExist inputPath
        bytes <- if exists then B.readFile inputPath else pure ""
        pure (base64 bytes)
    pure (name, hash, metadata, input)
  unless (historicDenominator == 95 && length expected == 95 && length actual == 95 && actual == expected && Set.size (Set.fromList [name | (name, _, _, _) <- actual]) == 95) $
    fail "frozen95 cohort count, order, source hashes, metadata or stdin changed"

validateCommon16 :: FilePath -> [Value] -> IO ()
validateCommon16 cwd actual = do
  durable <- readJson (cwd </> "docs/evidence/portable-exact-cohorts-2026-09-22.json")
  cohorts <- require (arrayField "performance_cohorts" durable)
  expected <- forM [row | row <- cohorts, field "performance_cohort" row == Right (String "common16")] $ \row -> String <$> require (textField "fixture" row)
  unless (length expected == 16 && length actual == 16 && actual == expected && Set.size (Set.fromList [name | String name <- actual]) == 16) $
    fail "common16 cohort membership or order changed"

measureFixture :: Bool -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> [(String, String)] -> [Value] -> Value -> IO Value
measureFixture coverage cwd output directory _baseline candidate runtime bash fish env common fixture = do
  name <- T.unpack <$> require (textField "fixture" fixture)
  index <- require (numberField "index" fixture)
  expected <- require (textField "input_sha256" fixture)
  actual <- digestFile (nativeSource cwd name)
  unless (expected == T.pack actual) $ fail ("Frozen fixture hash differs: " <> name)
  baselineTranslation <- require (field "baseline_translation" fixture)
  baselineHash <- require (textField "sha256" baselineTranslation)
  savedHash <- digestFile (output </> "baseline" </> show index <> ".fish")
  unless (baselineHash == T.pack savedHash) $ fail ("Frozen translation changed: " <> name)
  metadata <- require (field "metadata" fixture)
  argv <- require (arrayField "fixtureMetaArgs" metadata >>= traverse asString)
  stdinText <- require (textField "stdin_base64" fixture)
  input <- require (unbase64 stdinText)
  let historic = index < 95
      base = ["fixture" .= name, "historic" .= historic]
  modes <- forM ["default", "stable"] $ \mode -> do
    let generated = directory </> show index <> "-" <> mode <> ".fish"
        extra = ["--runtime", runtime] <> (if mode == "stable" then ["--directory-contract", "stable"] else [])
    translation <- nativeTranslation candidate fixture generated cwd env extra
    pure (mode, translation, generated)
  let admitted = [(mode, translation, generated) | (mode, translation, generated) <- modes, field "exit" translation == Right (Number 0)]
  reference <- if null admitted then pure Nothing else Just <$> nativeExecute ([bash, "--noprofile", "--norc", nativeSource cwd name] <> argv) cwd input env
  checkedModes <- forM modes $ \(mode, translation, generated) -> do
    let matching = filter (\(selected, _, _) -> selected == mode) admitted
    case (reference, matching) of
      (Just baselineObservation, _ : _) -> do
        observed <- nativeExecute ([fish, "--no-config", generated] <> argv) cwd input env
        pure (mode, object ["translation" .= translation, "execution" .= nativeRecord observed, "matched" .= nativeSame baselineObservation observed])
      _ -> pure (mode, object ["translation" .= translation])
  let modeValue = object [fromString mode .= value | (mode, value) <- checkedModes]
      initial = insertFields (object base) (["modes" .= modeValue] <> maybe [] (\observation -> ["bash" .= nativeRecord observation]) reference)
      timed = String (T.pack name) `elem` common || field "extra_arithmetic" fixture == Right (Bool True)
  if coverage || not timed then pure initial else timeNativeFixture cwd output directory fish env fixture argv input reference checkedModes initial

timeNativeFixture :: FilePath -> FilePath -> FilePath -> FilePath -> [(String, String)] -> Value -> [String] -> B.ByteString -> Maybe Observation -> [(String, Value)] -> Value -> IO Value
timeNativeFixture cwd output directory fish env fixture argv input reference modes initial = do
  index <- require (numberField "index" fixture)
  name <- T.unpack <$> require (textField "fixture" fixture)
  baselineTrans <- require (field "baseline_translation" fixture)
  let defaultAccepted = case lookup "default" modes of
        Just value -> (field "translation" value >>= field "exit") == Right (Number 0)
        Nothing -> False
  unless (field "exit" baselineTrans == Right (Number 0) && defaultAccepted) $ fail ("Timed translation rejected: " <> name)
  baselineObservation <- maybe (fail "timed fixture has no Bash execution") pure reference
  let baselineCommand = [fish, "--no-config", output </> "baseline" </> show index <> ".fish"] <> argv
      commands :: [(String, [String])]
      commands = [("baseline", baselineCommand), ("candidate", [fish, "--no-config", directory </> show index <> "-default.fish"] <> argv)]
  initialBaseline <- nativeExecute baselineCommand cwd input env
  unless (nativeSame baselineObservation initialBaseline) $ fail ("First run mismatch: " <> name)
  forM_ [1 :: Int .. 3] $ \_ -> forM_ commands $ \(_, command) -> do
    observed <- nativeExecute command cwd input env
    unless (nativeSame baselineObservation observed) $ fail ("Warmup mismatch: " <> name)
  samples <- forM [0 :: Int .. 19] $ \iteration -> forM (if even iteration then commands else reverse commands) $ \(provider, command) -> do
    observed <- nativeExecute command cwd input env
    unless (nativeSame baselineObservation observed) $ fail ("Sample mismatch: " <> name)
    pure (provider, elapsedNs observed)
  let times provider = [nanoseconds | sample <- samples, (sampleProvider, nanoseconds) <- sample, sampleProvider == provider]
      baselineTimes = times "baseline"
      candidateTimes = times "candidate"
  pure $
    insertFields
      initial
      [ "first_run" .= object ["baseline" .= nativeRecord initialBaseline, "candidate" .= getDefaultExecution modes],
        "samples_ns" .= object ["baseline" .= baselineTimes, "candidate" .= candidateTimes],
        "median_ns" .= object ["baseline" .= median baselineTimes, "candidate" .= median candidateTimes]
      ]
  where
    getDefaultExecution variants = case lookup "default" variants of
      Just value -> fromRight Null (field "execution" value)
      _ -> Null

nativeTotals :: [Value] -> IO Value
nativeTotals rows = do
  modes <- forM ["default", "stable"] $ \mode -> do
    let historic = filter ((== Right (Bool True)) . field "historic") rows
        observations = map (field "modes" >=> field mode) historic
        admitted = length [() | Right observation <- observations, (field "translation" observation >>= field "exit") == Right (Number 0)]
        matched = length [() | Right observation <- observations, field "matched" observation == Right (Bool True)]
        mismatches = length [() | Right observation <- observations, field "matched" observation == Right (Bool False)]
    pure (mode, object ["admitted" .= admitted, "matched" .= matched, "admitted_mismatches" .= mismatches])
  pure $ object [fromString (T.unpack name) .= value | (name, value) <- modes]

nativeMeasurementReport :: FilePath -> String -> FilePath -> FilePath -> FilePath -> FilePath -> Text -> [Value] -> [Value] -> [Value] -> Value -> [(String, String)] -> IO Value
nativeMeasurementReport cwd _timestamp output baseline candidate runtime fingerprint frozen rows common totals env = do
  baselineHash <- digestFile baseline
  candidateHash <- digestFile candidate
  runtimeHash <- digestFile runtime
  runtimeBytes <- B.length <$> B.readFile runtime
  adapterHash <- digestFile (output </> "providers/python3")
  description <- nativeExecute [runtime, "--describe"] cwd "" env
  linkage <- nativeExecute ["ldd", runtime] cwd "" env
  platform <- hostPlatform
  let groups = [("common16", [row | row <- rows, field "fixture" row `elem` map Right common]), ("arithmetic", [row | row <- rows, field "historic" row == Right (Bool False)])]
      aggregates = object [fromString name .= nativeAggregate cohortRows | (name, cohortRows) <- groups]
      getValue key value = fromRight Null (field key value)
      doubleValue key value = case field key value of
        Right (Number n) -> case fromJSON (Number n) of Success result -> result; Error _ -> 0
        _ -> 0 :: Double
      matched mode = fromRight 0 (field mode totals >>= numberField "matched")
      mismatches mode = fromRight 0 (field mode totals >>= numberField "admitted_mismatches")
      originalMatches = [name | fixture <- take 95 frozen, let name = getValue "fixture" fixture, (field "tools" fixture >>= field "monk" >>= field "status") == Right (String "match")]
      retained = all (\name -> any (\row -> field "fixture" row == Right name && all (\mode -> (field "modes" row >>= field mode >>= field "matched") == Right (Bool True)) ["default", "stable"]) rows) originalMatches
      large = fromMaybe Null (find ((== Right (String "benchmark/fixtures/large-exact.bash")) . field "fixture") rows)
      oldLarge = fromMaybe Null (find ((== Right (String "benchmark/fixtures/large-exact.bash")) . field "fixture") frozen)
      largeBytes = fromRight 0 (field "modes" large >>= field "default" >>= field "translation" >>= numberField "bytes")
      oldBytes = fromRight 0 (field "baseline_translation" oldLarge >>= numberField "bytes")
      byteRatio = if oldBytes > 0 then fromIntegral largeBytes / fromIntegral oldBytes else 1 / 0 :: Double
      arithmeticRatio = doubleValue "baseline_over_candidate" (getValue "arithmetic" aggregates)
      commonRatio = doubleValue "baseline_over_candidate" (getValue "common16" aggregates)
      gates =
        object
          [ "default_at_least_45" .= (matched "default" >= 45),
            "stable_at_least_48" .= (matched "stable" >= 48),
            "zero_admitted_mismatches" .= (mismatches "default" == 0 && mismatches "stable" == 0),
            "retains_original_exact" .= retained,
            "large_exact_at_most_25_percent" .= (byteRatio <= 0.25),
            "arithmetic_at_least_2x" .= (arithmeticRatio >= 2),
            "common16_at_most_10_percent_regression" .= (commonRatio >= 1 / 1.1)
          ]
  pure $
    object
      [ "source_fingerprint" .= fingerprint,
        "cwd" .= cwd,
        "platform" .= platform,
        "baseline_sha256" .= baselineHash,
        "candidate_sha256" .= candidateHash,
        "runtime" .= object ["sha256" .= runtimeHash, "bytes" .= runtimeBytes, "abi" .= (1 :: Int), "describe" .= nativeRecord description, "link_dependencies" .= C.unpack (observedStdout linkage)],
        "protocol" .= object ["warmups" .= (3 :: Int), "samples" .= (20 :: Int), "serial" .= True, "alternating_order" .= True, "first_run" .= ("initial idle-load performance observation before three warmups, after preliminary coverage; not OS cold-cache measurement" :: Text)],
        "process_launches" .= object ["status" .= ("unmeasured" :: Text), "reason" .= ("ptrace previously denied; static calls are not process launches" :: Text)],
        "historical_python_adapter" .= object ["path" .= (output </> "providers/python3"), "sha256" .= adapterHash, "scope" .= ("Frozen background-jobs.bash command only" :: Text)],
        "totals" .= totals,
        "aggregates" .= aggregates,
        "gates" .= gates,
        "large_exact_fish_ratio" .= byteRatio,
        "bundle_bytes" .= object ["native_executable" .= runtimeBytes, "large_exact_fish" .= largeBytes, "large_exact_plus_native" .= (largeBytes + runtimeBytes)],
        "fixtures" .= rows
      ]

nativeAggregate :: [Value] -> Value
nativeAggregate rows =
  let sample name index =
        sum
          [ fromRight
              0
              ( field "samples_ns" row >>= field name >>= \case
                  Array values -> case drop index (toList values) of
                    Number n : _ -> Right (round n :: Integer)
                    _ -> Left "sample absent"
                  _ -> Left "sample absent"
              )
          | row <- rows
          ]
      baseline = [sample "baseline" index | index <- [0 .. 19]]
      candidate = [sample "candidate" index | index <- [0 .. 19]]
      baselineMedian = median baseline
      candidateMedian = median candidate
   in object ["sample_totals_ns" .= object ["baseline" .= baseline, "candidate" .= candidate], "median_ns" .= object ["baseline" .= baselineMedian, "candidate" .= candidateMedian], "baseline_over_candidate" .= (baselineMedian / candidateMedian)]

median :: (Integral a) => [a] -> Double
median values = case drop 9 (sort values) of
  left : right : _ -> (fromIntegral left + fromIntegral right) / 2
  _ -> 0

insertFields :: Value -> [Pair] -> Value
insertFields (Object original) pairs = case object pairs of
  Object additional -> Object (KM.union additional original)
  _ -> Object original
insertFields value _ = value

require :: Either String a -> IO a
require = either fail pure

asString :: Value -> Either String String
asString (String value) = Right (T.unpack value)
asString _ = Left "expected JSON string"

nativeSource :: FilePath -> FilePath -> FilePath
nativeSource cwd name
  | name == "test/fixtures/integration/background-jobs.bash" = cwd </> "docs/evidence/frozen95/background-jobs.bash"
  | otherwise = cwd </> name

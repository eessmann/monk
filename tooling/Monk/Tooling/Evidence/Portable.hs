-- | Freeze separate exact-effect cohorts and independently compare providers.
module Monk.Tooling.Evidence.Portable
  ( strengthenedCases,
    freezePortable,
    measurePortable,
    PortableOptions (..),
    runWorker,
    runHistoricalPython,
    portableTranslate,
    portableExecute,
    candidateEntry,
    insertFields,
  )
where

import Control.Exception (evaluate)
import Control.Monad (foldM)
import Data.Aeson (Value (..), eitherDecodeStrict', object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Pair)
import Data.Bits ((.|.))
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.List (lookup, nub)
import Data.Text qualified as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Monk.Runtime.Digest (sha256)
import Monk.Tooling.Evidence.Common (Observation (..), arrayField, base64, cleanEnvironment, compareEffects, copyTree, digestFile, field, hostPlatform, observationRecord, provenance, runObservation, snapshot, textField, unbase64, writeJson)
import Monk.Tooling.Evidence.Freeze (freezeWithShake)
import Monk.Tooling.Evidence.Native (comparisonCorpusPath, readComparisonCorpus, validateCommon16, validateHistoric95)
import Monk.Tooling.Evidence.Profile (runProfile)
import System.Directory (canonicalizePath, copyFileWithMetadata, createDirectory, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, doesPathExist, removeDirectoryRecursive)
import System.Environment (getExecutablePath)
import System.Exit qualified as Exit
import System.FilePath (replaceExtension, takeDirectory, (</>))
import System.IO qualified as IO
import System.Posix.Files (createNamedPipe, createSymbolicLink, ownerReadMode, ownerWriteMode)

strengthenedCases :: [(String, String, B.ByteString)]
strengthenedCases =
  [ ("read-array-live-ifs", "IFS=' :'; read -r -a values; observed=$?; printf 'status:%s\\n' \"$observed\"; printf 'field:<%s>\\n' \"${values[@]}\"\n", " alpha:beta:: gamma \n"),
    ("read-eof-without-newline", "IFS= read -r value; observed=$?; printf 'status:%s\\nvalue:<%s>\\n' \"$observed\" \"$value\"\n", "partial value"),
    ("read-nul-invalid-bytes", "IFS= read -r value; observed=$?; printf 'status:%s\\nvalue:<%s>\\n' \"$observed\" \"$value\"\n", B.pack [97, 0, 255, 98, 10]),
    ("dense-array-append-observed", "values=('first item' '' third); values+=('last item' '*'); printf '<%s>\\n' \"${values[@]}\"\n", ""),
    ("visible-time-output", "TIMEFORMAT=measured; time { printf timed; }\n", "")
  ]

freezePortable :: FilePath -> FilePath -> String -> IO Value
freezePortable repo output contract = do
  occupied <- doesPathExist output
  when occupied $ fail ("immutable evidence output already exists: " <> output)
  freezeWithShake
    (output </> "manifest.json")
    [(repo, ["test/fixtures//*", "benchmark/fixtures//*"])]
    [repo </> comparisonCorpusPath, repo </> "test/evidence/frozen95/background-jobs.bash"]
    (void (freezePortableIO repo output contract))
  pure $ object ["historic" .= (95 :: Int), "effects" .= (4 :: Int), "strengthened" .= (5 :: Int)]

freezePortableIO :: FilePath -> FilePath -> String -> IO Value
freezePortableIO repo output contract = do
  let inputs = output </> "inputs"
  createDirectory inputs
  forM_ ["test/fixtures", "benchmark/fixtures"] $ \folder -> do
    createDirectoryIfMissing True (takeDirectory (inputs </> folder))
    copyTree (repo </> folder) (inputs </> folder)
  corpus <- readComparisonCorpus repo
  corpusHash <- digestFile (repo </> comparisonCorpusPath)
  oldRows <- require (arrayField "historic95" corpus)
  validateHistoric95 repo oldRows
  historical <- forM oldRows $ \fixture -> do
    name <- T.unpack <$> require (textField "fixture" fixture)
    expected <- require (textField "input_sha256" fixture)
    initial <- digestFile (inputs </> name)
    when (expected /= T.pack initial && name == "test/fixtures/integration/background-jobs.bash") $ do
      let archived = repo </> "test/evidence/frozen95/background-jobs.bash"
      archivedHash <- digestFile archived
      unless (expected == T.pack archivedHash) $ fail "Archived frozen95 background-jobs fixture hash differs"
      copyFileWithMetadata archived (inputs </> name)
    actual <- digestFile (inputs </> name)
    unless (expected == T.pack actual) $ fail ("Frozen95 input changed: " <> name)
    let inputFile = replaceExtension (inputs </> name) "stdin"
    present <- doesFileExist inputFile
    input <- if present then B.readFile inputFile else pure ""
    metadata <- require (field "metadata" fixture)
    pure $ object ["fixture" .= name, "cohort" .= ("historic95" :: Text), "input_sha256" .= expected, "metadata" .= metadata, "stdin_base64" .= base64 input]
  validateHistoric95 repo historical
  let extra = inputs </> "effects"
  createDirectory extra
  effects <- forM effectCases $ \(cohort, name, source) -> do
    let path = extra </> name <> ".bash"
    B.writeFile path (C.pack source)
    hash <- digestFile path
    pure $
      object
        [ "fixture" .= ("effects/" <> name <> ".bash"),
          "cohort" .= cohort,
          "input_sha256" .= hash,
          "stdin_base64" .= ("" :: Text),
          "metadata" .= object ["fixtureMetaArgs" .= ([] :: [Text]), "fixtureMetaRecursive" .= False]
        ]
  B.writeFile (extra </> "caller.json") "{\"version\":1,\"exportedFunctions\":[\"f\"],\"ambientEffects\":\"none\"}\n"
  let strengthened = inputs </> "strengthened"
  createDirectory strengthened
  controls <- forM strengthenedCases $ \(name, source, input) -> do
    let path = strengthened </> name <> ".bash"
    B.writeFile path (C.pack source)
    B.writeFile (replaceExtension path "stdin") input
    hash <- digestFile path
    pure $
      object
        [ "fixture" .= ("strengthened/" <> name <> ".bash"),
          "cohort" .= ("strengthened" :: Text),
          "input_sha256" .= hash,
          "stdin_base64" .= base64 input,
          "metadata" .= object ["fixtureMetaArgs" .= ([] :: [Text]), "fixtureMetaRecursive" .= False, "fixtureMetaHasStdin" .= not (B.null input)]
        ]
  tree <- snapshot inputs
  common <- require (arrayField "common16" corpus)
  arithmetic <- require (arrayField "arithmetic3" corpus)
  let manifest =
        object
          [ "schema" .= (2 :: Int),
            "directory_contract" .= object ["lane" .= contract, "applies_to" .= (["current", "candidate"] :: [Text])],
            "candidate_entry" .= candidateEntry,
            "historic_denominator" .= (95 :: Int),
            "corpus_sha256" .= corpusHash,
            "fixtures" .= (historical <> effects <> controls),
            "input_tree" .= tree,
            "common16" .= common,
            "arithmetic3" .= arithmetic
          ]
  validateCommon16 repo common
  writeJson (output </> "manifest.json") manifest
  pure $ object ["historic" .= (95 :: Int), "effects" .= (4 :: Int), "strengthened" .= (5 :: Int)]

effectCases :: [(Text, String, String)]
effectCases =
  [ ("filesystem", "bytes-append-mode", "printf '\\000\\377A' > output; printf 'B\\n' >> output; chmod 640 output; cat output\n"),
    ("caller", "function-caller-state", "f() { local value=inside; echo \"$value\"; }; echo sourced\n"),
    ("process", "background-handshake", "\"$1\" evidence worker producer &\npid=$!\ncat ready.fifo\nprintf \"go\\n\" > go.fifo\nwait \"$pid\"\ncat events\n"),
    ("process", "large-process-substitution", "cat <(\"$1\" evidence worker stream)\n")
  ]

runWorker :: String -> IO ()
runWorker action = case action of
  "stream" -> B.hPut IO.stdout (B.concat (replicate 4096 (B.pack [0 .. 255])))
  "producer" -> do
    B.writeFile "events" "ready\n"
    IO.withBinaryFile "ready.fifo" IO.WriteMode (`B.hPut` "ready\n")
    response <- IO.withBinaryFile "go.fifo" IO.ReadMode C.hGetLine
    unless (response == "go") $ fail "worker handshake did not receive go"
    B.appendFile "events" "released\n"
  _ -> fail "worker action must be producer or stream"

runHistoricalPython :: [String] -> IO ()
runHistoricalPython arguments = do
  unless (arguments == ["-c", "import sys; sys.stdin.read(); raise SystemExit(7)"]) $
    fail "historical Python adapter accepts only the frozen background-jobs command"
  input <- B.hGetContents IO.stdin
  _ <- evaluate (B.length input)
  Exit.exitWith (Exit.ExitFailure 7)

candidateEntry :: Value
candidateEntry = object ["standalone" .= ("runtime --abi 2 launch SCRIPT ARGS" :: Text), "sourceable" .= ("fish --no-config caller observer" :: Text)]

measurePortable :: PortableOptions -> IO Value
measurePortable options = do
  manifestBytes <- B.readFile (frozenDirectory options </> "manifest.json")
  manifest <- either fail pure (eitherDecodeStrict' manifestBytes)
  let inputs = frozenDirectory options </> "inputs"
  tree <- snapshot inputs
  unless (field "input_tree" manifest == Right tree) $ fail "Frozen input tree changed"
  receiptHash <- case stage options of
    "final" -> do
      path <- maybe (fail "Final comparison requires a successful stable build receipt") pure (buildReceipt options)
      raw <- B.readFile path
      receipt <- either fail pure (eitherDecodeStrict' raw)
      unless (field "schema" receipt == Right (Number 2) && field "successful_stable_command" receipt == Right (Bool True)) $ fail "Build receipt is unsuccessful or unstable"
      production <- require (field "production_after" receipt >>= textField "sha256")
      unless (production == sourceFingerprint options) $ fail "Build receipt has a different source fingerprint"
      binaries <- require (arrayField "binaries" receipt)
      forM_ [candidateBinary options, candidateRuntime options] $ \binaryPath -> do
        resolved <- canonicalizePath binaryPath
        hash <- digestFile binaryPath
        unless (any (\row -> field "path" row == Right (String (T.pack resolved)) && field "sha256" row == Right (String (T.pack hash))) binaries) $ fail ("Build receipt does not identify provider: " <> binaryPath)
      pure (Just (C.unpack (sha256 raw), receipt))
    "preliminary" -> pure Nothing
    _ -> fail "stage must be preliminary or final"
  let output = outputDirectory options
      providers = output </> "providers"
  createDirectoryIfMissing True (takeDirectory output)
  createDirectory output
  createDirectory providers
  executable <- getExecutablePath >>= canonicalizePath
  createSymbolicLink executable (providers </> "python3")
  adapterHash <- digestFile executable
  supplied <- forM
    [ ("original", originalBinary options),
      ("current", currentBinary options),
      ("candidate", candidateBinary options),
      ("babelfish", babelfishBinary options),
      ("current_runtime", currentRuntime options),
      ("runtime", candidateRuntime options)
    ]
    $ \(name, raw) -> do
      source <- canonicalizePath raw
      copyFileWithMetadata source (providers </> name)
      pure (name, source)
  forM_ ["candidate", "runtime"] $ \name -> do
    let source = fromMaybe "" (lookup name supplied)
    copied <- digestFile (providers </> name)
    originalHash <- digestFile source
    unless (copied == originalHash) $ fail ("Provider changed while copying: " <> name)
  bash <- canonicalizePath (bashBinary options)
  fish <- canonicalizePath (fishBinary options)
  baseEnv <- cleanEnvironment output
  let pathValue = providers <> ":" <> takeDirectory bash <> ":" <> takeDirectory fish <> ":" <> fromMaybe "" (lookup "PATH" baseEnv)
      env = ("PATH", pathValue) : filter ((/= "PATH") . fst) baseEnv
  profile <- runProfile bash fish
  platform <- hostPlatform
  let candidateRuntimePath = providers </> "runtime"
      currentRuntimePath = providers </> "current_runtime"
      cwd = output </> "execution-cwd"
  currentProbe <- runObservation [currentRuntimePath, "--describe"] "" inputs env (timeoutSeconds options)
  currentProbeRecord <- observationRecord output "current-runtime-describe" [currentRuntimePath, "--describe"] currentProbe
  tools <- forM ["original", "current", "candidate", "babelfish"] $ \name -> do
    provider <- provenance (providers </> name)
    let suppliedPath = fromMaybe "" (lookup name supplied)
    pure (name, insertFields provider ["supplied_path" .= suppliedPath])
  runtimes <- forM [("current", currentRuntimePath), ("candidate", candidateRuntimePath)] $ \(name, path) -> do
    value <- provenance path
    pure (name, value)
  frozenRows <- require (arrayField "fixtures" manifest)
  timestamp <- formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q+00:00" <$> getCurrentTime
  let reportBase rows totals =
        object
          [ "schema" .= (2 :: Int),
            "stage" .= stage options,
            "timestamp" .= timestamp,
            "platform" .= platform,
            "source_fingerprint" .= sourceFingerprint options,
            "source_fingerprint_kind" .= (if stage options == "final" then "verified final build inputs" else "working-tree inputs at capture; binary can precede in-progress source edits" :: Text),
            "build_receipt_sha256" .= fmap fst receiptHash,
            "verification_input_fingerprint" .= maybe Null (getValue "sha256" . getValue "after" . snd) receiptHash,
            "directory_contract" .= object ["lane" .= directoryContract options, "applies_to" .= (["current", "candidate"] :: [Text])],
            "candidate_entry" .= candidateEntry,
            "historic_denominator" .= (95 :: Int),
            "frozen_manifest_sha256" .= C.unpack (sha256 manifestBytes),
            "reference" .= profile,
            "tools" .= object [fromString name .= value | (name, value) <- tools],
            "current_runtime_probe" .= currentProbeRecord,
            "runtimes" .= object [fromString name .= value | (name, value) <- runtimes],
            "historical_python_adapter" .= object ["path" .= (providers </> "python3"), "sha256" .= adapterHash, "scope" .= ("Frozen background-jobs.bash python3 -c invocation only; stdin consumed, exit 7." :: Text)],
            "performance" .= object ["status" .= ("unverified" :: Text), "reason" .= ("This collector records correctness observations, not timing acceptance." :: Text)],
            "scope" .= (["raw stdout", "raw stderr", "exit status", "separate filesystem bytes and modes", "separate caller observations", "separate handshake process events"] :: [Text]),
            "fixtures" .= rows,
            "totals" .= totals
          ]
  rows <-
    foldM
      ( \prior (index, fixture) -> do
          let directory = output </> pad3 index
          createDirectory directory
          fixtureName <- T.unpack <$> require (textField "fixture" fixture)
          cohort <- require (field "cohort" fixture)
          hash <- require (field "input_sha256" fixture)
          (reference, baselineCommand, referenceFs) <- portableExecute bash False Nothing fixture (inputs </> fixtureName) inputs cwd env (timeoutSeconds options)
          baselineRecord <- observationRecord directory "bash" baselineCommand reference
          forM_ referenceFs (writeJson (directory </> "bash.filesystem.json"))
          observed <- forM ["original", "current", "candidate", "babelfish"] $ \name -> do
            let generated = directory </> name <> ".fish"
                runtime = if name == "current" then currentRuntimePath else candidateRuntimePath
            (translated, translationCommand) <- portableTranslate name (providers </> name) runtime fixture inputs generated env (timeoutSeconds options) (directoryContract options)
            translationRecord <- observationRecord directory (name <> ".translation") translationCommand translated
            generatedHash <- digestFile generated
            value <-
              if status translated /= "completed"
                then pure $ object ["translation" .= translationRecord, "generated_sha256" .= generatedHash, "status" .= ("translation-timeout" :: Text)]
                else
                  if exit translated /= 0
                    then pure $ object ["translation" .= translationRecord, "generated_sha256" .= generatedHash, "status" .= ("translation-rejected" :: Text)]
                    else do
                      let launcher = if name == "candidate" then Just candidateRuntimePath else Nothing
                      (result, command, fs) <- portableExecute fish True launcher fixture generated inputs cwd env (timeoutSeconds options)
                      executionRecord <- observationRecord directory name command result
                      forM_ fs (writeJson (directory </> name <> ".filesystem.json"))
                      let compared = compareEffects reference result referenceFs fs
                          unsupported = name == "current" && "requires 64-bit Linux" `B.isInfixOf` observedStderr currentProbe && "missing or incompatible native runtime" `B.isInfixOf` observedStderr result
                          withStatus = if unsupported then insertFields compared ["status" .= ("unsupported-native-platform" :: Text), "reason" .= ("The unchanged historical runtime requires 64-bit Linux; native Darwin execution is unavailable." :: Text)] else compared
                      pure $ insertFields withStatus ["translation" .= translationRecord, "generated_sha256" .= generatedHash, "execution" .= executionRecord]
            pure (name, value)
          let row =
                object
                  [ "fixture" .= fixtureName,
                    "cohort" .= cohort,
                    "input_sha256" .= hash,
                    "tools" .= object [fromString name .= value | (name, value) <- observed],
                    "bash" .= baselineRecord
                  ]
          let updated = prior <> [row]
          writeJson (output </> "partial.json") (reportBase updated (object []))
          pure updated
      )
      []
      (zip [0 :: Int ..] frozenRows)
  let totals = object [fromString cohort .= object [fromString tool .= countStatuses cohort tool rows | tool <- ["original", "current", "candidate", "babelfish"]] | cohort <- ["historic95", "filesystem", "caller", "process", "strengthened"]]
  writeJson (output </> "report.json") (reportBase rows totals)
  exists <- doesDirectoryExist cwd
  when exists (removeDirectoryRecursive cwd)
  pure totals

portableTranslate :: String -> FilePath -> FilePath -> Value -> FilePath -> FilePath -> [(String, String)] -> Int -> Text -> IO (Observation, [String])
portableTranslate tool binary runtime fixture inputs generated env timeout contract = do
  name <- T.unpack <$> require (textField "fixture" fixture)
  metadata <- require (field "metadata" fixture)
  let recursive = field "fixtureMetaRecursive" metadata == Right (Bool True)
      cohort = getValue "cohort" fixture
      source = inputs </> name
      command =
        if tool == "babelfish"
          then [binary]
          else
            [binary, source]
              <> (if recursive then ["--recursive", "--sources", "inline"] else [])
              <> (if tool /= "original" then ["--runtime", runtime] <> (if contract == "stable" then ["--directory-contract", "stable"] else []) <> (if cohort == String "caller" then ["--entry", "sourceable", "--caller-contract", inputs </> "effects/caller.json"] else []) else [])
  sourceBytes <- if tool == "babelfish" then B.readFile source else pure ""
  translated <- runObservation command sourceBytes inputs env timeout
  B.writeFile generated (observedStdout translated)
  pure (translated, command)

portableExecute :: FilePath -> Bool -> Maybe FilePath -> Value -> FilePath -> FilePath -> FilePath -> [(String, String)] -> Int -> IO (Observation, [String], Maybe Value)
portableExecute shell fish launcher fixture source inputs cwd env timeout = do
  exists <- doesDirectoryExist cwd
  when exists (removeDirectoryRecursive cwd)
  copyTree inputs cwd
  metadata <- require (field "metadata" fixture)
  argv <- require (arrayField "fixtureMetaArgs" metadata >>= traverse asString)
  cohort <- require (textField "cohort" fixture)
  name <- require (textField "fixture" fixture)
  input <- require (textField "stdin_base64" fixture >>= unbase64)
  workerExecutable <- getExecutablePath >>= canonicalizePath
  let processCase = cohort == "process"
      arguments = if processCase then [workerExecutable] else argv
  when (processCase && "background-handshake" `T.isInfixOf` name) $ do
    createNamedPipe (cwd </> "ready.fifo") (ownerReadMode .|. ownerWriteMode)
    createNamedPipe (cwd </> "go.fifo") (ownerReadMode .|. ownerWriteMode)
  let command
        | cohort == "caller" =
            if fish
              then [shell, "--no-config", "-c", "set --global value caller; set --global sentinel unchanged; source \"$argv[1]\"; set result $status; f; printf \"caller:%s:%s:%s\\n\" \"$value\" \"$sentinel\" \"$result\"", source]
              else [shell, "--noprofile", "--norc", "-c", "value=caller; sentinel=unchanged; source \"$1\"; result=$?; f; printf \"caller:%s:%s:%s\\n\" \"$value\" \"$sentinel\" \"$result\"", "caller-observer", source]
        | fish && isJust launcher = fromMaybe shell launcher : ["--abi", "2", "launch", source] <> arguments
        | fish = [shell, "--no-config", source] <> arguments
        | otherwise = [shell, "--noprofile", "--norc", source] <> arguments
  observed <- runObservation command input cwd env timeout
  fs <- if cohort `elem` ["filesystem", "process"] && status observed == "completed" then Just <$> snapshot cwd else pure Nothing
  pure (observed, command, fs)

countStatuses :: String -> String -> [Value] -> Value
countStatuses cohort tool rows = object [fromString name .= length (filter (== name) statuses) | name <- nub statuses]
  where
    statuses = [T.unpack value | row <- rows, field "cohort" row == Right (String (T.pack cohort)), Right value <- [field "tools" row >>= field (T.pack tool) >>= textField "status"]]

insertFields :: Value -> [Pair] -> Value
insertFields (Object original) pairs = case object pairs of Object extra -> Object (KM.union extra original); _ -> Object original
insertFields value _ = value

asString :: Value -> Either String String
asString (String value) = Right (T.unpack value)
asString _ = Left "expected argument string"

pad3 :: Int -> String
pad3 index = let digits = show index in replicate (max 0 (3 - length digits)) '0' <> digits

data PortableOptions = PortableOptions
  { frozenDirectory :: FilePath,
    outputDirectory :: FilePath,
    originalBinary :: FilePath,
    currentBinary :: FilePath,
    candidateBinary :: FilePath,
    babelfishBinary :: FilePath,
    currentRuntime :: FilePath,
    candidateRuntime :: FilePath,
    bashBinary :: FilePath,
    fishBinary :: FilePath,
    sourceFingerprint :: Text,
    buildReceipt :: Maybe FilePath,
    stage :: Text,
    timeoutSeconds :: Int,
    directoryContract :: Text
  }

getValue :: Text -> Value -> Value
getValue key value = fromRight Null (field key value)

require :: Either String a -> IO a
require = either fail pure

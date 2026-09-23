{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Compile before measuring: ghc -O2 -threaded -package aeson scripts/MeasureRustRuntime.hs -o measure-rust-runtime
-- prepare BASELINE_RUNTIME RUST_RUNTIME TRANSLATOR BASH FISH WORKSPACE REPORT
-- measure WORKSPACE
-- Preparation freezes providers and validates raw output/status without timing.
module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, catch, evaluate, throwIO, try)
import Control.Monad (forM, forM_, unless, when)
import Data.Aeson (FromJSON, ToJSON, Value, eitherDecodeStrict', encode, object, (.=))
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.ByteString.Lazy qualified as BL
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)
import System.Directory (canonicalizePath, copyFileWithMetadata, createDirectory, createDirectoryIfMissing, doesPathExist, getFileSize)
import System.Environment (getArgs)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (Handle, hClose)
import System.Posix.Signals (sigKILL, signalProcessGroup)
import System.Process (CreateProcess (..), StdStream (CreatePipe), createProcess, getPid, proc, waitForProcess)
import System.Timeout (timeout)

data Workload = Workload
  { fixtureName :: String,
    sourceFile :: Maybe FilePath,
    baselineCommand :: [String],
    candidateCommand :: [String],
    referenceCommand :: Maybe [String]
  }
  deriving (Generic, Show)

instance ToJSON Workload

instance FromJSON Workload

data Plan = Plan
  { workspace :: FilePath,
    reportPath :: FilePath,
    providers :: [(String, FilePath, String, Integer)],
    environment :: [(String, String)],
    workloads :: [Workload],
    frozenArtifacts :: [(FilePath, String)],
    preparationEvidence :: Value
  }
  deriving (Generic, Show)

instance ToJSON Plan

instance FromJSON Plan

data Observation = Observation
  { observedExit :: Int,
    observedStdout :: B.ByteString,
    observedStderr :: B.ByteString,
    elapsedNs :: Word64
  }
  deriving (Show)

same :: Observation -> Observation -> Bool
same a b = observedExit a == observedExit b && observedStdout a == observedStdout b && observedStderr a == observedStderr b

run :: Bool -> FilePath -> [(String, String)] -> [String] -> IO Observation
run _ _ _ [] = fail "empty command"
run timed working env (program : arguments) = do
  start <- if timed then getMonotonicTimeNSec else pure 0
  (Just input, Just output, Just errors, process) <-
    createProcess
      (proc program arguments)
        { cwd = Just working,
          env = Just env,
          std_in = CreatePipe,
          std_out = CreatePipe,
          std_err = CreatePipe,
          create_group = True
        }
  pid <- getPid process
  out <- capture output
  err <- capture errors
  hClose input
  done <- timeout 30000000 $ do
    status <- waitForProcess process
    stdoutBytes <- takeMVar out >>= either throwIO pure
    stderrBytes <- takeMVar err >>= either throwIO pure
    pure (status, stdoutBytes, stderrBytes)
  end <- if timed then getMonotonicTimeNSec else pure 0
  case done of
    Nothing -> do
      forM_ pid $ \group -> catch (signalProcessGroup sigKILL group) ignore
      _ <- waitForProcess process
      fail ("30-second command timeout: " <> show (program : arguments))
    Just (status, stdoutBytes, stderrBytes) ->
      pure
        Observation
          { observedExit = case status of ExitSuccess -> 0; ExitFailure code -> code,
            observedStdout = stdoutBytes,
            observedStderr = stderrBytes,
            elapsedNs = end - start
          }
  where
    ignore :: IOException -> IO ()
    ignore _ = pure ()
    capture :: Handle -> IO (MVar (Either IOException B.ByteString))
    capture handle = do
      result <- newEmptyMVar
      _ <- forkIO $ do
        value <- try $ do
          raw <- B.hGetContents handle
          _ <- evaluate (B.length raw)
          pure raw
        putMVar result value
      pure result

writeJson :: (ToJSON a) => FilePath -> a -> IO ()
writeJson path value = BL.writeFile path (encode value <> "\n")

baseEnvironment :: FilePath -> FilePath -> FilePath -> [(String, String)]
baseEnvironment work bash fish =
  [("PATH", takeDirectory bash <> ":" <> takeDirectory fish <> ":/usr/bin:/bin:/usr/sbin:/sbin"), ("HOME", work </> "home"), ("TMPDIR", "/private/tmp"), ("LANG", "C"), ("LC_ALL", "C"), ("TZ", "UTC")]

providerEnvironment :: Plan -> String -> [(String, String)]
providerEnvironment plan provider =
  let executable = providerPath plan provider
      old = fromMaybe "" (lookup "PATH" (environment plan))
   in ("PATH", takeDirectory executable <> ":" <> old) : filter ((/= "PATH") . fst) (environment plan)

providerPath :: Plan -> String -> FilePath
providerPath plan name = case [path | (key, path, _, _) <- providers plan, key == name] of
  [path] -> path
  _ -> error ("provider missing: " <> name)

hashFile :: FilePath -> IO String
hashFile path = do
  result <- run False "/private/tmp" [("PATH", "/usr/bin:/bin")] ["/usr/bin/shasum", "-a", "256", path]
  unless (observedExit result == 0) $ fail ("hash failed: " <> path)
  case C.words (observedStdout result) of
    digest : _ -> pure (C.unpack digest)
    _ -> fail "empty hash response"

record :: FilePath -> String -> Observation -> IO Value
record directory name observed = do
  let stdoutPath = directory </> name <> ".stdout"
      stderrPath = directory </> name <> ".stderr"
  B.writeFile stdoutPath (observedStdout observed)
  B.writeFile stderrPath (observedStderr observed)
  outHash <- hashFile stdoutPath
  errHash <- hashFile stderrPath
  pure $ object ["exit" .= observedExit observed, "stdout_bytes" .= B.length (observedStdout observed), "stdout_sha256" .= outHash, "stderr_bytes" .= B.length (observedStderr observed), "stderr_sha256" .= errHash]

fixtureSources :: [(String, B.ByteString)]
fixtureSources =
  [ ("arithmetic-loop-32", "i=0\ntotal=0\nwhile (( i < 32 )); do\n  (( total += i ))\n  (( i++ ))\ndone\nprintf '%s\\n' \"$total\"\n"),
    ("byte-output-8", "i=0\nwhile (( i < 8 )); do\n  printf '%s:%s:%s\\n' raw \"$i\" 'a\\tb'\n  echo -e '\\001\\377end'\n  (( i++ ))\ndone\n"),
    ("supervised-read-pipeline", "printf 'one\\ntwo\\nthree\\nfour\\nfive\\n' | while IFS= read -r line; do\n  printf '<%s>\\n' \"$line\"\ndone\n"),
    ("process-substitution", "while IFS= read -r line; do\n  printf '[%s]\\n' \"$line\"\ndone < <(printf 'red\\ngreen\\nblue\\n')\n")
  ]

prepare :: [String] -> IO ()
prepare [baseline, candidate, translator, bash, fish, rawWork, rawReport] = do
  occupied <- doesPathExist rawWork
  when occupied $ fail "preparation workspace must not already exist"
  createDirectoryIfMissing True (takeDirectory rawWork)
  createDirectory rawWork
  work <- canonicalizePath rawWork
  forM_ ["home", "tmp", "inputs", "generated", "observations", "providers", "providers/baseline", "providers/candidate"] $ createDirectoryIfMissing True . (work </>)
  createDirectoryIfMissing True (takeDirectory rawReport)
  reportDirectory <- canonicalizePath (takeDirectory rawReport)
  let report = reportDirectory </> takeFileName rawReport
      env = baseEnvironment work bash fish
  copied <- forM [("baseline", baseline, "baseline/monk-runtime"), ("candidate", candidate, "candidate/monk-runtime"), ("translator", translator, "monk")] $ \(name, source, relative) -> do
    original <- canonicalizePath source
    before <- hashFile original
    let destination = work </> "providers" </> relative
    copyFileWithMetadata original destination
    after <- hashFile original
    copiedHash <- hashFile destination
    unless (before == after && after == copiedHash) $ fail ("provider changed during freeze: " <> name)
    size <- getFileSize destination
    pure (name, destination, copiedHash, size)
  referenceProviders <- forM [("bash", bash), ("fish", fish)] $ \(name, source) -> do
    canonical <- canonicalizePath source
    digest <- hashFile canonical
    size <- getFileSize canonical
    pure (name, canonical, digest, size)
  let initial = Plan work report (copied <> referenceProviders) env [] [] (object [])
      old = providerPath initial "baseline"
      new = providerPath initial "candidate"
      tool = providerPath initial "translator"
  generated <- forM fixtureSources $ \(name, source) -> do
    let input = work </> "inputs" </> name <> ".bash"
        output provider = work </> "generated" </> name <> "." <> provider <> ".fish"
    B.writeFile input source
    forM_ [("baseline", old), ("candidate", new)] $ \(provider, runtime) -> do
      result <- run False work (providerEnvironment initial provider) [tool, input, "--strict", "--runtime", runtime, "-o", output provider]
      _ <- record (work </> "observations") (name <> "." <> provider <> ".translation") result
      unless (observedExit result == 0) $ fail ("strict translation failed: " <> name <> ": " <> C.unpack (observedStderr result))
    pure Workload {fixtureName = name, sourceFile = Just input, baselineCommand = [old, "--abi", "2", "launch", output "baseline"], candidateCommand = [new, "--abi", "2", "launch", output "candidate"], referenceCommand = Just [bash, input]}
  let ready = initial {workloads = Workload "startup-describe" Nothing [old, "--describe"] [new, "--describe"] Nothing : generated}
  parity <- forM (workloads ready) $ \workload -> do
    baselineResult <- run False work (providerEnvironment ready "baseline") (baselineCommand workload)
    candidateResult <- run False work (providerEnvironment ready "candidate") (candidateCommand workload)
    unless (same baselineResult candidateResult) $ fail ("runtime parity failed: " <> fixtureName workload <> ": " <> show (baselineResult, candidateResult))
    reference <- case referenceCommand workload of
      Nothing -> pure Nothing
      Just command -> do
        result <- run False work env command
        unless (same baselineResult result) $ fail ("Bash parity failed: " <> fixtureName workload)
        Just <$> record (work </> "observations") (fixtureName workload <> ".bash") result
    baselineRecord <- record (work </> "observations") (fixtureName workload <> ".baseline") baselineResult
    candidateRecord <- record (work </> "observations") (fixtureName workload <> ".candidate") candidateResult
    pure $ object ["fixture" .= fixtureName workload, "baseline" .= baselineRecord, "candidate" .= candidateRecord, "bash" .= reference, "streams_and_status_equal" .= True]
  host <- forM [("uname", ["/usr/bin/uname", "-srm"]), ("macos", ["/usr/bin/sw_vers"]), ("cpu", ["/usr/sbin/sysctl", "-n", "machdep.cpu.brand_string"]), ("logical_cpus", ["/usr/sbin/sysctl", "-n", "hw.logicalcpu"]), ("memory_bytes", ["/usr/sbin/sysctl", "-n", "hw.memsize"]), ("bash_version", [bash, "--version"]), ("fish_version", [fish, "--version"])] $ \(name :: String, command) -> do
    result <- run False work env command
    pure (name, C.unpack (observedStdout result))
  let artifactPaths = [path | workload <- generated, Just path <- [sourceFile workload]] <> [work </> "generated" </> fixtureName workload <> "." <> provider <> ".fish" | workload <- generated, provider <- ["baseline", "candidate"]]
  artifacts <- forM artifactPaths $ \path -> do digest <- hashFile path; pure (path, digest)
  let final = ready {frozenArtifacts = artifacts, preparationEvidence = object ["parity" .= parity, "host" .= host, "harness_compiler" .= ("GHC 9.14.1 -O2 -threaded" :: String), "candidate_toolchain" .= ("Rust nightly 2026-09-23, release profile, -Zon-broken-pipe=inherit" :: String)]}
  writeJson (work </> "plan.json") final
  putStrLn ("Prepared and parity-checked; no timings collected: " <> work)
prepare _ = fail "prepare BASELINE_RUNTIME RUST_RUNTIME TRANSLATOR BASH FISH WORKSPACE REPORT"

median :: [Double] -> Double
median raw = let values = sort raw; count = length values; half = div count 2 in if odd count then values !! half else (values !! (half - 1) + values !! half) / 2

distribution :: [Double] -> Value
distribution raw = case sort raw of
  [] -> error "distribution requires nonempty samples"
  values@(first : rest) ->
    let percentile :: Double -> Double
        percentile p = values !! floor (p * fromIntegral (length values - 1))
     in object ["min" .= first, "p10" .= percentile 0.1, "median" .= median values, "p90" .= percentile 0.9, "max" .= foldl max first rest]

measure :: FilePath -> IO ()
measure work = do
  plan <- B.readFile (work </> "plan.json") >>= either fail pure . eitherDecodeStrict'
  forM_ (providers plan) $ \(_, path, expected, _) -> hashFile path >>= \actual -> unless (actual == expected) (fail ("frozen provider changed: " <> path))
  forM_ (frozenArtifacts plan) $ \(path, expected) -> hashFile path >>= \actual -> unless (actual == expected) (fail ("frozen input changed: " <> path))
  results <- forM (workloads plan) $ \workload -> do
    reference <- run False (workspace plan) (providerEnvironment plan "baseline") (baselineCommand workload)
    samples <- forM [0 :: Int .. 22] $ \iteration -> do
      let order = if even iteration then ["baseline", "candidate"] else ["candidate", "baseline"]
      forM order $ \provider -> do
        let command = if provider == "baseline" then baselineCommand workload else candidateCommand workload
        observed <- run (iteration >= 3) (workspace plan) (providerEnvironment plan provider) command
        unless (same reference observed) $ fail ("sample parity changed: " <> fixtureName workload <> " " <> provider)
        pure (provider, elapsedNs observed)
    let measured = drop 3 samples
        baselineTimes = [fromMaybe 0 (lookup "baseline" pair) | pair <- measured]
        candidateTimes = [fromMaybe 0 (lookup "candidate" pair) | pair <- measured]
        baselineValues = map fromIntegral baselineTimes
        candidateValues = map fromIntegral candidateTimes
        ratios = zipWith (/) candidateValues baselineValues
    putStrLn (fixtureName workload <> ": candidate/baseline " <> show (median candidateValues / median baselineValues))
    pure $ object ["fixture" .= fixtureName workload, "baseline_command" .= baselineCommand workload, "candidate_command" .= candidateCommand workload, "samples_ns" .= object ["baseline" .= baselineTimes, "candidate" .= candidateTimes], "baseline_ns" .= distribution baselineValues, "candidate_ns" .= distribution candidateValues, "paired_candidate_over_baseline" .= distribution ratios, "ratio_of_medians" .= (median candidateValues / median baselineValues), "every_sample_streams_and_status_equal" .= True]
  timestamp <- getCurrentTime
  let size name = case [bytes | (key, _, _, bytes) <- providers plan, key == name] of [bytes] -> bytes; _ -> 0
      output = object ["schema" .= (1 :: Int), "measured_at" .= show timestamp, "providers" .= providers plan, "preparation" .= preparationEvidence plan, "frozen_artifacts" .= frozenArtifacts plan, "protocol" .= object ["samples_per_provider" .= (20 :: Int), "warmups_per_provider" .= (3 :: Int), "alternating_order" .= True, "serial" .= True, "timer" .= ("monotonic nanoseconds around process launch and raw stream capture; no polling sleep" :: String), "builds_and_tests_suspended" .= True], "binary_size_bytes" .= object ["baseline" .= size "baseline", "candidate" .= size "candidate", "candidate_over_baseline" .= (fromIntegral (size "candidate") / fromIntegral (size "baseline") :: Double)], "workloads" .= results, "limitations" .= (["Single host; warmed wall-time observations include process launch and stream-capture overhead.", "Small representative workloads, not the historical aggregate performance gate.", "No Linux execution, cold-cache measurements, allocation profile, or process tracing."] :: [String])]
  writeJson (reportPath plan) output
  putStrLn ("Wrote " <> reportPath plan)

main :: IO ()
main =
  getArgs >>= \case
    "prepare" : rest -> prepare rest
    ["measure", work] -> measure work
    _ -> fail "expected prepare ... or measure WORKSPACE; compile before the quiet timing window"

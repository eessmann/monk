-- | Command dispatcher for host-side evidence collectors.
module Monk.Tooling.Evidence.Dispatch (runEvidence) where

import Data.Aeson (Value, encode)
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Monk.Tooling.Evidence.Babelfish (runBabelfish)
import Monk.Tooling.Evidence.Comparison (runComparison)
import Monk.Tooling.Evidence.Native (freezeNative, measureNative)
import Monk.Tooling.Evidence.Performance (freezePerformance, measurePerformance)
import Monk.Tooling.Evidence.Portable (PortableOptions (..), freezePortable, measurePortable, runHistoricalPython, runWorker)
import Monk.Tooling.Evidence.Profile (runProfile)
import Monk.Tooling.Evidence.Trace (runTrace)
import Monk.Tooling.Evidence.Verification (runVerification)

runEvidence :: [String] -> IO ()
runEvidence arguments = case arguments of
  "profile" : rest -> do
    let bash = optionDefault "--bash" "bash" rest
        fish = optionDefault "--fish" "fish" rest
    runProfile bash fish >>= outputValue
  "compare-bakeoff-bash" : runDirectory : rest -> do
    let bash = optionDefault "--bash" "bash" rest
        fish = optionDefault "--fish" "fish" rest
        timeout = ceiling (readOptionDefault "--timeout" 30 rest :: Double)
    runComparison runDirectory bash fish timeout >>= outputValue
  "native" : action : rest -> do
    output <- required "--out" rest
    baseline <- required "--baseline" rest
    bash <- required "--bash" rest
    fish <- required "--fish" rest
    case action of
      "freeze" -> freezeNative output baseline bash fish >>= outputValue
      "coverage" -> runNativeMeasurement True output baseline bash fish rest
      "measure" -> runNativeMeasurement False output baseline bash fish rest
      _ -> fail "native action must be freeze, coverage or measure"
  "babelfish" : rest -> do
    output <- required "--out" rest
    binary <- required "--babelfish" rest
    bash <- required "--bash" rest
    fish <- required "--fish" rest
    runBabelfish output binary bash fish ("--time-common16" `elem` rest) (optionDefault "--run-name" "babelfish-fresh" rest) >>= outputValue
  "trace" : rest -> do
    output <- required "--out" rest
    fish <- required "--fish" rest
    variant <- required "--variant" rest
    runTrace output fish variant (option "--candidate-dir" rest) >>= outputValue
  "verification" : rest -> do
    output <- required "--output" rest
    let binaries = options "--binary" rest
        command = case dropWhile (/= "--") rest of _ : xs -> xs; [] -> []
    result <- runVerification output binaries command
    exitWith result
  "portable-comparison" : "freeze" : rest -> do
    output <- required "--output" rest
    let repo = optionDefault "--repo" "." rest
        contract = optionDefault "--directory-contract" "default" rest
    validateDirectoryContract contract
    freezePortable repo output contract >>= outputValue
  "portable-comparison" : "measure" : rest -> do
    frozen <- required "--frozen" rest
    output <- required "--output" rest
    original <- required "--original" rest
    current <- required "--current" rest
    candidate <- required "--candidate" rest
    babelfish <- required "--babelfish" rest
    oldRuntime <- required "--current-runtime" rest
    runtime <- required "--runtime" rest
    bash <- required "--bash" rest
    fish <- required "--fish" rest
    fingerprint <- required "--source-fingerprint" rest
    stageName <- required "--stage" rest
    let contract = optionDefault "--directory-contract" "default" rest
        config = PortableOptions frozen output original current candidate babelfish oldRuntime runtime bash fish (T.pack fingerprint) (option "--build-receipt" rest) (T.pack stageName) (ceiling (readOptionDefault "--timeout" 10 rest :: Double)) (T.pack contract)
    validateDirectoryContract contract
    measurePortable config >>= outputValue
  "portable-performance" : "freeze" : rest -> do
    frozen <- required "--frozen" rest
    output <- required "--output" rest
    freezePerformance frozen output
  "portable-performance" : "measure" : rest -> do
    frozen <- required "--frozen" rest
    output <- required "--output" rest
    baseline <- required "--baseline" rest
    candidate <- required "--candidate" rest
    oldRuntime <- required "--baseline-runtime" rest
    runtime <- required "--runtime" rest
    bash <- required "--bash" rest
    fish <- required "--fish" rest
    receipt <- required "--build-receipt" rest
    measurePerformance frozen output baseline candidate oldRuntime runtime bash fish receipt >>= outputValue
  ["worker", action] -> runWorker action
  "historical-python" : rest -> runHistoricalPython rest
  _ -> fail "unknown evidence command"
  where
    runNativeMeasurement coverage output baseline bash fish rest = do
      candidate <- required "--candidate" rest
      runtime <- required "--runtime" rest
      fingerprint <- required "--source-fingerprint" rest
      measureNative coverage output baseline candidate runtime bash fish (T.pack fingerprint) >>= outputValue

outputValue :: Value -> IO ()
outputValue value = BL.putStr (encode value <> "\n")

option :: String -> [String] -> Maybe String
option name arguments = case dropWhile (/= name) arguments of
  _ : value : _ | value /= "--" -> Just value
  _ -> Nothing

options :: String -> [String] -> [String]
options name = go
  where
    go (key : value : rest) | key == name = value : go rest
    go (_ : rest) = go rest
    go [] = []

optionDefault :: String -> String -> [String] -> String
optionDefault name fallback = fromMaybe fallback . option name

required :: String -> [String] -> IO String
required name arguments = maybe (fail ("missing required option " <> name)) pure (option name arguments)

readOptionDefault :: (Read a) => String -> a -> [String] -> a
readOptionDefault name fallback arguments = fromMaybe fallback (option name arguments >>= readMaybe)

validateDirectoryContract :: String -> IO ()
validateDirectoryContract value =
  unless (value `elem` ["default", "stable"]) $ fail "--directory-contract must be default or stable"

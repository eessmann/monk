-- | Reviewed fixture admission and rendered Fish syntax evidence.
module Monk.Tooling.Parity (generateParityManifest) where

import Control.Exception qualified as E
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.List (nub)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Development.Shake
  ( ShakeOptions (..),
    Verbosity (Silent),
    getDirectoryFiles,
    need,
    shake,
    shakeOptions,
    want,
    (%>),
  )
import Monk.Runtime.Digest (sha256)
import Monk.Tooling.Process (ProcessResult (..), ProcessSpec (..), runProcess)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesFileExist, findExecutable, makeAbsolute)
import System.Exit (ExitCode (..))
import System.FilePath (replaceExtension, takeDirectory, (</>))
import System.IO (hPutStrLn)
import System.IO.Temp (withTempDirectory)

data Policy = Policy
  { fixture :: FilePath,
    expected :: Text,
    diagnosticPrefix :: Text,
    rationale :: Text
  }

-- | Build the same ten-column TSV produced by the original admission check.
-- Shake tracks the translator, policy, fixture inventory, recursive markers,
-- and every Bash fixture (including sourced children).
generateParityManifest :: FilePath -> FilePath -> IO Bool
generateParityManifest monkBinary outputPath = do
  monk <- makeAbsolute monkBinary
  monkHash <- C.unpack . sha256 <$> B.readFile monk
  fish <- findExecutable "fish" >>= maybe (fail "fish executable not found") canonicalizePath
  fishHash <- C.unpack . sha256 <$> B.readFile fish
  output <- makeAbsolute outputPath
  createDirectoryIfMissing True (takeDirectory output)
  let options =
        shakeOptions
          { shakeFiles = takeDirectory output </> ".parity-shake",
            shakeVerbosity = Silent,
            shakeVersion = show (monk, monkHash, fish, fishHash)
          }
  result <- E.try @E.SomeException $ shake options $ do
    want [output]
    output %> \target -> do
      fixtures <- getDirectoryFiles "" ["test/fixtures//*.bash"]
      recursiveMarkers <- getDirectoryFiles "" ["test/fixtures//*.recursive"]
      need (fish : monk : "test/fixtures/admission.tsv" : fixtures <> recursiveMarkers)
      policies <- liftIO readPolicies
      let declared = sort (map fixture policies)
      unless (declared == sort fixtures) $
        fail "fixture policy inventory is missing, duplicated, or obsolete"
      liftIO $ withTempDirectory (takeDirectory target) "parity-" $ \scratch -> do
        outcomes <- forM (zip [0 :: Int ..] policies) (uncurry (checkFixture monk fish scratch))
        B.writeFile target (TE.encodeUtf8 (renderManifest (map fst outcomes)))
        forM_ outcomes $ \(_, failure) ->
          forM_ failure $ \details ->
            hPutStrLn stderr ("unaccounted fixture result: " <> details)
        let good = length (filter (isNothing . snd) outcomes)
        putStrLn (show good <> "/" <> show (length policies) <> " reviewed fixture admissions and syntax verified")
  -- A failed admission still produces a reviewable manifest. The rule must
  -- succeed so Shake retains its target; the CLI exits nonzero from the saved
  -- verification column, including when Shake reuses an unchanged target.
  case result of
    Left failure -> do
      hPutStrLn stderr (E.displayException failure)
      pure False
    Right () -> do
      raw <- B.readFile output
      policyCount <- length <$> readPolicies
      pure (manifestVerified policyCount (TE.decodeUtf8With lenientDecode raw))

readPolicies :: IO [Policy]
readPolicies = do
  raw <- TE.decodeUtf8 <$> B.readFile "test/fixtures/admission.tsv"
  case T.lines raw of
    header : rows
      | header == "fixture\texpected\tdiagnostic_prefix\trationale" ->
          traverse parsePolicy rows
    _ -> fail "invalid fixture admission header"
  where
    parsePolicy line = case T.splitOn "\t" line of
      [path, verdict, prefix, why] ->
        pure (Policy (T.unpack path) verdict prefix why)
      _ -> fail ("invalid fixture policy row: " <> T.unpack line)

checkFixture :: FilePath -> FilePath -> FilePath -> Int -> Policy -> IO (Text, Maybe String)
checkFixture monk fishBinary scratch index policy = do
  let target = scratch </> ("fixture-" <> show index <> ".fish")
      path = fixture policy
  recursive <- doesFileExist (replaceExtension path "recursive")
  let arguments =
        [path, "--strict", "--output", target]
          <> if recursive then ["--recursive", "--sources", "inline"] else []
  translated <- runBounded monk arguments
  exists <- doesFileExist target
  let diagnostics = TE.decodeUtf8With lenientDecode (processStderr translated)
      codes = sort . nub $ concatMap (extractCodes . T.strip) (T.lines diagnostics)
      requirements = sort . nub $ mapMaybe extractRequirement (T.lines diagnostics)
      accepted = processExit translated == ExitSuccess && exists
  fish <- if exists then B.readFile target else pure B.empty
  syntax <-
    if accepted
      then (== ExitSuccess) . processExit <$> runBounded fishBinary ["--no-config", "--no-execute", target]
      else pure False
  verified <- case expected policy of
    "exact" -> pure (accepted && syntax)
    "reject" ->
      pure
        ( processExit translated /= ExitSuccess
            && not exists
            && any (T.isPrefixOf (diagnosticPrefix policy)) codes
        )
    other -> fail ("unknown fixture policy: " <> T.unpack other <> " for " <> path)
  let columns =
        [ T.pack path,
          expected policy,
          if accepted then "accepted" else "rejected",
          boolText syntax,
          if accepted then T.pack (C.unpack (sha256 fish)) else "",
          T.pack (show (B.length fish)),
          T.intercalate "," codes,
          T.intercalate ";" requirements,
          boolText verified,
          rationale policy
        ]
      failure = if verified then Nothing else Just (path <> "\n" <> T.unpack diagnostics)
  pure (tsvRow columns, failure)

runBounded :: FilePath -> [String] -> IO ProcessResult
runBounded command arguments = do
  result <-
    runProcess
      ProcessSpec
        { executable = command,
          arguments = arguments,
          workingDirectory = Nothing,
          environment = Nothing,
          stdinBytes = B.empty,
          timeoutMicros = 30000000
        }
  when (processTimedOut result) $
    fail (command <> " timed out after 30 seconds")
  pure result

extractCodes :: Text -> [Text]
extractCodes line = concatMap (`bracketed` line) ["error[", "warning[", "note["]
  where
    bracketed marker remaining =
      let (_, afterMarker) = T.breakOn marker remaining
       in if T.null afterMarker
            then []
            else
              let (code, afterCode) = T.breakOn "]" (T.drop (T.length marker) afterMarker)
               in if T.null afterCode
                    then []
                    else code : bracketed marker (T.drop 1 afterCode)

extractRequirement :: Text -> Maybe Text
extractRequirement line =
  let marker = "runtime requirement: "
      (_, rest) = T.breakOn marker line
      requirement = T.drop (T.length marker) rest
   in if T.null rest || T.null requirement then Nothing else Just requirement

renderManifest :: [Text] -> Text
renderManifest rows =
  T.unlines (manifestHeader : rows)

manifestHeader :: Text
manifestHeader = "fixture\texpected\tactual\tfish_syntax\trendered_sha256\tfish_bytes\tdiagnostic_codes\trequirements\tverified\trationale"

tsvRow :: [Text] -> Text
tsvRow = T.intercalate "\t" . map escape
  where
    escape value
      | T.any (`elem` ['\t', '\n', '\r', '"']) value = "\"" <> T.replace "\"" "\"\"" value <> "\""
      | otherwise = value

boolText :: Bool -> Text
boolText True = "true"
boolText False = "false"

manifestVerified :: Int -> Text -> Bool
manifestVerified policyCount manifest = case T.lines manifest of
  header : rows ->
    header == manifestHeader
      && length rows == policyCount
      && all verifiedRow rows
  _ -> False
  where
    verifiedRow row = case T.splitOn "\t" row of
      [_, _, _, _, _, _, _, _, "true", _] -> True
      _ -> False

-- | Compile the public API consumer and its expected abstraction failures.
module Monk.Tooling.Boundaries (checkPublicBoundaries) where

import Data.Aeson (Value, eitherDecodeStrict', encode, object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Monk.Tooling.Process (ProcessResult (..), ProcessSpec (..), runProcess)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, makeAbsolute)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)

data CaseResult = CaseResult
  { caseName :: Text,
    caseExit :: Int,
    casePassed :: Bool,
    caseDiagnostics :: Text
  }

-- | Return whether the positive consumer compiled and every negative case
-- failed with its intended diagnostic. The optional report has the same JSON
-- fields as the historical check and retains complete compiler output.
checkPublicBoundaries :: FilePath -> FilePath -> Maybe FilePath -> IO Bool
checkPublicBoundaries compiler planPath reportPath = do
  plan <- B.readFile planPath
  packageId <- either fail pure (mainLibraryId plan)
  compilerId <- either fail pure (planCompilerId plan)
  absolutePlan <- makeAbsolute planPath
  let packageDb = takeDirectory (takeDirectory absolutePlan) </> "packagedb" </> T.unpack compilerId
  present <- doesDirectoryExist packageDb
  unless present $ fail ("Cabal package database is missing: " <> packageDb)
  source <- makeAbsolute "test/compile-fail"
  results <- withSystemTempDirectory "monk-boundaries-" $ \scratch ->
    compileCases compiler packageId packageDb source scratch
  versionRun <- runBounded compiler ["--numeric-version"]
  unless (processExit versionRun == ExitSuccess) $
    fail (compiler <> " --numeric-version failed")
  let version = T.strip (TE.decodeUtf8With lenientDecode (processStdout versionRun))
      report =
        object
          [ "compiler" .= version,
            "package_id" .= packageId,
            "results" .= map caseJson results
          ]
  forM_ reportPath $ \path -> do
    createDirectoryIfMissing True (takeDirectory path)
    BL.writeFile path (encode report <> "\n")
  let passed = length results == 12 && all casePassed results
  unless passed $
    forM_ results $ \result ->
      unless (casePassed result) (putStrLn (T.unpack (caseDiagnostics result)))
  pure passed

mainLibraryId :: B.ByteString -> Either String Text
mainLibraryId bytes = do
  value <- eitherDecodeStrict' bytes
  units <- parseEither parseUnits value
  case [unitId | (name, component, unitId) <- units, name == "monk", component == Just "lib"] of
    [unitId] -> Right unitId
    _ -> Left "Expected exactly one built main Monk library in Cabal plan"
  where
    parseUnits :: Value -> Parser [(Text, Maybe Text, Text)]
    parseUnits = withObject "Cabal plan" $ \plan -> do
      entries <- plan .: "install-plan"
      traverse parseUnit entries
    parseUnit = withObject "Cabal unit" $ \unit ->
      (,,) <$> unit .: "pkg-name" <*> unit .:? "component-name" <*> unit .: "id"

planCompilerId :: B.ByteString -> Either String Text
planCompilerId bytes = do
  value <- eitherDecodeStrict' bytes
  compilerId <- parseEither (withObject "Cabal plan" (.: "compiler-id")) value
  if "ghc-" `T.isPrefixOf` compilerId && not (T.any (== '/') compilerId)
    then Right compilerId
    else Left "Cabal plan has an invalid GHC compiler ID"

compileCases :: FilePath -> Text -> FilePath -> FilePath -> FilePath -> IO [CaseResult]
compileCases compiler packageId packageDb source scratch = go [] ("Positive" : negativeCases)
  where
    command name =
      [ "-v0",
        "-fno-code",
        "-fforce-recomp",
        "-package-db",
        packageDb,
        "-package-id",
        T.unpack packageId,
        "-outputdir",
        scratch,
        source </> T.unpack name <> ".hs"
      ]
    go completed [] = pure (reverse completed)
    go completed (name : remaining) = do
      run <- runBounded compiler (command name)
      let diagnostics =
            TE.decodeUtf8With lenientDecode (processStdout run <> processStderr run)
          passed
            | name == "Positive" = processExit run == ExitSuccess && not (processTimedOut run)
            | otherwise =
                processExit run /= ExitSuccess
                  && not (processTimedOut run)
                  && matchesCase name diagnostics
          result = CaseResult name (exitNumber (processExit run)) passed diagnostics
      putStrLn (T.unpack ((if passed then "PASS " else "FAIL ") <> name))
      if name == "Positive" && not passed
        then pure [result]
        else go (result : completed) remaining

negativeCases :: [Text]
negativeCases =
  [ "ForgeResult",
    "UpdateResult",
    "ForgeGraph",
    "UpdateGraph",
    "ForgeBundle",
    "UpdateBundle",
    "UpdateRuntimeImage",
    "ForgeRuntimeImage",
    "ForgeRuntimeArtifact",
    "PrivatePlan",
    "PrivatePublisher"
  ]

matchesCase :: Text -> Text -> Bool
matchesCase name raw = case name of
  "ForgeResult" -> forge "MkTranslationResult"
  "UpdateResult" -> update "translationScript"
  "ForgeGraph" -> forge "MkSourceGraph"
  "UpdateGraph" -> update "sourceRoot"
  "ForgeBundle" -> forge "MkOutputBundle"
  "UpdateBundle" -> update "bundleRuntimeArtifacts"
  "UpdateRuntimeImage" -> update "nativeImageBytes"
  "ForgeRuntimeImage" -> forge "MkNativeRuntimeImage"
  "ForgeRuntimeArtifact" -> forge "MkNativeRuntimeArtifact"
  "PrivatePlan" -> hidden "Language.Bash.Plan"
  "PrivatePublisher" -> hidden "Monk.Output.Publication"
  _ -> False
  where
    diagnostics = T.toCaseFold raw
    forge constructor =
      let symbol = T.toCaseFold constructor
       in ordered "not in scope:" symbol diagnostics
            || ordered symbol "not in scope" diagnostics
    update field =
      let symbol = T.toCaseFold field
       in orderedWithin symbol "is not a record selector" 30 diagnostics
            || orderedWithin "not in scope: record field" symbol 10 diagnostics
    hidden moduleName =
      orderedWithin "could not load module" (T.toCaseFold moduleName) 10 diagnostics
        && orderedWithin (T.toCaseFold moduleName) "hidden module" 50 diagnostics

ordered :: Text -> Text -> Text -> Bool
ordered preceding following haystack =
  let (_, suffix) = T.breakOn preceding haystack
   in not (T.null suffix) && following `T.isInfixOf` T.drop (T.length preceding) suffix

orderedWithin :: Text -> Text -> Int -> Text -> Bool
orderedWithin preceding following limit = seek
  where
    seek haystack =
      let (_, suffix) = T.breakOn preceding haystack
       in not (T.null suffix)
            && let remainder = T.drop (T.length preceding) suffix
                   (between, match) = T.breakOn following remainder
                in (not (T.null match) && T.length between <= limit)
                     || seek (T.drop 1 suffix)

runBounded :: FilePath -> [String] -> IO ProcessResult
runBounded compiler arguments =
  runProcess
    ProcessSpec
      { executable = compiler,
        arguments = arguments,
        workingDirectory = Nothing,
        environment = Nothing,
        stdinBytes = B.empty,
        timeoutMicros = 60000000
      }

exitNumber :: ExitCode -> Int
exitNumber ExitSuccess = 0
exitNumber (ExitFailure code) = code

caseJson :: CaseResult -> Value
caseJson result =
  object
    [ "case" .= caseName result,
      "exit" .= caseExit result,
      "passed" .= casePassed result,
      "diagnostics" .= caseDiagnostics result
    ]

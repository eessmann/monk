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
  packageId <- either fail pure (libraryId "lib" plan)
  internalPackageId <- either fail pure (libraryId "lib:monk-compiler" plan)
  foundationPackageId <- either fail pure (libraryId "lib:monk-foundation" plan)
  publicationPackageId <- either fail pure (libraryId "lib:monk-publication" plan)
  compilerId <- either fail pure (planCompilerId plan)
  absolutePlan <- makeAbsolute planPath
  let packageDb = takeDirectory (takeDirectory absolutePlan) </> "packagedb" </> T.unpack compilerId
  present <- doesDirectoryExist packageDb
  unless present $ fail ("Cabal package database is missing: " <> packageDb)
  source <- makeAbsolute "test/compile-fail"
  results <- withSystemTempDirectory "monk-boundaries-" $ \scratch -> do
    publicResults <- compileCases compiler [packageId] packageDb source scratch ("Positive" : negativeCases)
    -- Explicit module exposure keeps GHC from hiding the compiler sublibrary
    -- when another component of the same package is exposed afterwards.
    internalResults <- compileCases compiler [internalPackageId, foundationPackageId <> " (Monk.Runtime.Abi2, Monk.Translation.Types)"] packageDb source scratch ("InternalPositive" : internalNegativeCases)
    publicationResults <- compileCases compiler [publicationPackageId] packageDb source scratch ["PublicationPositive", "ForgePublicationPlan"]
    pure (publicResults <> internalResults <> publicationResults)
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
  let passed = length results == 4 + length negativeCases + length internalNegativeCases && all casePassed results
  unless passed $
    forM_ results $ \result ->
      unless (casePassed result) (putStrLn (T.unpack (caseDiagnostics result)))
  pure passed

libraryId :: Text -> B.ByteString -> Either String Text
libraryId selected bytes = do
  value <- eitherDecodeStrict' bytes
  units <- parseEither parseUnits value
  case [unitId | (name, component, unitId) <- units, name == "monk", component == Just selected] of
    [unitId] -> Right unitId
    _ -> Left ("Expected exactly one built Monk component " <> T.unpack selected <> " in Cabal plan")
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

compileCases :: FilePath -> [Text] -> FilePath -> FilePath -> FilePath -> [Text] -> IO [CaseResult]
compileCases compiler packageIds packageDb source scratch = go []
  where
    command name =
      [ "-v0",
        "-fno-code",
        "-fforce-recomp",
        "-package-db",
        packageDb
      ]
        <> concatMap (\packageId -> ["-package-id", T.unpack packageId]) packageIds
        <> [ "-outputdir",
             scratch,
             source </> T.unpack name <> ".hs"
           ]
    go completed [] = pure (reverse completed)
    go completed (name : remaining) = do
      run <- runBounded compiler (command name)
      let diagnostics =
            TE.decodeUtf8With lenientDecode (processStdout run <> processStderr run)
          passed
            | isPositive name = processExit run == ExitSuccess && not (processTimedOut run)
            | otherwise =
                processExit run /= ExitSuccess
                  && not (processTimedOut run)
                  && matchesCase name diagnostics
          result = CaseResult name (exitNumber (processExit run)) passed diagnostics
      putStrLn (T.unpack ((if passed then "PASS " else "FAIL ") <> name))
      if isPositive name && not passed
        then pure [result]
        else go (result : completed) remaining

negativeCases :: [Text]
negativeCases =
  [ "NestedBackground",
    "ForgeBackgroundGrammar",
    "ForgeStageGrammar",
    "ForgeRedirectForm",
    "ControlPipelineStage",
    "ExecPipelineStage",
    "SubstitutionExecutable",
    "CoerceCommandName",
    "RawCommandName",
    "ForgeResult",
    "UpdateResult",
    "ForgeGraph",
    "UpdateGraph",
    "ForgeBundle",
    "UpdateBundle",
    "UpdateRuntimeImage",
    "ForgeRuntimeImage",
    "ForgeRuntimeArtifact",
    "PrivatePlan",
    "PrivatePublisher",
    "BlockSuffixArgument",
    "RedirectAppendDescriptor",
    "RedirectCombinedInput",
    "BackgroundPipelineStage",
    "SequencePipelineStage"
  ]

internalNegativeCases :: [Text]
internalNegativeCases = ["PipelineBackgroundField", "RawVariableIdentifier", "RawSetIdentifier", "RawFunctionParameter", "CoerceIdentifier", "CrossOwnerEntryBody", "UnownedEntryBody", "WrongFunctionBody", "LoopAsFunctionRoot", "CoerceBodyKind", "IncompleteIntegerRequest", "UnownedArtifactPayload", "IncompletePrimitive", "PrimitiveScalarArgument", "CoercePrimitive", "CrossRegionScalar", "CoerceRegionScalar", "CrossLoopBodyTarget", "CrossScopeLoopTarget", "CrossScopeReturnTarget", "CrossScopeArgumentsTarget", "CoerceControlScope", "IncompleteSessionRequest", "SessionReplyCardinality", "CoerceSessionRequest", "CoerceEmission", "CoerceWorld", "CoerceDenseProof", "CoerceDenseUpdate", "CrossWorldDenseProof", "StaleDenseUpdate", "ReproveOldArrayShape", "DraftArtifactInspection", "CrossOwnerView", "UnquotedExecutable", "CoerceOwnedPlan", "CoerceArtifactPhase", "CoerceProvider", "UnownedLoopExit", "UnprovedNativeStatement", "SessionNativeCapability"]

isPositive :: Text -> Bool
isPositive name = name `elem` ["Positive", "InternalPositive", "PublicationPositive"]

matchesCase :: Text -> Text -> Bool
matchesCase name raw = case name of
  "NestedBackground" -> "an asynchronous job cannot be backgrounded again" `T.isInfixOf` diagnostics
  "PipelineBackgroundField" -> "applied to five visible arguments" `T.isInfixOf` diagnostics && "has only four" `T.isInfixOf` diagnostics
  "ForgeBackgroundGrammar" -> "illegal instance for type family" `T.isInfixOf` diagnostics && "backgroundgrammar" `T.isInfixOf` diagnostics
  "ForgePublicationPlan" -> forge "MkManagedPublicationPlan"
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
  "BlockSuffixArgument" -> mismatch "redirect"
  "RedirectAppendDescriptor" -> "illegal redirect form" `T.isInfixOf` diagnostics
  "RedirectCombinedInput" -> "illegal redirect form" `T.isInfixOf` diagnostics
  "BackgroundPipelineStage" -> "illegal pipeline stage grammar" `T.isInfixOf` diagnostics
  "SequencePipelineStage" -> "illegal pipeline stage grammar" `T.isInfixOf` diagnostics
  "ForgeStageGrammar" -> "illegal instance for type family" `T.isInfixOf` diagnostics && "stagegrammar" `T.isInfixOf` diagnostics
  "ForgeRedirectForm" -> "illegal instance for type synonym" `T.isInfixOf` diagnostics && "redirectform" `T.isInfixOf` diagnostics
  "ControlPipelineStage" -> "illegal pipeline stage grammar" `T.isInfixOf` diagnostics
  "ExecPipelineStage" -> "illegal pipeline stage grammar" `T.isInfixOf` diagnostics
  "SubstitutionExecutable" -> mismatch "executable"
  "CoerceCommandName" -> mismatch "coerce"
  "RawCommandName" -> mismatch "commandname"
  "UnownedArtifactPayload" -> orderedWithin "does not export" "draftartifact" 30 diagnostics
  "RawVariableIdentifier" -> mismatch "varscalar"
  "RawSetIdentifier" -> mismatch "set"
  "RawFunctionParameter" -> mismatch "mkfishfunction"
  "CoerceIdentifier" -> mismatch "coerce"
  "CrossOwnerEntryBody" -> mismatch "sealsourceplan"
  "UnownedEntryBody" -> mismatch "entrybody"
  "WrongFunctionBody" -> mismatch "definefunction"
  "LoopAsFunctionRoot" -> mismatch "scopedbody"
  "CoerceBodyKind" -> mismatch "coerce"
  "IncompleteIntegerRequest" -> mismatch "primitive"
  "IncompletePrimitive" -> mismatch "primitive"
  "PrimitiveScalarArgument" -> mismatch "tlist"
  "CoercePrimitive" -> mismatch "coerce"
  "IncompleteSessionRequest" -> mismatch "request"
  "SessionReplyCardinality" -> mismatch "tlist"
  "CoerceSessionRequest" -> mismatch "coerce"
  "CoerceEmission" -> mismatch "coerce"
  "CrossRegionScalar" -> mismatch "runscalar"
  "CoerceRegionScalar" -> mismatch "coerce"
  "CrossLoopBodyTarget" -> mismatch "break"
  "CrossScopeLoopTarget" -> mismatch "break"
  "CrossScopeReturnTarget" -> mismatch "consumereturn"
  "CrossScopeArgumentsTarget" -> mismatch "consumesetarguments"
  "CoerceControlScope" -> mismatch "coerce"
  "CoerceWorld" -> mismatch "coerce"
  "CoerceDenseProof" -> mismatch "coerce"
  "CoerceDenseUpdate" -> mismatch "coerce"
  "StaleDenseUpdate" -> mismatch "denseupdate"
  "ReproveOldArrayShape" -> mismatch "lookupdense"
  "CrossWorldDenseProof" -> mismatch "writedenseat"
  "DraftArtifactInspection" -> mismatch "artifactentry"
  "CrossOwnerView" -> mismatch "coerce"
  "UnquotedExecutable" -> mismatch "executable"
  "CoerceOwnedPlan" -> mismatch "coerce"
  "CoerceArtifactPhase" -> mismatch "coerce"
  "CoerceProvider" -> mismatch "coerce"
  "UnownedLoopExit" -> mismatch "looptarget"
  "UnprovedNativeStatement" -> mismatch "nativeregionproof"
  "SessionNativeCapability" -> mismatch "false"
  _ -> False
  where
    diagnostics = T.toCaseFold raw
    mismatch symbol = "couldn't match" `T.isInfixOf` diagnostics && symbol `T.isInfixOf` diagnostics
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
        && (orderedWithin (T.toCaseFold moduleName) "hidden module" 100 diagnostics || orderedWithin (T.toCaseFold moduleName) "hidden package" 100 diagnostics)

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

-- | Pure release-linkage checks shared by the host-side package inspector.
module Monk.Tooling.Package
  ( Target (..),
    Linkage (..),
    inspectLinkage,
    packageReport,
    readTarget,
    verifyDescription,
    verifyArtifactIdentity,
    nativeCheckReceipt,
    checkerDigest,
    requiredNativeSuites,
    attestExecution,
    inspectPackage,
  )
where

import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Monk.Runtime.Digest (sha256)
import System.Exit (ExitCode (..))
import System.Info (arch, os)
import System.Process (readProcessWithExitCode)

data Target = X86_64Linux | Aarch64Linux | Aarch64Darwin
  deriving stock (Eq, Show)

data Linkage = Linkage
  { dynamicDependencies :: [Text],
    minimumVersion :: Maybe Text
  }
  deriving stock (Eq, Show)

-- | Inspect command output from readelf or the Darwin binary tools.
-- The caller must check each command's exit status before passing its output.
inspectLinkage :: Target -> Text -> Text -> Text -> Either Text Linkage
inspectLinkage target architecture programHeaders dependencies = case target of
  X86_64Linux -> inspectElf "Advanced Micro Devices X86-64"
  Aarch64Linux -> inspectElf "AArch64"
  Aarch64Darwin -> inspectMachO
  where
    inspectElf expectedMachine = do
      machine <-
        maybe (Left "ELF architecture is unavailable") Right $
          listToMaybe [T.strip (T.drop 1 rest) | line <- T.lines architecture, let (prefix, rest) = T.breakOn ":" (T.strip line), prefix == "Machine", not (T.null rest)]
      if machine /= expectedMachine
        then Left ("unexpected ELF architecture: " <> machine)
        else
          if any (T.isPrefixOf "INTERP" . T.strip) (T.lines programHeaders)
            then Left "Linux release contains a dynamic interpreter"
            else
              if "(NEEDED)" `T.isInfixOf` dependencies
                then Left "Linux release contains dynamic dependencies"
                else Right (Linkage [] Nothing)

    inspectMachO = do
      if T.strip architecture /= "arm64"
        then Left ("unexpected Mach-O architecture: " <> T.strip architecture)
        else Right ()
      let linked = [T.strip (fst (T.breakOn " (" line)) | line <- drop 1 (T.lines programHeaders), not (T.null (T.strip line))]
          allowed = ["/usr/lib/libSystem.B.dylib", "/usr/lib/libffi.dylib", "/usr/lib/libiconv.2.dylib"]
      case filter (`notElem` allowed) linked of
        bad : _ -> Left ("non-Apple dynamic dependency: " <> bad)
        [] -> Right ()
      let commands = map T.words (T.splitOn "Load command" dependencies)
          buildVersions =
            [ version
            | command <- commands,
              "LC_BUILD_VERSION" `elem` command,
              following "platform" command `elem` [Just "1", Just "macos"],
              Just version <- [following "minos" command]
            ]
          legacyVersions =
            [ version
            | command <- commands,
              "LC_VERSION_MIN_MACOSX" `elem` command,
              Just version <- [following "version" command]
            ]
          minimum = listToMaybe (buildVersions <> legacyVersions)
      maybe (Left "Mach-O minimum macOS deployment target is unavailable") (Right . Linkage linked . Just) minimum

following :: (Eq a) => a -> [a] -> Maybe a
following needle values = case dropWhile (/= needle) values of
  _ : result : _ -> Just result
  _ -> Nothing

readTarget :: String -> Either Text Target
readTarget name = case name of
  "x86_64-linux" -> Right X86_64Linux
  "aarch64-linux" -> Right Aarch64Linux
  "aarch64-darwin" -> Right Aarch64Darwin
  _ -> Left ("unknown target: " <> T.pack name)

targetName :: Target -> Text
targetName target = case target of
  X86_64Linux -> "x86_64-linux"
  Aarch64Linux -> "aarch64-linux"
  Aarch64Darwin -> "aarch64-darwin"

verifyDescription :: Target -> Text -> Either Text [Text]
verifyDescription target description = case T.lines description of
  [header, capabilities, reportedTarget]
    | header == "monk-runtime 2 bash53-i64" && reportedTarget == "target " <> targetName target -> Right (T.words capabilities)
  _ -> Left "executed runtime description does not match package ABI/profile/target"

-- | The package receipt must still describe the copied artifact that CI ran.
verifyArtifactIdentity :: Value -> B.ByteString -> Either Text ()
verifyArtifactIdentity (Object fields) bytes
  | KM.lookup "sha256" fields /= Just (String (T.pack (C.unpack (sha256 bytes)))) = Left "release artifact SHA-256 differs from package report"
  | KM.lookup "bytes" fields /= Just (toJSON (B.length bytes)) = Left "release artifact byte count differs from package report"
  | otherwise = Right ()
verifyArtifactIdentity _ _ = Left "release report must be a JSON object"

-- | A receipt is written only after its runtime suite completes successfully.
nativeCheckReceipt :: Text -> B.ByteString -> B.ByteString -> Value
nativeCheckReceipt suite binary checker =
  object
    [ "schema" .= (1 :: Int),
      "suite" .= suite,
      "runtime_sha256" .= checkerDigest binary,
      "checker_sha256" .= checkerDigest checker,
      "passed" .= True
    ]

checkerDigest :: B.ByteString -> Text
checkerDigest = T.pack . C.unpack . sha256

requiredNativeSuites :: [Text]
requiredNativeSuites =
  [ "protocol",
    "portable",
    "printf",
    "expansion",
    "session",
    "descriptors",
    "read",
    "process-substitution",
    "pattern-parts",
    "exec",
    "signals",
    "direct-output",
    "native-launcher",
    "child-transport"
  ]

-- | Attestation requires a complete set of receipts for this exact runtime
-- and checker, in addition to its inspected linkage and executed description.
attestExecution :: Value -> Text -> Text -> [Value] -> Either Text Value
attestExecution (Object fields) description checkerHash receipts = do
  if KM.lookup "linkage_verified" fields == Just (Bool True)
    then Right ()
    else Left "release linkage verification is missing"
  if KM.lookup "abi" fields == Just (toJSON (2 :: Int))
    && KM.lookup "profile" fields == Just (String "bash53-i64")
    then Right ()
    else Left "release ABI/profile is invalid"
  targetNameValue <- case KM.lookup "target" fields of
    Just (String value) -> Right value
    _ -> Left "release target is missing"
  target <- readTarget (T.unpack targetNameValue)
  capabilities <- verifyDescription target description
  runtimeHash <- case KM.lookup "sha256" fields of
    Just (String value) -> Right value
    _ -> Left "release runtime SHA-256 is missing"
  if length receipts == length requiredNativeSuites
    then Right ()
    else Left "missing native check receipts"
  forM_ requiredNativeSuites $ \suite ->
    case filter (\receipt -> receiptField "suite" receipt == Just (String suite)) receipts of
      [receipt]
        | receiptField "schema" receipt /= Just (toJSON (1 :: Int)) -> Left ("invalid native check receipt schema: " <> suite)
        | receiptField "passed" receipt /= Just (Bool True) -> Left ("native check did not pass: " <> suite)
        | receiptField "runtime_sha256" receipt /= Just (String runtimeHash) -> Left ("native check runtime hash differs: " <> suite)
        | receiptField "checker_sha256" receipt /= Just (String checkerHash) -> Left ("native check checker hash differs: " <> suite)
        | otherwise -> Right ()
      _ -> Left ("missing native check receipt: " <> suite)
  pure $
    Object $
      KM.insert "check_receipts_verified" (Bool True) $
        KM.insert "check_receipt_suites" (toJSON requiredNativeSuites) $
          KM.insert "execution_checks" (toJSON executionChecks) $
            KM.insert "runtime_capabilities" (toJSON capabilities) $
              KM.insert "runtime_description_verified" (Bool True) $
                KM.insert "execution_verified" (Bool True) fields
attestExecution _ _ _ _ = Left "release report must be a JSON object"

receiptField :: Text -> Value -> Maybe Value
receiptField key (Object fields) = KM.lookup (fromString (T.unpack key)) fields
receiptField _ _ = Nothing

executionChecks :: [Text]
executionChecks =
  [ "describe",
    "byte-protocol",
    "portable-descriptors",
    "printf",
    "expansion",
    "session",
    "descriptors",
    "read",
    "process-substitution",
    "pattern-parts",
    "exec",
    "signals",
    "direct-output",
    "native-launcher",
    "child-transport"
  ]

packageReport :: Target -> Linkage -> B.ByteString -> Text -> Value
packageReport target linkage binary host =
  object
    [ "target" .= targetName target,
      "format" .= format,
      "architecture" .= architecture,
      "dynamic_dependencies" .= dynamicDependencies linkage,
      "linkage_verified" .= True,
      "execution_verified" .= False,
      "sha256" .= C.unpack (sha256 binary),
      "bytes" .= B.length binary,
      "abi" .= (2 :: Int),
      "profile" .= ("bash53-i64" :: Text),
      "runtime_description_verified" .= False,
      "platform_requirements" .= requirements,
      "inspection_host" .= host
    ]
  where
    (format, architecture, requirements) = case target of
      X86_64Linux -> ("ELF" :: Text, "Advanced Micro Devices X86-64" :: Text, linuxRequirements)
      Aarch64Linux -> ("ELF", "AArch64", linuxRequirements)
      Aarch64Darwin -> ("Mach-O", "arm64", darwinRequirements)
    linuxRequirements =
      object
        [ "os" .= ("Linux" :: Text),
          "minimum_kernel" .= ("5.4" :: Text),
          "basis" .= ("Declared conservative release floor for the pinned musl/GHC toolchain and POSIX spawn, poll, Unix sockets, descriptor passing, and /dev/fd; not inferred from ELF headers." :: Text),
          "minimum_execution_verified" .= False,
          "descriptor_paths" .= ("Process substitution additionally requires usable /dev/fd paths; probed before effects." :: Text)
        ]
    darwinRequirements =
      object
        [ "os" .= ("macOS" :: Text),
          "minimum_version" .= minimumVersion linkage,
          "basis" .= ("Mach-O deployment-target load command" :: Text),
          "minimum_execution_verified" .= False,
          "descriptor_paths" .= ("Process substitution additionally requires usable /dev/fd paths; probed before effects." :: Text)
        ]

-- | Inspect an already built artifact on the build host. The artifact itself
-- is never executed unless the caller supplies a separate description file.
inspectPackage :: Target -> FilePath -> Maybe FilePath -> IO Value
inspectPackage target binary descriptionFile = do
  (architecture, linkageOutput, dependencyOutput) <- case target of
    X86_64Linux -> elfOutput
    Aarch64Linux -> elfOutput
    Aarch64Darwin -> (,,) <$> run "lipo" ["-archs", binary] <*> run "otool" ["-L", binary] <*> run "otool" ["-l", binary]
  linkage <- either (fail . T.unpack) pure (inspectLinkage target architecture linkageOutput dependencyOutput)
  bytes <- B.readFile binary
  let report = packageReport target linkage bytes (T.pack (os <> " " <> arch))
  case descriptionFile of
    Nothing -> pure report
    Just path -> do
      descriptionBytes <- B.readFile path
      description <- either (fail . show) pure (TE.decodeUtf8' descriptionBytes)
      capabilities <- either (fail . T.unpack) pure (verifyDescription target description)
      pure $ case report of
        Object fields -> Object $ KM.insert "runtime_capabilities" (toJSON capabilities) $ KM.insert "runtime_description_verified" (Bool True) fields
        value -> value
  where
    elfOutput = (,,) <$> run "readelf" ["-h", binary] <*> run "readelf" ["-l", binary] <*> run "readelf" ["-d", binary]
    run command arguments = do
      (status, output, errors) <- readProcessWithExitCode command arguments ""
      case status of
        ExitSuccess -> pure (T.pack output)
        ExitFailure code -> fail (command <> " exited " <> show code <> ": " <> errors)

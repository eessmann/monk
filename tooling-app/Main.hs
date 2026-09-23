module Main (main) where

import Data.Aeson (eitherDecodeStrict', encode)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Monk.Tooling.Boundaries (checkPublicBoundaries)
import Monk.Tooling.Evidence.Dispatch (runEvidence)
import Monk.Tooling.Package (attestExecution, checkerDigest, inspectPackage, nativeCheckReceipt, readTarget, requiredNativeSuites, verifyArtifactIdentity)
import Monk.Tooling.Parity (generateParityManifest)
import Monk.Tooling.Process (ProcessResult (..), ProcessSpec (..), runProcess)
import Monk.Tooling.Runtime.Checks (runChecks)
import Monk.Tooling.Summary (summaryReport)
import Options.Applicative
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import System.Environment qualified as Env
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose)
import System.Posix.IO (OpenMode (WriteOnly), creat, defaultFileFlags, exclusive, fdToHandle, openFd)
import System.Process (callProcess)

data Command
  = Inspect FilePath String (Maybe FilePath) (Maybe FilePath)
  | Attest FilePath FilePath FilePath FilePath FilePath
  | RuntimeCheck String FilePath (Maybe FilePath) (Maybe FilePath)
  | ParityManifest FilePath FilePath
  | BoundariesCheck FilePath FilePath (Maybe FilePath)
  | Profile String String
  | CompareBakeoff FilePath String String Double
  | Summary FilePath FilePath

main :: IO ()
main = do
  program <- Env.getProgName
  arguments <- Env.getArgs
  if program == "python3"
    then runEvidence ("historical-python" : arguments)
    else dispatch arguments

dispatch :: [String] -> IO ()
dispatch arguments = case arguments of
  "evidence" : "summary" : _ -> runParsed
  "evidence" : "--help" : _ -> runParsed
  "evidence" : rest -> runEvidence rest
  _ -> runParsed

runParsed :: IO ()
runParsed = do
  invocation <- execParser (info (commandParser <**> helper) (fullDesc <> progDesc "Monk development and evidence tools"))
  case invocation of
    Inspect binary targetName reportPath descriptionPath -> do
      target <- either (die . toString) pure (readTarget targetName)
      report <- inspectPackage target binary descriptionPath
      let bytes = encode report <> "\n"
      maybe (pure ()) (\path -> createDirectoryIfMissing True (takeDirectory path) >> BL.writeFile path bytes) reportPath
      BL.putStr bytes
    Attest packagePath binaryPath descriptionPath receiptsDirectory output -> do
      packageBytes <- B.readFile packagePath
      report <- either die pure (eitherDecodeStrict' packageBytes)
      binaryBytes <- B.readFile binaryPath
      either (die . toString) pure (verifyArtifactIdentity report binaryBytes)
      descriptionBytes <- B.readFile descriptionPath
      described <-
        runProcess
          ProcessSpec
            { executable = binaryPath,
              arguments = ["--describe"],
              workingDirectory = Nothing,
              environment = Nothing,
              stdinBytes = B.empty,
              timeoutMicros = 10000000
            }
      unless
        ( processExit described == ExitSuccess
            && not (processTimedOut described)
            && processStdout described == descriptionBytes
            && B.null (processStderr described)
        )
        $ die "executed runtime description differs from the supplied description file"
      description <- either (die . show) pure (TE.decodeUtf8' descriptionBytes)
      checker <- Env.getExecutablePath >>= B.readFile
      let checkerHash = checkerDigest checker
      receipts <- forM requiredNativeSuites $ \suite -> do
        bytes <- B.readFile (receiptsDirectory </> T.unpack suite <> ".json")
        either die pure (eitherDecodeStrict' bytes)
      verified <- either (die . toString) pure (attestExecution report description checkerHash receipts)
      createDirectoryIfMissing True (takeDirectory output)
      BL.writeFile output (encode verified <> "\n")
    RuntimeCheck suite runtimePath monkPath reportPath -> do
      binary <- canonicalizePath runtimePath
      if suite == "child-transport"
        then do
          let script = "test/native/child-transport.sh"
              expectedScriptHash = "5fa3e5f7af8745522c230564b6b70d3587df9f49e338d81509e8a09f5aee5a85"
          scriptBytes <- B.readFile script
          unless (checkerDigest scriptBytes == expectedScriptHash) $
            die "child transport check script differs from the version embedded in monk-tool"
          callProcess "bash" [script, binary]
        else runChecks suite binary monkPath
      forM_ reportPath $ \path -> do
        runtimeBytes <- B.readFile binary
        checkerBytes <- Env.getExecutablePath >>= B.readFile
        createDirectoryIfMissing True (takeDirectory path)
        descriptor <- openFd path WriteOnly defaultFileFlags {exclusive = True, creat = Just 0o666}
        handle <- fdToHandle descriptor
        BL.hPut handle (encode (nativeCheckReceipt (T.pack suite) runtimeBytes checkerBytes) <> "\n")
        hClose handle
    ParityManifest monkPath output -> do
      passed <- generateParityManifest monkPath output
      unless passed exitFailure
    BoundariesCheck compiler plan report -> do
      passed <- checkPublicBoundaries compiler plan report
      unless passed exitFailure
    Profile bash fish -> runEvidence ["profile", "--bash", bash, "--fish", fish]
    CompareBakeoff directory bash fish timeout -> runEvidence ["compare-bakeoff-bash", directory, "--bash", bash, "--fish", fish, "--timeout", show timeout]
    Summary input output -> do
      source <- canonicalizePath input
      raw <- B.readFile source
      report <- either die pure (summaryReport source raw)
      descriptor <- openFd output WriteOnly defaultFileFlags {exclusive = True, creat = Just 0o666}
      handle <- fdToHandle descriptor
      BL.hPut handle (encode report <> "\n")
      hClose handle

commandParser :: Parser Command
commandParser =
  hsubparser $
    command "runtime" (info runtimeParser (progDesc "Native runtime checks"))
      <> command "parity" (info parityParser (progDesc "Fixture admission manifests"))
      <> command "boundaries" (info boundariesParser (progDesc "Public API boundary checks"))
      <> command "profile" (info profileParser (progDesc "Inspect reference Bash and Fish versions"))
      <> command "compare" (info compareParser (progDesc "Compare translated output with Bash"))
      <> command "evidence" (info evidenceParser (progDesc "Evidence collection and summaries"))

profileParser :: Parser Command
profileParser =
  Profile
    <$> strOption (long "bash" <> value "bash" <> metavar "BASH")
    <*> strOption (long "fish" <> value "fish" <> metavar "FISH")

compareParser :: Parser Command
compareParser = hsubparser $ command "bakeoff-bash" (info compareBakeoffParser (progDesc "Compare a bakeoff report against Bash"))

compareBakeoffParser :: Parser Command
compareBakeoffParser =
  CompareBakeoff
    <$> strArgument (metavar "RUN_DIRECTORY")
    <*> strOption (long "bash" <> value "bash" <> metavar "BASH")
    <*> strOption (long "fish" <> value "fish" <> metavar "FISH")
    <*> option auto (long "timeout" <> value 30 <> metavar "SECONDS")

parityParser :: Parser Command
parityParser = hsubparser $ command "manifest" (info manifestParser (progDesc "Generate reviewed fixture admission TSV"))

manifestParser :: Parser Command
manifestParser =
  ParityManifest
    <$> strArgument (metavar "MONK_BIN")
    <*> strArgument (metavar "OUTPUT")

boundariesParser :: Parser Command
boundariesParser = hsubparser $ command "check" (info boundariesCheckParser (progDesc "Compile public and private API probes"))

boundariesCheckParser :: Parser Command
boundariesCheckParser =
  BoundariesCheck
    <$> strOption (long "ghc" <> value "ghc" <> metavar "GHC")
    <*> strOption (long "plan" <> value "dist-newstyle/cache/plan.json" <> metavar "PLAN")
    <*> optional (strOption (long "report" <> metavar "FILE"))

runtimeParser :: Parser Command
runtimeParser =
  hsubparser $
    command "inspect" (info inspectParser (progDesc "Check release architecture and linkage"))
      <> command "attest" (info attestParser (progDesc "Record successful native execution checks"))
      <> command "check" (info runtimeCheckParser (progDesc "Run a native runtime regression suite"))

evidenceParser :: Parser Command
evidenceParser = hsubparser $ command "summary" (info summaryParser (progDesc "Publish a compact evidence receipt"))

summaryParser :: Parser Command
summaryParser =
  Summary
    <$> strArgument (metavar "INPUT")
    <*> strArgument (metavar "OUTPUT")

inspectParser :: Parser Command
inspectParser =
  Inspect
    <$> strOption (long "binary" <> metavar "FILE" <> help "Runtime artifact to inspect")
    <*> strOption (long "target" <> metavar "TARGET" <> help "x86_64-linux, aarch64-linux or aarch64-darwin")
    <*> optional (strOption (long "report" <> metavar "FILE" <> help "Write the JSON report"))
    <*> optional (strOption (long "description-file" <> metavar "FILE" <> help "Previously captured --describe output"))

attestParser :: Parser Command
attestParser =
  Attest
    <$> strOption (long "package-report" <> metavar "FILE")
    <*> strOption (long "binary" <> metavar "FILE")
    <*> strOption (long "description-file" <> metavar "FILE")
    <*> strOption (long "check-receipts" <> metavar "DIRECTORY")
    <*> strOption (long "output" <> metavar "FILE")

runtimeCheckParser :: Parser Command
runtimeCheckParser =
  RuntimeCheck
    <$> strOption (long "suite" <> metavar "NAME")
    <*> strOption (long "runtime" <> metavar "FILE")
    <*> optional (strOption (long "monk" <> metavar "FILE"))
    <*> optional (strOption (long "report" <> metavar "FILE"))

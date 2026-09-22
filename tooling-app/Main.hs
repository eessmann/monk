module Main (main) where

import Data.Aeson (encode)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Monk.Tooling.Package (inspectPackage, readTarget)
import Monk.Tooling.Summary (summaryReport)
import Options.Applicative
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO (hClose)
import System.Posix.IO (OpenMode (WriteOnly), creat, defaultFileFlags, exclusive, fdToHandle, openFd)

data Command = Inspect FilePath String (Maybe FilePath) (Maybe FilePath) | Summary FilePath FilePath

main :: IO ()
main = do
  invocation <- execParser (info (commandParser <**> helper) (fullDesc <> progDesc "Monk development and evidence tools"))
  case invocation of
    Inspect binary targetName reportPath descriptionPath -> do
      target <- either (die . toString) pure (readTarget targetName)
      report <- inspectPackage target binary descriptionPath
      let bytes = encode report <> "\n"
      maybe (pure ()) (\path -> createDirectoryIfMissing True (takeDirectory path) >> BL.writeFile path bytes) reportPath
      BL.putStr bytes
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
      <> command "evidence" (info evidenceParser (progDesc "Evidence collection and summaries"))

runtimeParser :: Parser Command
runtimeParser = hsubparser $ command "inspect" (info inspectParser (progDesc "Check release architecture and linkage"))

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

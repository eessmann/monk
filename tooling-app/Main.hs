module Main (main) where

import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as BL
import Monk.Tooling.Package (inspectPackage, readTarget)
import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

data Command = Inspect FilePath String (Maybe FilePath) (Maybe FilePath)

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

commandParser :: Parser Command
commandParser = hsubparser $ command "runtime" (info runtimeParser (progDesc "Native runtime checks"))

runtimeParser :: Parser Command
runtimeParser = hsubparser $ command "inspect" (info inspectParser (progDesc "Check release architecture and linkage"))

inspectParser :: Parser Command
inspectParser =
  Inspect
    <$> strOption (long "binary" <> metavar "FILE" <> help "Runtime artifact to inspect")
    <*> strOption (long "target" <> metavar "TARGET" <> help "x86_64-linux, aarch64-linux or aarch64-darwin")
    <*> optional (strOption (long "report" <> metavar "FILE" <> help "Write the JSON report"))
    <*> optional (strOption (long "description-file" <> metavar "FILE" <> help "Previously captured --describe output"))

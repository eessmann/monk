module Bakeoff.Process
  ( ProcessOutput (..),
    runProcessText,
    writeJsonFile,
    readJsonFile,
    normalizeRuntimeStderr,
    writeComparisonFile,
  )
where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict', encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Path (Abs, Dir, File, Path, toFilePath)
import System.Exit (ExitCode)
import System.FilePath (dropTrailingPathSeparator)
import System.Process (CreateProcess, readCreateProcessWithExitCode)
import System.Timeout (timeout)

data ProcessOutput = MkProcessOutput
  { poExitCode :: ExitCode,
    poStdout :: Text,
    poStderr :: Text
  }
  deriving stock (Eq, Show)

runProcessText :: Maybe Int -> CreateProcess -> Text -> IO (Maybe ProcessOutput)
runProcessText timeoutSeconds process stdinInput =
  timeout timeoutMicros $ do
    (exitCode, out, err) <- readCreateProcessWithExitCode process (T.unpack stdinInput)
    pure
      MkProcessOutput
        { poExitCode = exitCode,
          poStdout = T.pack out,
          poStderr = T.pack err
        }
  where
    timeoutMicros = maybe maxBound secondsToMicros timeoutSeconds
    secondsToMicros seconds = seconds * 1_000_000

writeJsonFile :: (ToJSON a) => Path b File -> a -> IO ()
writeJsonFile path value =
  LBS.writeFile (toFilePath path) (encode value)

readJsonFile :: (FromJSON a) => Path b File -> IO a
readJsonFile path = do
  decoded <- eitherDecodeFileStrict' (toFilePath path)
  case decoded of
    Left err -> fail ("failed to decode JSON from " <> toFilePath path <> ": " <> err)
    Right value -> pure value

normalizeRuntimeStderr :: Path Abs Dir -> Text -> Text
normalizeRuntimeStderr outDir =
  T.replace ".babelfish.fish" ".fish"
    . T.replace ".monk.fish" ".fish"
    . T.replace (toText (dropTrailingPathSeparator (toFilePath outDir))) "<out_dir>"

writeComparisonFile :: Path b File -> Text -> Text -> Text -> Text -> IO ()
writeComparisonFile path leftName leftText rightName rightText =
  writeFileText (toFilePath path) $
    T.unlines
      [ "--- " <> leftName,
        leftText,
        "+++ " <> rightName,
        rightText
      ]

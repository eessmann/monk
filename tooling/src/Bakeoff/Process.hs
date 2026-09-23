module Bakeoff.Process
  ( ProcessOutput (..),
    runProcessText,
    writeJsonFile,
    readJsonFile,
    normalizeRuntimeStderr,
    writeComparisonFile,
  )
where

import Control.Exception (throwIO)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict', encode)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Monk.Host.Process qualified as Host
import Path (Abs, Dir, File, Path, toFilePath)
import System.Exit (ExitCode)
import System.FilePath (dropTrailingPathSeparator)
import System.Process (CreateProcess)

data ProcessOutput = MkProcessOutput
  { poExitCode :: ExitCode,
    poStdout :: Text,
    poStderr :: Text
  }
  deriving stock (Eq, Show)

runProcessText :: Maybe Int -> CreateProcess -> Text -> IO (Maybe ProcessOutput)
runProcessText timeoutSeconds process stdinInput = do
  result <- Host.runCreateProcess ((* 1_000_000) <$> timeoutSeconds) process (encodeUtf8 stdinInput)
  if Host.processTimedOut result
    then pure Nothing
    else do
      output <- either throwIO pure (TE.decodeUtf8' (Host.processStdout result))
      errors <- either throwIO pure (TE.decodeUtf8' (Host.processStderr result))
      pure
        ( Just
            MkProcessOutput
              { poExitCode = Host.processExit result,
                poStdout = output,
                poStderr = errors
              }
        )

writeJsonFile :: (ToJSON a) => Path b File -> a -> IO ()
writeJsonFile path value =
  LBS.writeFile (toFilePath path) (encode value)

readJsonFile :: (FromJSON a) => Path b File -> IO a
readJsonFile path = do
  decoded <- eitherDecodeFileStrict' (toFilePath path)
  case decoded of
    Left err -> fail ("failed to decode JSON from " <> toFilePath path <> ": " <> err)
    Right value -> pure value

normalizeRuntimeStderr :: Path Abs Dir -> B.ByteString -> B.ByteString
normalizeRuntimeStderr outDir =
  replaceBytes ".babelfish.fish" ".fish"
    . replaceBytes ".monk.fish" ".fish"
    . replaceBytes (encodeUtf8 (toText (dropTrailingPathSeparator (toFilePath outDir)))) "<out_dir>"

replaceBytes :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
replaceBytes needle replacement = B.concat . go
  where
    go bytes =
      let (before, after) = B.breakSubstring needle bytes
       in if B.null after then [before] else before : replacement : go (B.drop (B.length needle) after)

writeComparisonFile :: Path b File -> Text -> Text -> Text -> Text -> IO ()
writeComparisonFile path leftName leftText rightName rightText =
  writeFileText (toFilePath path) $
    T.unlines
      [ "--- " <> leftName,
        leftText,
        "+++ " <> rightName,
        rightText
      ]

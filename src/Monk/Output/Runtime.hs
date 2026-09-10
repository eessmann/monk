-- | Capture an executable image independently of its mutable provider path.
module Monk.Output.Runtime
  ( NativeRuntimeImage,
    nativeImageBytes,
    nativeImageDigest,
    nativeImageOperations,
    nativeImageABI,
    nativeImageProfile,
    captureNativeRuntime,
    validateNativeRuntimeFile,
  )
where

import Control.Exception (IOException, bracket, try)
import Data.ByteString qualified as BS
import Data.Set qualified as S
import GHC.Show qualified as GHC
import Monk.Runtime.Compatibility (checkRuntimeFile)
import Monk.Runtime.Digest (sha256)
import Monk.Translation.Types
import System.Directory (findExecutable, getTemporaryDirectory, makeAbsolute, removeFile)
import System.IO qualified as IO
import System.IO.Error (ioError, userError)
import System.Posix.Files (setFileMode)

data NativeRuntimeImage = MkNativeRuntimeImage BS.ByteString Text (S.Set NativeOperation)
  deriving stock (Eq)

instance GHC.Show NativeRuntimeImage where
  show image = "NativeRuntimeImage " <> show (nativeImageDigest image) <> " " <> show (BS.length (nativeImageBytes image)) <> " bytes " <> show (nativeImageOperations image)

nativeImageBytes :: NativeRuntimeImage -> BS.ByteString
nativeImageBytes (MkNativeRuntimeImage bytes _ _) = bytes

nativeImageDigest :: NativeRuntimeImage -> Text
nativeImageDigest (MkNativeRuntimeImage _ digest _) = digest

nativeImageOperations :: NativeRuntimeImage -> S.Set NativeOperation
nativeImageOperations (MkNativeRuntimeImage _ _ operations) = operations

nativeImageABI :: NativeRuntimeImage -> Int
nativeImageABI _ = 1

nativeImageProfile :: NativeRuntimeImage -> TargetProfile
nativeImageProfile _ = Bash53Signed64Fish46

captureNativeRuntime :: RuntimeSelection -> S.Set NativeOperation -> IO (Either Text NativeRuntimeImage)
captureNativeRuntime selection operations = do
  result <- try @IOException $ do
    provider <- case selection of
      RuntimeOnPath -> maybe (ioError (userError "monk-runtime is not on PATH")) pure =<< findExecutable "monk-runtime"
      RuntimePath path -> makeAbsolute path
      RuntimeGeneration _ -> ioError (userError "an installed provider is required when capturing a runtime image")
    bytes <- BS.readFile provider
    temporary <- getTemporaryDirectory
    checked <- bracket (IO.openBinaryTempFile temporary "monk-runtime-image") cleanup $ \(path, handle) -> do
      BS.hPut handle bytes
      IO.hClose handle
      setFileMode path 0o700
      validateNativeRuntimeFile path operations
    pure (MkNativeRuntimeImage bytes (decodeUtf8 (sha256 bytes)) operations <$ checked)
  pure (either (Left . show) id result)
  where
    cleanup (path, handle) = do
      _ <- try @IOException (IO.hClose handle)
      removeFile path

-- | Validate the exact staged executable, rather than reopening its provider.
validateNativeRuntimeFile :: FilePath -> S.Set NativeOperation -> IO (Either Text ())
validateNativeRuntimeFile path operations = first toText <$> checkRuntimeFile path (map (toString . nativeOperationName) (S.toList operations))

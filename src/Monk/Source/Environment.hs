-- | An explicit snapshot of Bash source lookup state. Discovery never falls
-- back to the directory containing the calling script.
module Monk.Source.Environment
  ( SourceEnvironment (..),
    captureSourceEnvironment,
    resolveSourcePathIn,
    SourceSnapshot,
    readSourceSnapshot,
    snapshotPath,
    snapshotText,
    snapshotIdentity,
  )
where

import Control.Exception (IOException, bracket, catch, throwIO, try)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Foreign.Ptr (castPtr)
import GHC.Fingerprint (Fingerprint, fingerprintData)
import Monk.Translation.Types
import System.Directory (canonicalizePath, doesFileExist, getCurrentDirectory, makeAbsolute)
import System.FilePath (isAbsolute, pathSeparator, searchPathSeparator, (</>))
import System.IO.Error (ioError, isEOFError, userError)
import System.Posix.Files (deviceID, fileID, fileSize, getFdStatus, isRegularFile, modificationTimeHiRes, statusChangeTimeHiRes)
import System.Posix.IO (OpenFileFlags (..), OpenMode (ReadOnly), closeFd, defaultFileFlags, openFd)
import System.Posix.IO.ByteString qualified as Posix
import System.Posix.Types (Fd)

-- | Relative PATH entries are relative to the execution cwd. The supported
-- profile is non-POSIX Bash, which tries cwd after a failed bare PATH lookup.
-- Runtime changes of these facts require fresh flow evidence before a source
-- occurrence can be admitted.
data SourceEnvironment = MkSourceEnvironment
  { sourceWorkingDirectory :: FilePath,
    sourceSearchPath :: [FilePath],
    sourcePathSearch :: Bool
  }
  deriving stock (Show, Eq, Ord)

captureSourceEnvironment :: IO SourceEnvironment
captureSourceEnvironment = do
  cwd <- getCurrentDirectory
  search <- maybe [] (map toString . T.split (== searchPathSeparator) . toText) <$> lookupEnv "PATH"
  pure (MkSourceEnvironment cwd search True)

resolveSourcePathIn :: SourceEnvironment -> Text -> IO (Either Diagnostic FilePath)
resolveSourcePathIn environment target
  | T.null target || T.any (== '\0') target = pure (Left (sourceError "invalid-path" "A source path must be nonempty and contain no NUL"))
  | not (isAbsolute (sourceWorkingDirectory environment)) = pure (Left (sourceError "invalid-environment" "Source discovery requires an absolute execution cwd"))
  | otherwise = do
      result <- try @IOException (firstExisting candidates)
      pure $ case result of
        Left err -> Left (sourceError "lookup" ("Cannot resolve source " <> target <> ": " <> show err))
        Right Nothing -> Left (sourceError "not-found" ("No source file found for " <> target <> " in the declared cwd/PATH"))
        Right (Just path) -> Right path
  where
    cwd = sourceWorkingDirectory environment
    name = toString target
    atCwd path = if isAbsolute path then path else cwd </> path
    candidates
      | pathSeparator `elem` name || not (sourcePathSearch environment) = [atCwd name]
      | otherwise = [atCwd dir </> name | dir <- sourceSearchPath environment] <> [atCwd name]
    firstExisting [] = pure Nothing
    firstExisting (path : rest) = do
      exists <- doesFileExist path
      if exists then Just <$> canonicalizePath path else firstExisting rest

-- | The bytes are owned by the graph even if the source later changes on disk.
-- The fingerprint is an inspection label, not authority: graph reuse is keyed
-- by canonical path and retains this exact immutable snapshot.
data SourceSnapshot = MkSourceSnapshot FilePath ByteString Text Fingerprint Text
  deriving stock (Show, Eq)

snapshotPath :: SourceSnapshot -> FilePath
snapshotPath (MkSourceSnapshot path _ _ _ _) = path

snapshotText :: SourceSnapshot -> Text
snapshotText (MkSourceSnapshot _ _ contents _ _) = contents

snapshotIdentity :: SourceSnapshot -> Text
snapshotIdentity (MkSourceSnapshot path _ _ fingerprint objectId) = toText path <> ":" <> objectId <> ":" <> show fingerprint

readSourceSnapshot :: FilePath -> IO (Either Diagnostic SourceSnapshot)
readSourceSnapshot path = do
  result <- try @IOException $ do
    absolute <- makeAbsolute path >>= canonicalizePath
    (contents, objectId) <- bracket
      (openFd absolute ReadOnly defaultFileFlags {nofollow = True, nonBlock = True, cloexec = True})
      closeFd
      $ \fd -> do
        before <- getFdStatus fd
        unless (isRegularFile before) (ioError (userError "Source input must be a regular file"))
        bytes <- readDescriptor fd
        after <- getFdStatus fd
        unless
          ( fileSize before == fileSize after
              && modificationTimeHiRes before == modificationTimeHiRes after
              && statusChangeTimeHiRes before == statusChangeTimeHiRes after
          )
          (ioError (userError "Source changed while its immutable snapshot was read"))
        pure (bytes, show (deviceID before) <> ":" <> show (fileID before))
    digest <- BS.useAsCStringLen contents $ \(ptr, size) -> fingerprintData (castPtr ptr) size
    pure (absolute, contents, digest, objectId)
  pure $ case result of
    Left err -> Left (sourceError "read" ("Cannot read source " <> toText path <> ": " <> show err))
    Right (absolute, contents, digest, objectId) -> case TE.decodeUtf8' contents of
      Left _ -> Left (sourceError "encoding" "The execution profile requires UTF-8 source text")
      Right text -> Right (MkSourceSnapshot absolute contents text digest objectId)

readDescriptor :: Fd -> IO ByteString
readDescriptor fd = go []
  where
    go chunks = do
      chunk <- Posix.fdRead fd 32768 `catch` \err -> if isEOFError err then pure mempty else throwIO (err :: IOException)
      if BS.null chunk then pure (BS.concat (reverse chunks)) else go (chunk : chunks)

sourceError :: Text -> Text -> Diagnostic
sourceError code message = MkDiagnostic (MkDiagnosticCode ("monk.source." <> code)) PhaseSource DiagnosticError Unsafe message Nothing

-- | Freeze command inputs before and after a local verification command.
module Monk.Tooling.Evidence.Verification
  ( sourceIdentity,
    productionIdentity,
    runVerification,
  )
where

import Data.Aeson (Value (..), encode, object, toJSON, (.=))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Bits ((.&.))
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.ByteString.Lazy qualified as BL
import Data.List (lookup, nub)
import Data.Text qualified as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Monk.Runtime.Digest (sha256)
import Monk.Tooling.Evidence.Common (Observation (..), arrayField, digestFile, field, hostPlatform, runObservation, textField, writeJson)
import System.Directory (canonicalizePath, createDirectory, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, findExecutable, getCurrentDirectory, listDirectory)
import System.Environment (getEnvironment, getExecutablePath)
import System.Exit (ExitCode (..))
import System.FilePath (makeRelative, takeDirectory, takeExtension, (</>))
import System.Posix.Files (fileMode, getFileStatus, getSymbolicLinkStatus, isDirectory, isSymbolicLink, readSymbolicLink)

productionDirs, buildDirs, productionFiles, buildFiles, ignoredDirs, generatedSuffixes :: [FilePath]
productionDirs = ["app", "src", "compiler-src", "foundation-src", "host-src", "publication-src", "runtime", "protocol", "nix", ".cargo"]
productionFiles = ["monk.cabal", "cabal.project", "cabal.project.freeze", "cabal.project.local", "Cargo.toml", "Cargo.lock", "rust-toolchain.toml", "devenv.nix", "devenv.yaml", "devenv.lock", "devenv.local.nix", "devenv.local.yaml", "Setup.hs"]
buildDirs = productionDirs <> ["test", "support-test", "benchmark", "scripts", "test-support", "docs", ".github", "tooling", "tooling-app", "tooling-test"]
buildFiles = productionFiles <> ["README.md", "CHANGELOG.md", "LICENSE", "LICENSE.md", ".hlint.yaml", ".ormolu", ".gitignore"]
ignoredDirs = ["__pycache__", ".git", ".devenv", ".direnv", "artifacts", "dist", "result", "target"]
generatedSuffixes = [".pyc", ".pyo", ".o", ".hi", ".dyn_o", ".dyn_hi", ".hie"]

-- Python's sorted-key JSON spelling is part of the historic fingerprint format.
pythonJson :: Value -> BL.ByteString
pythonJson = \case
  Object entries -> "{" <> BL.intercalate ", " [encode (K.toText key) <> ": " <> pythonJson value | (key, value) <- sortOn (K.toText . fst) (KM.toList entries)] <> "}"
  Array values -> "[" <> BL.intercalate ", " (map pythonJson (toList values)) <> "]"
  value -> encode value

makeIdentity :: [Value] -> Value
makeIdentity files = object ["sha256" .= C.unpack (sha256 (BL.toStrict (pythonJson (toJSON files)))), "files" .= files]

sourceIdentity :: FilePath -> IO Value
sourceIdentity root = do
  descendants <- concat <$> traverse (walk . (root </>)) buildDirs
  individual <- filterM doesFileExist (map (root </>) buildFiles)
  let paths = sort (nub (descendants <> individual))
  rows <- forM paths $ \path -> do
    bytes <- B.readFile path
    info <- getFileStatus path
    symlink <- isSymbolicLink <$> getSymbolicLinkStatus path
    target <- if symlink then Just <$> readSymbolicLink path else pure Nothing
    pure $ object $ ["path" .= makeRelative root path, "sha256" .= C.unpack (sha256 bytes), "mode" .= (fromIntegral (fileMode info .&. 0o7777) :: Int)] <> maybe [] (\name -> ["symlink_target" .= name]) target
  pure (makeIdentity rows)
  where
    walk folder = do
      exists <- doesDirectoryExist folder
      if not exists
        then pure []
        else do
          names <- sort <$> listDirectory folder
          fmap concat $ forM names $ \name -> do
            let path = folder </> name
            info <- getSymbolicLinkStatus path
            if isDirectory info
              then if name `elem` ignoredDirs || any (`isPrefixOf` name) ["dist-", "result-"] then pure [] else walk path
              else
                if takeExtension path `elem` generatedSuffixes
                  then pure []
                  else do
                    regular <- doesFileExist path
                    if regular then pure [path] else pure []

productionIdentity :: Value -> Either String Value
productionIdentity source = do
  files <- arrayField "files" source
  filtered <- filterM include files
  pure (makeIdentity filtered)
  where
    include row = do
      path <- T.unpack <$> textField "path" row
      pure $ path `elem` productionFiles || takeWhile (/= '/') path `elem` productionDirs

runVerification :: FilePath -> [FilePath] -> [String] -> IO ExitCode
runVerification output binaryPaths command = do
  when (null command) $ fail "verification command required after --"
  root <- getCurrentDirectory >>= canonicalizePath
  createDirectoryIfMissing True (takeDirectory output)
  createDirectory output
  outputPath <- canonicalizePath output
  before <- sourceIdentity root
  executable <- getExecutablePath
  collectorHash <- digestFile executable
  tools <- catMaybes <$> forM ["ghc", "cabal", "cargo", "rustc", "bash", "fish"] (\name -> fmap (name,) <$> findExecutable name)
  env <- getEnvironment
  let names = ["NIX_GHC", "NIX_GHCPKG", "NIX_GHC_LIBDIR", "CABAL_CONFIG", "LC_ALL", "LANG", "TMPDIR"]
      selected = object [fromString key .= lookup key env | key <- names]
  started <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q+00:00" started
  observed <- runObservation command "" root env 3600
  B.writeFile (outputPath </> "command.log") (observedStdout observed <> observedStderr observed)
  after <- sourceIdentity root
  binaries <- forM binaryPaths $ \raw -> do
    path <- canonicalizePathSafe raw
    present <- doesFileExist path
    if present
      then do
        hash <- digestFile path
        bytes <- B.length <$> B.readFile path
        pure $ object ["path" .= path, "sha256" .= hash, "bytes" .= bytes]
      else pure $ object ["path" .= path, "missing" .= True]
  let stable = field "sha256" before == field "sha256" after
      complete = all ((/= Right (Bool True)) . field "missing") binaries
  productionBefore <- either fail pure (productionIdentity before)
  productionAfter <- either fail pure (productionIdentity after)
  logHash <- digestFile (outputPath </> "command.log")
  platform <- hostPlatform
  let report =
        object
          [ "schema" .= (2 :: Int),
            "timestamp" .= timestamp,
            "cwd" .= root,
            "command" .= command,
            "exit" .= exit observed,
            "command_status" .= status observed,
            "elapsed_seconds" .= (fromIntegral (elapsedNs observed) / (1000000000 :: Double)),
            "before" .= before,
            "after" .= after,
            "build_inputs_unchanged" .= stable,
            "production_before" .= productionBefore,
            "production_after" .= productionAfter,
            "production_inputs_unchanged" .= (field "sha256" productionBefore == field "sha256" productionAfter),
            "successful_stable_command" .= (status observed == "completed" && exit observed == 0 && stable && complete),
            "requested_binaries_present" .= complete,
            "binaries" .= binaries,
            "log_sha256" .= logHash,
            "collector_sha256" .= collectorHash,
            "toolchain_paths" .= object ([fromString name .= path | (name, path) <- tools] <> ["monk-tool" .= executable]),
            "environment" .= selected,
            "platform" .= platform
          ]
  writeJson (outputPath </> "receipt.json") report
  pure $ if status observed == "timeout" then ExitFailure 124 else if exit observed /= 0 then ExitFailure (exit observed) else if not stable then ExitFailure 3 else if not complete then ExitFailure 4 else ExitSuccess
  where
    canonicalizePathSafe path = do
      present <- doesFileExist path
      if present
        then canonicalizePath path
        else do
          root <- getCurrentDirectory
          pure $ if "/" `isPrefixOf` path then path else root </> path

-- | Byte preserving building blocks for independent runtime evidence.
module Monk.Tooling.Evidence.Common
  ( Observation (..),
    runObservation,
    observationRecord,
    nativeRecord,
    compareObservations,
    compareEffects,
    base64,
    unbase64,
    digestFile,
    writeJson,
    readJson,
    field,
    textField,
    arrayField,
    boolField,
    numberField,
    snapshot,
    cleanEnvironment,
    commandEnvironment,
    provenance,
    copyTree,
    hostPlatform,
  )
where

import Data.Aeson (Result (..), Value (..), eitherDecodeStrict', encode, fromJSON, object, toJSON, (.=))
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap qualified as KM
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.ByteString.Lazy qualified as BL
import Data.List (lookup)
import Data.Text qualified as T
import GHC.Clock (getMonotonicTimeNSec)
import Monk.Runtime.Digest (sha256)
import Monk.Tooling.Process (ProcessResult (..), ProcessSpec (..), runProcess)
import System.Directory (canonicalizePath, copyFileWithMetadata, createDirectoryIfMissing, doesDirectoryExist, getCurrentDirectory, listDirectory)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath (makeRelative, (</>))
import System.Posix.Files (fileMode, getSymbolicLinkStatus, isDirectory, isNamedPipe, isRegularFile, isSymbolicLink, readSymbolicLink)

data Observation = Observation
  { status :: Text,
    exit :: Int,
    observedStdout :: B.ByteString,
    observedStderr :: B.ByteString,
    elapsedNs :: Word64
  }
  deriving stock (Eq, Show)

runObservation :: [String] -> B.ByteString -> FilePath -> [(String, String)] -> Int -> IO Observation
runObservation [] _ _ _ _ = fail "empty evidence command"
runObservation (program : args) input cwd env seconds = do
  start <- getMonotonicTimeNSec
  result <- runProcess ProcessSpec {executable = program, arguments = args, workingDirectory = Just cwd, environment = Just env, stdinBytes = input, timeoutMicros = max 0 seconds * 1000000}
  end <- getMonotonicTimeNSec
  pure Observation {status = if processTimedOut result then "timeout" else "completed", exit = exitNumber (processExit result), observedStdout = processStdout result, observedStderr = processStderr result, elapsedNs = end - start}

exitNumber :: ExitCode -> Int
exitNumber ExitSuccess = 0
exitNumber (ExitFailure code) = code

observationRecord :: FilePath -> String -> [String] -> Observation -> IO Value
observationRecord directory label command observed = do
  B.writeFile (directory </> label <> ".stdout") (observedStdout observed)
  B.writeFile (directory </> label <> ".stderr") (observedStderr observed)
  pure $ object ["command" .= command, "status" .= status observed, "exit" .= exit observed, "stdout" .= stream (observedStdout observed), "stderr" .= stream (observedStderr observed)]
  where
    stream bytes = object ["bytes" .= B.length bytes, "sha256" .= C.unpack (sha256 bytes), "base64" .= base64 bytes]

nativeRecord :: Observation -> Value
nativeRecord observed = object ["elapsed_ns" .= elapsedNs observed, "exit" .= exit observed, "stdout" .= base64 (observedStdout observed), "stderr" .= base64 (observedStderr observed)]

compareObservations :: Observation -> Observation -> Value
compareObservations baseline candidate =
  object ["status" .= outcome, "differences" .= differences]
  where
    available = status baseline == "completed" && status candidate == "completed"
    differences = if not available then [] else [name | (name, different) <- [("stdout" :: Text, observedStdout baseline /= observedStdout candidate), ("stderr", observedStderr baseline /= observedStderr candidate), ("exit", exit baseline /= exit candidate)], different]
    outcome :: Text
    outcome | not available = "unavailable" | null differences = "match" | otherwise = "mismatch"

compareEffects :: Observation -> Observation -> Maybe Value -> Maybe Value -> Value
compareEffects baseline candidate before after = case compareObservations baseline candidate of
  Object values ->
    let raw = case KM.lookup "differences" values of
          Just (Array entries) -> toList entries
          _ -> []
        differences = raw <> [String "filesystem" | isJust before && before /= after && KM.lookup "status" values /= Just (String "unavailable")]
        outcome
          | KM.lookup "status" values == Just (String "unavailable") = String "unavailable"
          | null differences = String "match"
          | otherwise = String "mismatch"
     in Object $ KM.insert "status" outcome $ KM.insert "differences" (toJSON differences) values
  value -> value

alphabet :: B.ByteString
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

base64 :: B.ByteString -> Text
base64 bytes = T.pack (go (B.unpack bytes))
  where
    at n = toEnum (fromIntegral (B.index alphabet (fromIntegral n)))
    go (a : b : c : rest) = at (a `shiftR` 2) : at (((a .&. 3) `shiftL` 4) .|. (b `shiftR` 4)) : at (((b .&. 15) `shiftL` 2) .|. (c `shiftR` 6)) : at (c .&. 63) : go rest
    go [a, b] = [at (a `shiftR` 2), at (((a .&. 3) `shiftL` 4) .|. (b `shiftR` 4)), at ((b .&. 15) `shiftL` 2), '=']
    go [a] = [at (a `shiftR` 2), at ((a .&. 3) `shiftL` 4), '=', '=']
    go [] = []

unbase64 :: Text -> Either String B.ByteString
unbase64 input = B.pack . concat <$> traverse decodeChunk (chunks (T.unpack input))
  where
    chunks [] = []
    chunks xs = let (part, rest) = splitAt 4 xs in part : chunks rest
    digit c = maybe (Left ("invalid base64 character: " <> [c])) (Right . fromIntegral) (B.elemIndex (fromIntegral (fromEnum c)) alphabet)
    decodeChunk [a, b, '=', '='] = do
      x <- digit a
      y <- digit b
      pure [x `shiftL` 2 .|. y `shiftR` 4]
    decodeChunk [a, b, c, '='] = do
      x <- digit a
      y <- digit b
      z <- digit c
      pure [x `shiftL` 2 .|. y `shiftR` 4, y `shiftL` 4 .|. z `shiftR` 2]
    decodeChunk [a, b, c, d] = do
      x <- digit a
      y <- digit b
      z <- digit c
      w <- digit d
      pure [x `shiftL` 2 .|. y `shiftR` 4, y `shiftL` 4 .|. z `shiftR` 2, z `shiftL` 6 .|. w]
    decodeChunk _ = Left "invalid base64 length"

digestFile :: FilePath -> IO String
digestFile path = C.unpack . sha256 <$> B.readFile path

writeJson :: FilePath -> Value -> IO ()
writeJson path value = BL.writeFile path (encode value <> "\n")

readJson :: FilePath -> IO Value
readJson path = B.readFile path >>= either fail pure . eitherDecodeStrict'

field :: Text -> Value -> Either String Value
field key (Object values) = maybe (Left ("missing JSON key: " <> T.unpack key)) Right (KM.lookup (fromText key) values)
field key _ = Left ("expected JSON object containing " <> T.unpack key)

textField :: Text -> Value -> Either String Text
textField key value =
  field key value >>= \case
    String result -> Right result
    _ -> Left ("expected text key: " <> T.unpack key)

arrayField :: Text -> Value -> Either String [Value]
arrayField key value =
  field key value >>= \case
    Array result -> Right (toList result)
    _ -> Left ("expected array key: " <> T.unpack key)

boolField :: Text -> Value -> Either String Bool
boolField key value =
  field key value >>= \case
    Bool result -> Right result
    _ -> Left ("expected boolean key: " <> T.unpack key)

numberField :: Text -> Value -> Either String Int
numberField key value =
  field key value >>= \case
    Number result -> case fromJSON (Number result) of
      Success integer -> Right integer
      Error _ -> Left ("expected integer key: " <> T.unpack key)
    _ -> Left ("expected number key: " <> T.unpack key)

snapshot :: FilePath -> IO Value
snapshot root = do
  children <- sortedPaths root
  object <$> traverse row children
  where
    sortedPaths folder = do
      names <- sort <$> listDirectory folder
      fmap concat $ forM names $ \name -> do
        let path = folder </> name
        info <- getSymbolicLinkStatus path
        descendants <- if isDirectory info then sortedPaths path else pure []
        pure (path : descendants)
    row path = do
      info <- getSymbolicLinkStatus path
      let mode = fromIntegral (fileMode info .&. 0o7777) :: Int
      item <-
        if isRegularFile info
          then do
            bytes <- B.readFile path
            pure $ object ["mode" .= mode, "type" .= ("file" :: Text), "sha256" .= C.unpack (sha256 bytes), "bytes_base64" .= base64 bytes]
          else
            if isSymbolicLink info
              then object . ("mode" .= mode :) . ("type" .= ("symlink" :: Text) :) . pure . ("target" .=) <$> readSymbolicLink path
              else pure $ object ["mode" .= mode, "type" .= (if isDirectory info then "directory" else if isNamedPipe info then "fifo" else "special" :: Text)]
      pure (fromString (makeRelative root path), item)

cleanEnvironment :: FilePath -> IO [(String, String)]
cleanEnvironment output = do
  inherited <- getEnvironment
  pure $ [("LC_ALL", "C"), ("LANG", "C"), ("XDG_CONFIG_HOME", output </> "config")] <> filter (\(key, _) -> key `notElem` ["LC_ALL", "LANG", "XDG_CONFIG_HOME", "BASH_ENV", "ENV", "SHELLOPTS", "BASHOPTS", "CDPATH"]) inherited

commandEnvironment :: [(String, String)] -> Value
commandEnvironment env = object [fromString key .= lookup key env | key <- ["LC_ALL", "LANG", "PATH", "XDG_CONFIG_HOME"]]

provenance :: FilePath -> IO Value
provenance path = do
  resolved <- canonicalizePath path
  hash <- digestFile resolved
  pure $ object ["path" .= resolved, "sha256" .= hash]

copyTree :: FilePath -> FilePath -> IO ()
copyTree source target = do
  createDirectoryIfMissing True target
  names <- listDirectory source
  forM_ names $ \name -> do
    let from = source </> name
        to = target </> name
    directory <- doesDirectoryExist from
    if directory then copyTree from to else copyFileWithMetadata from to

hostPlatform :: IO Text
hostPlatform = do
  cwd <- getCurrentDirectory
  env <- getEnvironment
  observed <- runObservation ["uname", "-srm"] "" cwd env 10
  unless (status observed == "completed" && exit observed == 0) $ fail "uname -srm failed while recording host platform"
  pure (T.strip (T.pack (C.unpack (observedStdout observed))))

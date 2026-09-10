{-# LANGUAGE OverloadedStrings #-}

-- | Bounded conversion of the actual Fish cd diagnostic. This operation never
-- probes the requested path and never changes its own or its parent's cwd.
module Monk.Runtime.Directory (directoryDiagnostic, physicalDirectory, directoryStack, validateDirectories, initialOldpwdValid, directoryPathBound) where

import Control.Exception (IOException, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.Either (fromRight)
import Monk.Runtime.Protocol (protocolFailure)
import Numeric (showOct)
import System.IO (stderr)
import System.Posix.Directory.ByteString qualified as Posix
import System.Posix.Files.ByteString qualified as Files

directoryStack :: [ByteString] -> Either ByteString ByteString
directoryStack (cwd : home : stack) = Right (B.intercalate " " (map display (cwd : stack)) <> "\n")
  where
    display path
      | not (B.null home), path == home = "~"
      | not (B.null home), (home <> "/") `B.isPrefixOf` path = "~" <> B.drop (B.length home) path
      | otherwise = path
directoryStack _ = Left "directory stack needs cwd and HOME frames"

physicalDirectory :: IO ()
physicalDirectory = Posix.getWorkingDirectory >>= B.putStr . (<> "\n")

directoryDiagnostic :: ByteString -> IO ()
directoryDiagnostic input = case metadata 4 input of
  Just ([origin, line, operation, operand], actual)
    | operation `elem` ["cd", "pushd", "popd"] ->
        if B.null actual
          then pure ()
          else case reason operand actual of
            Just message -> B.hPut stderr (origin <> ": line " <> line <> ": " <> operation <> ": " <> bashDiagnosticPath operand <> ": " <> message <> "\n")
            Nothing -> protocolFailure "unrecognized Fish 4.6 C-locale cd diagnostic"
  _ -> protocolFailure "invalid directory diagnostic metadata"
  where
    metadata :: Int -> ByteString -> Maybe ([ByteString], ByteString)
    metadata 0 rest = Just ([], rest)
    metadata count rest = do
      index <- B.elemIndex 0 rest
      (fields, remaining) <- metadata (count - 1) (B.drop (index + 1) rest)
      pure (B.take index rest : fields, remaining)
    reason operand actual
      | ("cd: The directory '" <> operand <> "' does not exist\n") `B.isPrefixOf` actual = Just "No such file or directory"
      | ("cd: '" <> operand <> "' is not a directory\n") `B.isPrefixOf` actual = Just "Not a directory"
      | ("cd: Permission denied: '" <> operand <> "'\n") `B.isPrefixOf` actual = Just "Permission denied"
      | ("cd: Too many levels of symbolic links: '" <> operand <> "'\n") `B.isPrefixOf` actual = Just "Too many levels of symbolic links"
      | ("cd: File name too long\ncd: Unknown error trying to locate directory '" <> operand <> "'\n") `B.isPrefixOf` actual = Just "File name too long"
      | otherwise = Nothing

-- PWD validity is a boundary check, never a substitute for attempting cd.
validateDirectories :: [ByteString] -> IO Bool
validateDirectories (pwd : stack)
  | all ordinary (pwd : stack) = do
      result <- try @IOException $ do
        logical <- Files.getFileStatus pwd
        actual <- Files.getFileStatus "."
        pure (Files.deviceID logical == Files.deviceID actual && Files.fileID logical == Files.fileID actual)
      pure (fromRight False result)
  | otherwise = pure False
  where
    ordinary path = not (B.null path) && B.head path == 47 && B.length path <= 4095 && all ((<= 255) . B.length) (B.split 47 path) && all (`notElem` [".", ".."]) (B.split 47 path)
validateDirectories _ = pure False

-- Bash startup retains inherited OLDPWD only when it names a directory.
initialOldpwdValid :: ByteString -> IO Bool
initialOldpwdValid path = do
  result <- try @IOException (Files.getFileStatus path)
  pure (either (const False) Files.isDirectory result)

-- Bash's C-locale diagnostic quotes the whole operand when any byte is not
-- printable ASCII, including embedded newlines and UTF-8 multibyte text.
bashDiagnosticPath :: ByteString -> ByteString
bashDiagnosticPath path
  | B.any (\byte -> byte < 32 || byte >= 127) path = "$'" <> B.concatMap escaped path <> "'"
  | otherwise = path
  where
    escaped byte = case byte of
      7 -> "\\a"
      8 -> "\\b"
      9 -> "\\t"
      10 -> "\\n"
      11 -> "\\v"
      12 -> "\\f"
      13 -> "\\r"
      27 -> "\\E"
      39 -> "\\'"
      92 -> "\\\\"
      _
        | byte < 32 || byte >= 127 -> let digits = showOct byte "" in C.pack ('\\' : replicate (3 - length digits) '0' <> digits)
        | otherwise -> B.singleton byte

-- A lexical bound protects the parent builtin's stderr transport: Fish emits
-- oversized resolved-path errors outside that builtin's redirected stream.
directoryPathBound :: ByteString -> ByteString -> Bool
directoryPathBound cwd operand =
  let path = if B.isPrefixOf "/" operand then operand else cwd <> "/" <> operand
      prefix = if B.isPrefixOf "//" path && not (B.isPrefixOf "///" path) then "//" else "/"
      parts = reverse (foldl component [] (B.split 47 path))
      component prior "" = prior
      component prior "." = prior
      component prior ".." = drop 1 prior
      component prior part = part : prior
   in B.length (prefix <> B.intercalate "/" parts) <= 4095

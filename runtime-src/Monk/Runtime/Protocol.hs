{-# LANGUAGE OverloadedStrings #-}

module Monk.Runtime.Protocol (decodeFrames, encodeFrames, protocolFailure) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (stderr)

decodeFrames :: ByteString -> Either ByteString [ByteString]
decodeFrames bytes
  | B.null bytes = Right []
  | bytes == B.singleton 0 = Right [B.empty]
  | B.last bytes /= 0 = Left "unterminated frame"
  | otherwise = Right (B.split 0 (B.init bytes))

encodeFrames :: [ByteString] -> ByteString
encodeFrames = B.concat . map (<> B.singleton 0)

protocolFailure :: ByteString -> IO a
protocolFailure message = B.hPut stderr ("monk-runtime: " <> message <> "\n") >> exitWith (ExitFailure 125)

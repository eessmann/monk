-- | SHA-256 generation identity as 64 lowercase hexadecimal ASCII bytes.
-- The package implementation owns compression and padding; the public digest
-- representation remains stable for manifests and immutable generation names.
module Monk.Runtime.Digest (sha256) where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Builder (toLazyByteString, word8HexFixed)
import Data.ByteString.Lazy qualified as L

sha256 :: ByteString -> ByteString
sha256 input = L.toStrict (toLazyByteString hexadecimal)
  where
    hexadecimal = foldMap word8HexFixed (B.unpack (SHA256.hash input))

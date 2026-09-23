-- | SHA-256 generation identity, specified by FIPS 180-4. All arithmetic is
-- explicitly Word32; each compression consumes exactly one 64-byte block.
module Monk.Runtime.Digest (sha256) where

import Data.Array (Array, listArray, (!))
import Data.Bits (complement, rotateR, shiftR, xor, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.Word (Word32, Word64)
import Numeric (showHex)

sha256 :: ByteString -> ByteString
sha256 input = C.pack (concatMap hex final)
  where
    byteLength = B.length input
    bitLength = fromIntegral byteLength * 8 :: Word64
    padding = B.singleton 128 <> B.replicate ((55 - byteLength) `mod` 64) 0 <> B.pack [fromIntegral (bitLength `shiftR` i) | i <- [56, 48 .. 0]]
    final = consume initial (input <> padding)
    consume state bytes
      | B.null bytes = state
      | otherwise = let next = compress state (B.take 64 bytes) in foldr seq (consume next (B.drop 64 bytes)) next
    hex w = let digits = showHex w "" in replicate (8 - length digits) '0' <> digits

initial :: [Word32]
initial = [0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19]

compress :: [Word32] -> ByteString -> [Word32]
compress state block = zipWith (+) state (foldl' step state [0 .. 63])
  where
    schedule :: Array Int Word32
    schedule = listArray (0, 63) [word i | i <- [0 .. 63]]
    word i
      | i < 16 = B.foldl' (\n b -> n * 256 + fromIntegral b) 0 (B.take 4 (B.drop (4 * i) block))
      | otherwise = small1 (schedule ! (i - 2)) + schedule ! (i - 7) + small0 (schedule ! (i - 15)) + schedule ! (i - 16)
    step [a, b, c, d, e, f, g, h] i =
      let t1 = h + big1 e + choose e f g + constants ! i + schedule ! i
          t2 = big0 a + majority a b c
       in [t1 + t2, a, b, c, d + t1, e, f, g]
    step _ _ = error "SHA-256 internal state invariant"

small0, small1, big0, big1 :: Word32 -> Word32
small0 x = rotateR x 7 `xor` rotateR x 18 `xor` shiftR x 3
small1 x = rotateR x 17 `xor` rotateR x 19 `xor` shiftR x 10
big0 x = rotateR x 2 `xor` rotateR x 13 `xor` rotateR x 22
big1 x = rotateR x 6 `xor` rotateR x 11 `xor` rotateR x 25

choose, majority :: Word32 -> Word32 -> Word32 -> Word32
choose x y z = (x .&. y) `xor` (complement x .&. z)
majority x y z = (x .&. y) `xor` (x .&. z) `xor` (y .&. z)

constants :: Array Int Word32
constants =
  listArray
    (0, 63)
    [ 0x428a2f98,
      0x71374491,
      0xb5c0fbcf,
      0xe9b5dba5,
      0x3956c25b,
      0x59f111f1,
      0x923f82a4,
      0xab1c5ed5,
      0xd807aa98,
      0x12835b01,
      0x243185be,
      0x550c7dc3,
      0x72be5d74,
      0x80deb1fe,
      0x9bdc06a7,
      0xc19bf174,
      0xe49b69c1,
      0xefbe4786,
      0x0fc19dc6,
      0x240ca1cc,
      0x2de92c6f,
      0x4a7484aa,
      0x5cb0a9dc,
      0x76f988da,
      0x983e5152,
      0xa831c66d,
      0xb00327c8,
      0xbf597fc7,
      0xc6e00bf3,
      0xd5a79147,
      0x06ca6351,
      0x14292967,
      0x27b70a85,
      0x2e1b2138,
      0x4d2c6dfc,
      0x53380d13,
      0x650a7354,
      0x766a0abb,
      0x81c2c92e,
      0x92722c85,
      0xa2bfe8a1,
      0xa81a664b,
      0xc24b8b70,
      0xc76c51a3,
      0xd192e819,
      0xd6990624,
      0xf40e3585,
      0x106aa070,
      0x19a4c116,
      0x1e376c08,
      0x2748774c,
      0x34b0bcb5,
      0x391c0cb3,
      0x4ed8aa4a,
      0x5b9cca4f,
      0x682e6ff3,
      0x748f82ee,
      0x78a5636f,
      0x84c87814,
      0x8cc70208,
      0x90befffa,
      0xa4506ceb,
      0xbef9a3f7,
      0xc67178f2
    ]

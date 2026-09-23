{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import Data.ByteString qualified as B
import Data.List (nub)
import Monk.Runtime.Abi2
import Monk.Runtime.Digest (sha256)
import Monk.Runtime.Integer (integerOperation, integerValue, parseNumber)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if args == ["--digest"] then B.getContents >>= B.putStr . sha256 else tests

tests :: IO ()
tests = do
  let assert label condition = unless condition (fail label)
  assert "empty integer operation rejected" (either (const True) (const False) (integerOperation []))
  assert "batch wraps intermediate results" (integerValue "batch" ["push", "9223372036854775807", "push", "1", "add", "push", "3", "mul"] == Right (-9223372036854775808))
  assert "batch preserves operand order" (integerValue "batch" ["push", "3", "push", "4", "sub", "neg"] == Right 1)
  assert "batch rejects stack underflow" (integerOperation ["batch", "push", "1", "add"] == Left "invalid-batch")
  assert "batch excludes failing primitives" (integerOperation ["batch", "push", "1", "push", "0", "div"] == Left "invalid-batch")
  assert "batch reads empty scalar as zero" (integerValue "batch" ["push", "", "push", "2", "add", "neg"] == Right (-2))
  assert "signed overflow" (integerValue "add" ["9223372036854775807", "1"] == Right (-9223372036854775808))
  assert "bounded modular power" (integerValue "pow" ["2", "9223372036854775807"] == Right 0)
  assert "negative exponent" (integerValue "pow" ["2", "-1"] == Left "negative-exponent")
  assert "base64 numeric digits" (parseNumber "64#_" == Right 63)
  assert "invalid base" (parseNumber "01#1" == Left "invalid-number")
  assert "invalid byte number" (parseNumber (B.pack [255]) == Left "invalid-number")
  assert "SHA256 empty" (sha256 "" == "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
  assert "SHA256 abc" (sha256 "abc" == "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
  assert "SHA256 multiblock" (sha256 "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq" == "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1")
  assert "SHA256 binary padding boundary 55" (sha256 (B.pack (take 55 (cycle [0 .. 255]))) == "463eb28e72f82e0a96c0a4cc53690c571281131f672aa229e0d45ae59b598b59")
  assert "SHA256 binary padding boundary 56" (sha256 (B.pack (take 56 (cycle [0 .. 255]))) == "da2ae4d6b36748f2a318f23e7ab1dfdf45acdc9d049bd80e59de82a60895f562")
  assert "SHA256 binary padding boundary 63" (sha256 (B.pack (take 63 (cycle [0 .. 255]))) == "29af2686fd53374a36b0846694cc342177e428d1647515f078784d69cdb9e488")
  assert "SHA256 binary padding boundary 64" (sha256 (B.pack (take 64 (cycle [0 .. 255]))) == "fdeab9acf3710362bd2658cdc9a29e8f9c757fcf9811603a8c447cd1d9151108")
  assert "SHA256 binary padding boundary 65" (sha256 (B.pack (take 65 (cycle [0 .. 255]))) == "4bfd2c8b6f1eec7a2afeb48b934ee4b2694182027e6d0fc075074f2fabb31781")
  assert "SHA256 binary padding boundary 127" (sha256 (B.pack (take 127 (cycle [0 .. 255]))) == "92ca0fa6651ee2f97b884b7246a562fa71250fedefe5ebf270d31c546bfea976")
  assert "SHA256 binary padding boundary 128" (sha256 (B.pack (take 128 (cycle [0 .. 255]))) == "471fb943aa23c511f6f72f8d1652d9c880cfa392ad80503120547703e56a2be5")
  assert "SHA256 binary padding boundary 129" (sha256 (B.pack (take 129 (cycle [0 .. 255]))) == "5099c6a56203f9687f7d33f4bfdf576d31dc91f6b695ecea38b2770c87631135")
  assert "native operation inventory is unique" (unique (map abiOperationName [minBound .. maxBound]))
  let opcodes = [("cli", map cliOpcodeName [minBound .. maxBound]), ("session", map sessionOpcodeName [minBound .. maxBound]), ("integer", map integerOpcodeName [minBound .. maxBound]), ("pattern", map patternOpcodeName [minBound .. maxBound]), ("body", map bodyOpcodeName [minBound .. maxBound])]
  mapM_
    ( \(namespace, names) -> do
        assert (namespace <> " opcode inventory is unique") (unique names)
        assert (namespace <> " opcode inventory matches protocol") (names == [name | (group, name) <- abiOpcodes, group == namespace])
    )
    opcodes
  putStrLn "compiler support integer, digest and ABI inventory checks passed"
  where
    unique xs = length xs == length (nub xs)

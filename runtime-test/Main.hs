{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import Data.ByteString qualified as B
import Monk.Runtime.Digest (sha256)
import Monk.Runtime.Fields
import Monk.Runtime.Integer
import Monk.Runtime.Pattern
import Monk.Runtime.Protocol
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if args == ["--digest"] then B.getContents >>= B.putStr . sha256 else tests

tests :: IO ()
tests = do
  let assert label condition = unless condition (fail label)
  assert "zero frames remain distinct from one empty frame" (decodeFrames "" == Right [] && decodeFrames "\0" == Right [""])
  mapM_ (\frames -> assert "empty-only frames round trip" (decodeFrames (encodeFrames frames) == Right frames)) [replicate n "" | n <- [0 .. 8]]
  assert "frames preserve empty and arbitrary bytes" (decodeFrames (encodeFrames ["", B.pack [255, 10, 1], ""]) == Right ["", B.pack [255, 10, 1], ""])
  assert "unterminated frames rejected" (either (const True) (const False) (decodeFrames "abc"))
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
  assert "IFS empty middle fields" (splitFields " :" " :a:: b: " == ["", "a", "", "b"])
  assert "quoted argv attachment" (argvFields ["p", "s", "0", "", "z"] == Right ["p", "zs"])
  assert "echo NUL and stop" (echoBytes ["-e", "a\\0b\\cignored"] == B.pack [97, 0, 98])
  assert "quoted wildcard is literal" (not (matches "a" [(False, "*")]))
  assert "wildcard matches raw bytes" (matches (B.pack [97, 255]) [(True, "a?")])
  assert "repeated stars remain bounded" (matches (B.replicate 1000 97) [(True, B.replicate 100 42 <> "a")])
  assert "SHA256 empty" (sha256 "" == "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
  assert "SHA256 abc" (sha256 "abc" == "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
  assert "SHA256 multiblock" (sha256 "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq" == "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1")
  putStrLn "native runtime pure protocol and semantic tests passed"

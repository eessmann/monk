{-# LANGUAGE OverloadedStrings #-}

module Monk.Runtime.Integer (integerOperation, integerValue, parseNumber, wrap) where

import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, ord)

wrap :: Integer -> Integer
wrap n = (n + 2 ^ (63 :: Int)) `mod` 2 ^ (64 :: Int) - 2 ^ (63 :: Int)

parseNumber :: ByteString -> Either ByteString Integer
parseNumber input | B.null input = Right 0
parseNumber input = do
  let (sign, s) = case B.uncons input of Just ('-', r) -> (-1, r); Just ('+', r) -> (1, r); _ -> (1, input)
  (base, digits) <-
    if B.elem '#' s
      then case B.break (== '#') s of
        (b, r) | not (B.null b) && B.length b <= 2 && B.head b /= '0' && B.all isDigit b -> Right (foldl' (\n c -> n * 10 + toInteger (ord c - 48)) 0 (B.unpack b), B.tail r)
        _ -> Left "invalid-number"
      else
        if B.take 2 s `elem` ["0x", "0X"]
          then Right (16, B.drop 2 s)
          else if B.length s > 1 && B.head s == '0' then Right (8, B.tail s) else Right (10, s)
  if B.null digits || base < 2 || base > 64 then Left "invalid-number" else fmap (wrap . (sign *)) (foldl' (step base) (Right 0) (B.unpack digits))
  where
    step base acc c = do
      n <- acc
      let d
            | isDigit c = ord c - 48
            | isAsciiLower c = ord c - ord 'a' + 10
            | isAsciiUpper c = ord c - ord 'A' + if base <= 36 then 10 else 36
            | c == '@' = 62
            | c == '_' = 63
            | otherwise = 65
      if toInteger d >= base then Left "invalid-number" else Right (wrap (n * base + toInteger d))

integerValue :: ByteString -> [ByteString] -> Either ByteString Integer
integerValue "batch" args = batch [] args
  where
    batch [value] [] = Right value
    batch stack ("push" : spelling : rest) = do
      value <- parseNumber spelling
      batch (value : stack) rest
    batch (value : stack) (operation : rest) | operation `elem` ["pos", "neg", "not", "invert"] = do
      result <- integerValue operation [B.pack (show value)]
      batch (result : stack) rest
    batch (right : left : stack) (operation : rest) | operation `elem` ["add", "sub", "mul", "shl", "shr", "lt", "le", "gt", "ge", "eq", "ne", "and", "xor", "or"] = do
      result <- integerValue operation [B.pack (show left), B.pack (show right)]
      batch (result : stack) rest
    batch _ _ = Left "invalid-batch"
integerValue op args = do
  values <- traverse parseNumber args
  let bool p = if p then 1 else 0
  fmap wrap $ case (op, values) of
    ("read", [a]) -> Right a
    ("pos", [a]) -> Right a
    ("neg", [a]) -> Right (-a)
    ("not", [a]) -> Right (bool (a == 0))
    ("invert", [a]) -> Right (complement a)
    ("add", [a, b]) -> Right (a + b)
    ("sub", [a, b]) -> Right (a - b)
    ("mul", [a, b]) -> Right (a * b)
    ("div", [_, 0]) -> Left "division-by-zero"
    ("rem", [_, 0]) -> Left "division-by-zero"
    ("div", [a, b]) -> Right (a `quot` b)
    ("rem", [a, b]) -> Right (a `rem` b)
    ("pow", [a, b])
      | b >= 0 -> Right (power a b 1)
      | otherwise -> Left "negative-exponent"
    ("shl", [a, b]) -> Right (shiftL a (fromInteger (b .&. 63)))
    ("shr", [a, b]) -> Right (shiftR a (fromInteger (b .&. 63)))
    ("lt", [a, b]) -> Right (bool (a < b))
    ("le", [a, b]) -> Right (bool (a <= b))
    ("gt", [a, b]) -> Right (bool (a > b))
    ("ge", [a, b]) -> Right (bool (a >= b))
    ("eq", [a, b]) -> Right (bool (a == b))
    ("ne", [a, b]) -> Right (bool (a /= b))
    ("and", [a, b]) -> Right (a .&. b)
    ("xor", [a, b]) -> Right (xor a b)
    ("or", [a, b]) -> Right (a .|. b)
    ("logical-and", [a, b]) -> Right (bool (a /= 0 && b /= 0))
    ("logical-or", [a, b]) -> Right (bool (a /= 0 || b /= 0))
    _ -> Left "unknown-primitive"
  where
    power _ 0 acc = acc
    power a n acc = power (wrap (a * a)) (n `quot` 2) (if odd n then wrap (acc * a) else acc)

integerOperation :: [ByteString] -> Either ByteString ByteString
integerOperation ("batch" : args) = case integerValue "batch" args of
  Left reason -> Left reason
  Right value -> Right ("ok\n" <> B.pack (show value) <> "\n-\n")
integerOperation (op : args)
  | op `elem` unary && length args == 1 || op `elem` binary && length args == 2 = Right $ case integerValue op args of
      Left e -> "error\n-\n" <> e <> "\n"
      Right n -> "ok\n" <> B.pack (show n) <> "\n-\n"
  where
    unary = ["read", "pos", "neg", "not", "invert"]
    binary = ["add", "sub", "mul", "div", "rem", "pow", "shl", "shr", "lt", "le", "gt", "ge", "eq", "ne", "and", "xor", "or", "logical-and", "logical-or"]
integerOperation _ = Left "invalid integer operation or arity"

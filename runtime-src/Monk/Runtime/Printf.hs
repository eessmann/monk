{-# LANGUAGE OverloadedStrings #-}

-- | The finite printf language admitted by normalization. Values are bytes;
-- no locale decoder or source-language interpreter participates in writing.
module Monk.Runtime.Printf (printfBytes) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as C
import Data.ByteString.Lazy qualified as L
import Data.Int (Int64)
import Data.Word (Word8)
import Text.Read (readMaybe)

data Piece = Bytes ByteString | StringField | DecimalField

printfBytes :: [ByteString] -> Either ByteString ByteString
printfBytes ("--" : rest) = printfBytes rest
printfBytes (format : arguments) = do
  pieces <- parseFormat (B.unpack format)
  let consumes = any (\case Bytes _ -> False; _ -> True) pieces
      render values = do
        (output, remaining) <- pass pieces values
        if consumes && not (null remaining)
          then (output <>) <$> render remaining
          else pure output
  L.toStrict . Builder.toLazyByteString <$> render arguments
printfBytes [] = Left "printf needs an admitted format"

pass :: [Piece] -> [ByteString] -> Either ByteString (Builder.Builder, [ByteString])
pass [] values = Right (mempty, values)
pass (piece : rest) values = do
  let next def = case values of [] -> (def, []); value : remaining -> (value, remaining)
  (output, remaining) <- case piece of
    Bytes bytes -> pure (Builder.byteString bytes, values)
    StringField -> let (value, tailValues) = next "" in pure (Builder.byteString value, tailValues)
    DecimalField -> do
      let (value, tailValues) = next "0"
      number <- maybe (Left "printf decimal operand is not signed-64 data") Right (readMaybe (C.unpack value) :: Maybe Int64)
      if C.pack (show number) == value
        then pure (Builder.int64Dec number, tailValues)
        else Left "printf decimal operand is not canonical"
  (suffix, tailValues) <- pass rest remaining
  pure (output <> suffix, tailValues)

parseFormat :: [Word8] -> Either ByteString [Piece]
parseFormat [] = Right []
parseFormat (37 : conversion : rest) = do
  piece <- case conversion of
    37 -> pure (Bytes "%")
    115 -> pure StringField
    100 -> pure DecimalField
    _ -> Left "unsupported printf conversion"
  (piece :) <$> parseFormat rest
parseFormat [37] = Left "unterminated printf conversion"
parseFormat (92 : escape : rest) = case lookup escape [(97, 7), (98, 8), (101, 27), (102, 12), (110, 10), (114, 13), (116, 9), (118, 11), (92, 92)] of
  Just byte -> (Bytes (B.singleton byte) :) <$> parseFormat rest
  Nothing
    | octal escape -> numeric 8 3 (escape : rest)
    | escape == 120, first : _ <- rest, hexadecimal first -> numeric 16 2 rest
    | otherwise -> Left "unsupported printf escape"
  where
    numeric base maximumCount input =
      let digits = take maximumCount (takeWhile (if base == 8 then octal else hexadecimal) input)
          value = foldl (\number digit -> number * base + fromIntegral (digitValue digit)) (0 :: Int) digits
       in (Bytes (B.singleton (fromIntegral value)) :) <$> parseFormat (drop (length digits) input)
parseFormat [92] = Left "unterminated printf escape"
parseFormat input =
  let (bytes, rest) = span (`notElem` [37, 92]) input
   in (Bytes (B.pack bytes) :) <$> parseFormat rest

octal :: Word8 -> Bool
octal byte = byte >= 48 && byte <= 55

hexadecimal :: Word8 -> Bool
hexadecimal byte = byte >= 48 && byte <= 57 || byte >= 65 && byte <= 70 || byte >= 97 && byte <= 102

digitValue :: Word8 -> Word8
digitValue byte
  | byte >= 97 = byte - 87
  | byte >= 65 = byte - 55
  | otherwise = byte - 48

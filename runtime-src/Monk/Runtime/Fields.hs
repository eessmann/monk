{-# LANGUAGE OverloadedStrings #-}

module Monk.Runtime.Fields (splitFields, argvFields, echoBytes) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.Char (digitToInt, toLower)
import Numeric (showHex)

splitFields :: ByteString -> ByteString -> [ByteString]
splitFields ifs = go [] . B.dropWhile white
  where
    white c = B.elem c ifs && c `elem` [32, 9, 10]
    go acc s =
      let (field, rest) = B.break (`B.elem` ifs) s
       in if B.null rest
            then reverse (if B.null field then acc else field : acc)
            else
              let c = B.head rest
                  afterWhite = B.dropWhile white (B.tail rest)
                  nonwhite = not (white c) || not (B.null afterWhite) && B.elem (B.head afterWhite) ifs
                  after = if white c && nonwhite then B.dropWhile white (B.tail afterWhite) else afterWhite
               in go (if not (B.null field) || nonwhite then field : acc else acc) after

argvFields :: [ByteString] -> Either ByteString [ByteString]
argvFields (prefix : suffix : force : values) | force `elem` ["0", "1"] = Right $ case values of
  [] -> [prefix <> suffix | force == "1"]
  [x] -> [prefix <> x <> suffix]
  x : xs -> (prefix <> x) : reverse (case reverse xs of y : ys -> (y <> suffix) : ys; [] -> [])
argvFields _ = Left "invalid argv frames"

echoBytes :: [ByteString] -> ByteString
echoBytes args = B.pack (render escapes body <> [10 | newline && not stopped])
  where
    (newline, escapes, values) = options True False args
    body = B.unpack (B.intercalate " " values)
    stopped = escapes && stop body
    stop (92 : 99 : _) = True
    stop (92 : _ : xs) = stop xs
    stop (_ : xs) = stop xs
    stop [] = False
    options n e (a : as) | B.length a > 1 && B.head a == 45 && B.all (`elem` [110, 101, 69]) (B.tail a) = let (n', e') = B.foldl' (\(nn, ee) c -> case c of 110 -> (False, ee); 101 -> (nn, True); _ -> (nn, False)) (n, e) (B.tail a) in options n' e' as
    options n e as = (n, e, as)
    render False xs = xs
    render True (92 : 99 : _) = []
    render True (92 : c : xs) = case lookup c [(97, 7), (98, 8), (101, 27), (69, 27), (102, 12), (110, 10), (114, 13), (116, 9), (116, 9), (118, 11), (92, 92)] of
      Just v -> v : render True xs
      Nothing ->
        let (count, base) = case c of 48 -> (3, 8); 120 -> (2, 16); 117 -> (4, 16); 85 -> (8, 16); _ -> (0, 16)
            valid w = let ch = toLower (toEnum (fromIntegral w)) in ch `elem` (if base == 8 then "01234567" else "0123456789abcdef" :: String)
            ds = take count (takeWhile valid xs)
            number = foldl (\n w -> n * base + toInteger (digitToInt (toEnum (fromIntegral w)))) 0 ds
            out
              | null ds && c /= 48 = [92, c]
              | c `elem` [117, 85] = if number < 128 then [fromInteger number] else if number > 0x7fffffff then [] else B.unpack (C.pack ("\\" <> [if number <= 65535 then 'u' else 'U'] <> replicate ((if number <= 65535 then 4 else 8) - length hex) '0' <> hex))
              | otherwise = [fromInteger (number `mod` 256)]
            hex = map (\ch -> if ch >= 'a' && ch <= 'f' then toEnum (fromEnum ch - 32) else ch) (showHex number "")
         in out <> render True (drop (length ds) xs)
    render True (x : xs) = x : render True xs
    render True [] = []

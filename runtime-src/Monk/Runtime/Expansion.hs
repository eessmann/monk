{-# LANGUAGE OverloadedStrings #-}

-- | Compose scalar expansion fragments before splitting and pathname expansion.
-- Quoted empty fragments are positions, not a property of the whole word: they
-- can preserve an empty field between two independently split expansions.
module Monk.Runtime.Expansion (expandWords) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.List (groupBy)
import Data.Word (Word8)
import Monk.Runtime.Pattern (globPaths)

data FragmentByte = QuotedEmpty | FragmentByte Bool Bool Word8

-- | Wire frames: IFS followed by mode/value pairs. q is quoted; l is an
-- unquoted source literal; e is an unquoted scalar expansion. Only e splits.
expandWords :: [ByteString] -> IO (Either ByteString [ByteString])
expandWords (ifs : frames) = case fragments frames of
  Left failure -> pure (Left failure)
  Right bytes -> Right . concat <$> mapM (globPaths . patternParts) (fields ifs bytes)
expandWords [] = pure (Left "expansion requires IFS and fragment frames")

fragments :: [ByteString] -> Either ByteString [FragmentByte]
fragments [] = Right []
fragments (mode : value : rest)
  | mode `elem` ["q", "l", "e"] =
      let bytes = if mode == "q" && B.null value then [QuotedEmpty] else map (FragmentByte (mode == "e") (mode /= "q")) (B.unpack value)
       in (bytes <>) <$> fragments rest
fragments _ = Left "invalid expansion mode or fragment count"

fields :: ByteString -> [FragmentByte] -> [[(Bool, Word8)]]
fields ifs = go [] False []
  where
    white (FragmentByte True _ c) = B.elem c ifs && c `elem` [32, 9, 10]
    white _ = False
    delimiter (FragmentByte True _ c) = B.elem c ifs
    delimiter _ = False
    finish acc forced current = if forced || not (null current) then reverse current : acc else acc
    go acc forced current [] = reverse (finish acc forced current)
    go acc _ current (QuotedEmpty : rest) = go acc True current rest
    go acc forced current input@(byte@(FragmentByte _ active c) : rest)
      | delimiter byte =
          let afterWhite = dropWhile white input
              nonwhite = case afterWhite of next : _ -> delimiter next; [] -> False
              after = case afterWhite of _ : more | nonwhite -> dropWhile white more; _ -> afterWhite
              emitted = if nonwhite then reverse current : acc else finish acc forced current
           in go emitted False [] after
      | otherwise = go acc forced ((active, c) : current) rest

patternParts :: [(Bool, Word8)] -> [(Bool, ByteString)]
patternParts = map grouped . groupBy (\a b -> fst a == fst b)
  where
    grouped part@((active, _) : _) = (active, B.pack (map snd part))
    grouped [] = (False, B.empty)

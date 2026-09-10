{-# LANGUAGE OverloadedStrings #-}

module Monk.Runtime.Pattern (patternParts, matches, globPaths, trimPattern, replaceLiteral) where

import Control.Exception (IOException, bracket, catch)
import Control.Monad (filterM)
import Data.Array (listArray, (!))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.IntSet qualified as S
import Data.List (sort)
import System.Posix.Directory.ByteString qualified as D
import System.Posix.Files.ByteString qualified as F

patternParts :: [ByteString] -> Either ByteString [(Bool, ByteString)]
patternParts [] = Right []
patternParts (active : value : rest) | active `elem` ["0", "1"] = ((active == "1", value) :) <$> patternParts rest
patternParts _ = Left "invalid pattern frames"

tokens :: [(Bool, ByteString)] -> [Int]
tokens = concatMap (\(active, bytes) -> map (\c -> if active && c == 42 then -1 else if active && c == 63 then -2 else fromIntegral c) (B.unpack bytes))

matchTokens :: [Int] -> ByteString -> Bool
matchTokens ts input = S.member size (B.foldl' step (closure (S.singleton 0)) input)
  where
    size = length ts
    arr = listArray (0, size - 1) ts
    closure states = foldl (\s i -> if S.member i s && arr ! i == -1 then S.insert (i + 1) s else s) states [0 .. size - 1]
    step states c = closure (S.fromList (concatMap (\i -> if i == size then [] else if arr ! i == -1 then [i] else [i + 1 | arr ! i == -2 || arr ! i == fromIntegral c]) (S.toList states)))

matches :: ByteString -> [(Bool, ByteString)] -> Bool
matches subject = (`matchTokens` subject) . tokens

globPaths :: [(Bool, ByteString)] -> IO [ByteString]
globPaths parts
  | all (>= 0) (tokens parts) = pure [B.concat (map snd parts)]
  | otherwise = do
      found <- walk "" (spelledComponents False (components (tokens parts)))
      pure $ if null found then [B.concat (map snd parts)] else sort found
  where
    components [] = [[]]
    components xs = let (a, b) = break (== 47) xs in a : case b of [] -> []; _ : rest -> components rest
    -- Bash retains literal separators before its first expanded component,
    -- but joins the expanded suffix with one slash, including a trailing one.
    spelledComponents _ [] = []
    spelledComponents True ([] : rest@(_ : _)) = spelledComponents True rest
    spelledComponents expanded (part : rest) = part : spelledComponents (expanded || any (< 0) part) rest
    exists path = (F.getSymbolicLinkStatus path >> pure True) `catch` (\(_ :: IOException) -> pure False)
    directory path = (F.isDirectory <$> F.getFileStatus path) `catch` (\(_ :: IOException) -> pure False)
    listing path = bracket (D.openDirStream (if B.null path then "." else path)) D.closeDirStream (\stream -> let loop = do name <- D.readDirStream stream; if B.null name then pure [] else (name :) <$> loop in loop) `catch` (\(_ :: IOException) -> pure [])
    walk parent [] = do ok <- exists parent; pure [parent | ok]
    walk parent (component : rest)
      | null component = if null rest then do ok <- directory (if B.null parent then "." else parent); pure [parent | ok] else walk (parent <> "/") rest
      | all (>= 0) component = descend (parent <> B.pack (map fromIntegral component)) rest
      | otherwise = do
          names <- listing parent
          let visible name = not (B.null name) && (B.head name /= 46 || take 1 component == [46]) && name `notElem` [".", ".."]
          chosen <- filterM (\name -> if null rest then pure True else directory (parent <> name)) (filter (\name -> visible name && matchTokens component name) names)
          concat <$> mapM (\name -> descend (parent <> name) rest) chosen
    -- Keep each spelling separator, including leading, repeated and trailing
    -- slashes. Filesystem lookup may normalize them; Bash's result words do not.
    descend path [] = walk path []
    descend path rest = walk (path <> "/") rest

-- | Byte parameter removal; the admitted pattern alphabet is literal bytes,
-- star and question mark. Prefix/suffix selection follows Bash shortest/longest.
trimPattern :: Bool -> Bool -> ByteString -> ByteString -> ByteString
trimPattern prefix longest subject patternBytes =
  case filter matching candidates of
    n : _ -> if prefix then B.drop n subject else B.take (size - n) subject
    [] -> subject
  where
    size = B.length subject
    candidates = if longest then [size, size - 1 .. 0] else [0 .. size]
    matching n = matches (if prefix then B.take n subject else B.drop (size - n) subject) [(True, patternBytes)]

-- | Literal replacement never interprets replacement ampersands or backslashes.
replaceLiteral :: Bool -> ByteString -> ByteString -> ByteString -> ByteString
replaceLiteral allMatches subject needle replacement
  | B.null needle = subject
  | otherwise = case B.breakSubstring needle subject of
      (before, after)
        | B.null after -> before
        | otherwise -> before <> replacement <> (if allMatches then replaceLiteral True rest needle replacement else rest)
        where
          rest = B.drop (B.length needle) after

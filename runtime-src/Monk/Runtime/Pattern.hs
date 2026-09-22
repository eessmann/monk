{-# LANGUAGE OverloadedStrings #-}

module Monk.Runtime.Pattern (patternParts, matches, globPaths, trimPattern, trimPatternParts, replaceLiteral) where

import Control.Exception (IOException, bracket, catch)
import Control.Monad (filterM)
import Data.Array (listArray, (!))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.IntSet qualified as S
import Data.List (sort)
import Data.Word (Word8)
import System.Posix.Directory.ByteString qualified as D
import System.Posix.Files.ByteString qualified as F

patternParts :: [ByteString] -> Either ByteString [(Bool, ByteString)]
patternParts [] = Right []
patternParts (active : value : rest) | active `elem` ["0", "1"] = ((active == "1", value) :) <$> patternParts rest
patternParts _ = Left "invalid pattern frames"

-- Backslashes produced by an unquoted expansion remain pattern syntax. Quote
-- protected metacharacters first, then tokenize, including across fragments.
data Token = Literal Word8 | Star | AnyByte | ByteClass Bool S.IntSet
  deriving (Eq)

tokens :: [(Bool, ByteString)] -> [Token]
tokens = tokenize False

tokenize :: Bool -> [(Bool, ByteString)] -> [Token]
tokenize pathname = parse . unescape . concatMap encode
  where
    encode (active, bytes) = concatMap (\c -> [92 | not active && c `elem` [33, 94, 45, 46, 58, 61, 42, 91, 93, 63, 92, 37, 35, 40, 124, 41]] <> [c]) (B.unpack bytes)
    unescape (92 : c : rest) = (False, c) : unescape rest
    unescape (c : rest) = (True, c) : unescape rest
    unescape [] = []
    parse [] = []
    parse ((True, 42) : rest) = Star : parse rest
    parse ((True, 63) : rest) = AnyByte : parse rest
    parse ((True, 91) : rest) = case bracketClass rest of
      Just (token, after)
        | not pathname || all ((/= 47) . snd) (take (length rest - length after) rest) -> token : parse after
        | otherwise -> Literal 91 : parse rest
      Nothing -> Literal 91 : parse rest
    parse ((_, c) : rest) = Literal c : parse rest

-- POSIX C-locale classes are byte classes; neither Unicode decoding nor the
-- host process locale participates in matching.
classBytes :: [Word8] -> S.IntSet
classBytes name = S.fromList [n | n <- [0 .. 255], selected n]
  where
    alpha n = n >= 65 && n <= 90 || n >= 97 && n <= 122
    digit n = n >= 48 && n <= 57
    selected n = case B.pack name of
      "alnum" -> alpha n || digit n
      "alpha" -> alpha n
      "ascii" -> n < 128
      "blank" -> n `elem` [9, 32]
      "cntrl" -> n < 32 || n == 127
      "digit" -> digit n
      "graph" -> n >= 33 && n <= 126
      "lower" -> n >= 97 && n <= 122
      "print" -> n >= 32 && n <= 126
      "punct" -> n >= 33 && n <= 126 && not (alpha n || digit n)
      "space" -> n `elem` [9, 10, 11, 12, 13, 32]
      "upper" -> n >= 65 && n <= 90
      "word" -> alpha n || digit n || n == 95
      "xdigit" -> digit n || n >= 65 && n <= 70 || n >= 97 && n <= 102
      _ -> False

bracketClass :: [(Bool, Word8)] -> Maybe (Token, [(Bool, Word8)])
bracketClass input = do
  let (negative, body) = case input of
        (True, c) : rest | c `elem` [33, 94] -> (True, rest)
        _ -> (False, input)
  (members, rest) <- collect True [] body
  let bytes = S.unions (ranges members)
  pure (if invalidRange members || S.member (-1) bytes then ByteClass False S.empty else ByteClass negative bytes, rest)
  where
    collect _ _ [] = Nothing
    collect False members ((True, 93) : rest) = Just (reverse members, rest)
    collect _ members remaining = do
      (member, rest) <- atom remaining
      collect False (member : members) rest
    atom ((True, 91) : (True, marker) : rest) | marker `elem` [58, 46, 61] =
      case special marker [] rest of
        Just (name, after) -> Just (Right (if marker == 58 then classBytes name else case name of [c] -> S.singleton (fromIntegral c); _ -> S.singleton (-1)), after)
        Nothing -> Just (Left (False, 91), (True, marker) : rest)
    atom (byte : rest) = Just (Left byte, rest)
    atom [] = Nothing
    special _ _ [] = Nothing
    special marker acc ((True, c) : (True, 93) : rest) | c == marker = Just (reverse acc, rest)
    special marker acc ((_, c) : rest) = special marker (c : acc) rest
    invalidRange (Left _ : Left (True, 45) : Right _ : _) = True
    invalidRange (_ : rest) = invalidRange rest
    invalidRange [] = False
    ranges (Left (_, low) : Left (True, 45) : Left (_, high) : rest) = S.fromList [fromIntegral low .. fromIntegral high] : ranges rest
    ranges (Left (_, c) : rest) = S.singleton (fromIntegral c) : ranges rest
    ranges (Right members : rest) = members : ranges rest
    ranges [] = []

matchTokens :: [Token] -> ByteString -> Bool
matchTokens ts input = S.member size (B.foldl' step (closure (S.singleton 0)) input)
  where
    size = length ts
    arr = listArray (0, size - 1) ts
    closure states = foldl (\s i -> if S.member i s && arr ! i == Star then S.insert (i + 1) s else s) states [0 .. size - 1]
    accepts AnyByte _ = True
    accepts (Literal a) b = a == b
    accepts (ByteClass negative members) c = S.member (fromIntegral c) members /= negative
    accepts Star _ = False
    step states c = closure (S.fromList (concatMap (\i -> if i == size then [] else if arr ! i == Star then [i] else [i + 1 | accepts (arr ! i) c]) (S.toList states)))

literal :: Token -> Bool
literal (Literal _) = True
literal _ = False

matches :: ByteString -> [(Bool, ByteString)] -> Bool
matches subject = (`matchTokens` subject) . tokens

globPaths :: [(Bool, ByteString)] -> IO [ByteString]
globPaths parts
  | all literal (tokenize True parts) = pure [B.concat (map snd parts)]
  | otherwise = do
      found <- walk "" (spelledComponents False (components (tokenize True parts)))
      pure $ if null found then [B.concat (map snd parts)] else sort found
  where
    components [] = [[]]
    components xs = let (a, b) = break (== Literal 47) xs in a : case b of [] -> []; _ : rest -> components rest
    -- Bash retains literal separators before its first expanded component,
    -- but joins the expanded suffix with one slash, including a trailing one.
    spelledComponents _ [] = []
    spelledComponents True ([] : rest@(_ : _)) = spelledComponents True rest
    spelledComponents expanded (part : rest) = part : spelledComponents (expanded || not (all literal part)) rest
    exists path = (F.getSymbolicLinkStatus path >> pure True) `catch` (\(_ :: IOException) -> pure False)
    directory path = (F.isDirectory <$> F.getFileStatus path) `catch` (\(_ :: IOException) -> pure False)
    listing path = bracket (D.openDirStream (if B.null path then "." else path)) D.closeDirStream (\stream -> let loop = do name <- D.readDirStream stream; if B.null name then pure [] else (name :) <$> loop in loop) `catch` (\(_ :: IOException) -> pure [])
    walk parent [] = do ok <- exists parent; pure [parent | ok]
    walk parent (component : rest)
      | null component = if null rest then do ok <- directory (if B.null parent then "." else parent); pure [parent | ok] else walk (parent <> "/") rest
      | all literal component = descend (parent <> B.pack [c | Literal c <- component]) rest
      | otherwise = do
          names <- listing parent
          let visible name = not (B.null name) && (B.head name /= 46 || take 1 component == [Literal 46]) && name `notElem` [".", ".."]
          chosen <- filterM (\name -> if null rest then pure True else directory (parent <> name)) (filter (\name -> visible name && matchTokens component name) names)
          concat <$> mapM (\name -> descend (parent <> name) rest) chosen
    descend path [] = walk path []
    descend path rest = walk (path <> "/") rest

-- | Byte parameter removal; the admitted pattern alphabet is literal bytes,
-- star, question mark and C-locale byte bracket expressions. Prefix/suffix selection follows Bash shortest/longest.
trimPattern :: Bool -> Bool -> ByteString -> ByteString -> ByteString
trimPattern prefix longest subject patternBytes = trimPatternParts prefix longest subject [(True, patternBytes)]

-- | Quoted fragments remain literal while adjacent active fragments retain
-- wildcard syntax, including bracket expressions crossing fragment boundaries.
trimPatternParts :: Bool -> Bool -> ByteString -> [(Bool, ByteString)] -> ByteString
trimPatternParts prefix longest subject parts =
  case filter matching candidates of
    n : _ -> if prefix then B.drop n subject else B.take (size - n) subject
    [] -> subject
  where
    size = B.length subject
    candidates = if longest then [size, size - 1 .. 0] else [0 .. size]
    compiled = tokens parts
    matching n = matchTokens compiled (if prefix then B.take n subject else B.drop (size - n) subject)

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

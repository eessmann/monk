{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 914
{-# OPTIONS_GHC -Wno-pattern-namespace-specifier #-}
#endif
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Pure literal encodings and scalar facts shared by the normalization core.
module Language.Bash.Plan.Normalize.Literal
  ( decimalIndex,
    validName,
    noPathnameExpansion,
    indexedParameter,
    ansiBytes,
    compact,
    scalarLiteral,
    numericScalar,
    numericLiteral,
  )
where

import Data.ByteString qualified as BS
import Data.Char (digitToInt, isAsciiLower, isAsciiUpper, isDigit, isHexDigit, isOctDigit, toUpper)
import Data.List (lookup)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Language.Bash.Arithmetic.Plan qualified as A
import Language.Bash.Plan qualified as P
import Numeric (showHex)
import ShellCheck.AST (Id (..), pattern T_Literal)

decimalIndex :: Text -> Maybe Int
decimalIndex value
  | not (T.null value),
    T.all isDigit value,
    value == "0" || not (T.isPrefixOf "0" value) = do
      integer <- readMaybe (toString value) :: Maybe Integer
      guard (integer <= toInteger (maxBound :: Int))
      pure (fromInteger integer)
  | otherwise = Nothing

asciiLetter :: Char -> Bool
asciiLetter character = isAsciiLower character || isAsciiUpper character

validName :: Text -> Bool
validName name = case T.uncons name of
  Just (leading, rest) -> (asciiLetter leading || leading == '_') && T.all (\c -> asciiLetter c || isDigit c || c == '_') rest
  Nothing -> False

noPathnameExpansion :: M.Map Text Text -> P.Scalar -> Bool
noPathnameExpansion constants = \case
  P.Literal value -> safe value
  P.Variable name -> maybe False safe (M.lookup name constants)
  P.LastStatus -> True
  P.LastBackgroundPid -> True
  P.ArgumentCount -> True
  P.ArithmeticValue {} -> True
  P.Concat values -> all (noPathnameExpansion constants) values
  _ -> False
  where
    safe = not . T.any (`elem` ("*?[" :: String))

indexedParameter :: Text -> Maybe (Text, Int)
indexedParameter value = do
  let (name, rest) = T.breakOn "[" value
  guard (validName name)
  digits <- T.stripPrefix "[" rest >>= T.stripSuffix "]"
  (name,) <$> decimalIndex digits

-- Bash ANSI quotes are byte strings in the admitted C locale. NUL ends the
-- scalar; non-ASCII Unicode escapes remain their canonical textual spelling.
-- Pinned Nix Bash builds differ above the signed Unicode range. Retain both
-- byte results so generated output selects by runtime target, not build host.
ansiBytes :: Bool -> String -> Either Text ByteString
ansiBytes darwin input = BS.takeWhile (/= 0) . BS.concat <$> go input
  where
    go :: String -> Either Text [ByteString]
    go [] = pure []
    go ('\\' : code : rest)
      | Just byte <- lookup code [('?', 63), ('a', 7), ('b', 8), ('e', 27), ('E', 27), ('f', 12), ('n', 10), ('r', 13), ('t', 9), ('v', 11), ('\\', 92), ('\'', 39), ('"', 34)] = (BS.singleton byte :) <$> go rest
      | isOctDigit code = number 8 3 (code : rest) ""
      | code == 'x' = number 16 2 rest "\\x"
      | code `elem` ['u', 'U'] = do
          let (digits, remaining) = takeDigits isHexDigit (if code == 'u' then 4 else 8) rest
              value = foldl' (\n d -> n * 16 + toInteger (digitToInt d)) 0 digits
              width = if value <= 65535 then 4 else 8
              hex = map toUpper (showHex value "")
              rendered
                | value > 2147483647 && not darwin = BS.empty
                | value < 128 = BS.singleton (fromInteger value)
                | otherwise = encodeUtf8 (toText ((if width == 4 then "\\u" else "\\U") <> replicate (max 0 (width - length hex)) '0' <> hex))
          if null digits then (encodeUtf8 (toText ['\\', code]) :) <$> go rest else (rendered :) <$> go remaining
      | code == 'c' = case rest of
          '\\' : '\\' : remaining -> (BS.singleton 28 :) <$> go remaining
          character : remaining | ord character < 128 -> (BS.singleton (fromIntegral (if character == '?' then 127 else ord (toUpper character) `mod` 32)) :) <$> go remaining
          _ -> Left "ANSI control escape requires an ASCII operand"
      | otherwise = (encodeUtf8 (toText ['\\', code]) :) <$> go rest
    go (character : rest) = (encodeUtf8 (toText [character]) :) <$> go rest
    number :: Int -> Int -> String -> Text -> Either Text [ByteString]
    number base count sourceDigits fallback = do
      let (digits, remaining) = takeDigits (if base == 8 then isOctDigit else isHexDigit) count sourceDigits
          value = foldl' (\n d -> n * base + digitToInt d) 0 digits
          bytes = if null digits then encodeUtf8 fallback else BS.singleton (fromIntegral (value `mod` 256))
      (bytes :) <$> go remaining
    takeDigits predicate count value = let digits = take count (takeWhile predicate value) in (digits, drop (length digits) value)

compact :: [P.Scalar] -> P.Scalar
compact [] = P.Literal ""
compact [value] = value
compact values = maybe (P.Concat values) (P.Literal . mconcat) (traverse scalarLiteral values)

scalarLiteral :: P.Scalar -> Maybe Text
scalarLiteral (P.Literal value) = Just value
scalarLiteral _ = Nothing

numericScalar :: P.Scalar -> Bool
numericScalar (P.ArithmeticValue {}) = True
numericScalar P.LastBackgroundPid = True
numericScalar value = maybe False numericLiteral (scalarLiteral value)

numericLiteral :: Text -> Bool
numericLiteral value = T.null value || isRight (A.normalizeArithmetic (T_Literal (Id 0) (toString value)))

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Variables.Glob
  ( wordIsGlob,
    wordNeedsExtglobShim,
    renderGlobWord,
    renderGlobWordRaw,
    extglobShimListExpr,
    parseGlobPattern,
    renderExtglobForFish,
    renderExtglobRaw,
    patternExprFromToken,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Language.Fish.AST
import Language.Fish.Translator.Token (tokenRawText, tokenToLiteralText)
import ShellCheck.AST

wordIsGlob :: [Token] -> Bool
wordIsGlob parts =
  all isGlobToken parts && any hasGlobMeta parts
  where
    isGlobToken = \case
      T_Literal {} -> True
      T_Glob {} -> True
      T_Extglob {} -> True
      _ -> False

    hasGlobMeta = \case
      T_Literal _ s -> hasGlobChars (toText s)
      T_Glob {} -> True
      T_Extglob {} -> True
      _ -> False

    hasGlobChars = T.any (`elem` ("*?[]{}" :: String))

wordNeedsExtglobShim :: [Token] -> Bool
wordNeedsExtglobShim =
  any
    ( \case
        T_Extglob _ op _ -> extglobNeedsShim op
        _ -> False
    )

extglobNeedsShim :: String -> Bool
extglobNeedsShim op = op /= "@"

renderGlobWord :: [Token] -> Text
renderGlobWord =
  foldMap $ \case
    T_Literal _ s -> toText s
    T_Glob _ s -> toText s
    T_Extglob _ op parts -> fromMaybe (renderExtglobRaw op parts) (renderExtglobForFish op parts)
    other -> tokenToLiteralText other

renderGlobWordRaw :: [Token] -> Text
renderGlobWordRaw =
  foldMap $ \case
    T_Literal _ s -> toText s
    T_Glob _ s -> toText s
    T_Extglob _ op parts -> renderExtglobRaw op parts
    other -> tokenToLiteralText other

extglobShimListExpr :: Text -> FishExpr (TList TStr)
extglobShimListExpr pat =
  ExprCommandSubst
    ( Stmt
        ( Command
            "bash"
            [ ExprVal (ExprLiteral "-O"),
              ExprVal (ExprLiteral "extglob"),
              ExprVal (ExprLiteral "-c"),
              ExprVal (ExprLiteral (extglobShimScript pat))
            ]
        )
        NE.:| []
    )

extglobShimScript :: Text -> Text
extglobShimScript pat =
  "shopt -s extglob; shopt -u nullglob dotglob failglob; printf '%s\\n' " <> pat

parseGlobPattern :: Text -> GlobPattern
parseGlobPattern txt = GlobPattern (finishLiteral (go txt) [])
  where
    go t =
      case T.uncons t of
        Nothing -> []
        Just ('*', rest) ->
          case T.uncons rest of
            Just ('*', r) -> GlobStarStar : go r
            _ -> GlobStar : go rest
        Just ('?', rest) -> GlobQuestion : go rest
        Just ('[', rest) ->
          let (cls, r) = T.breakOn "]" rest
           in case T.uncons r of
                Just (_, r') -> GlobCharClass cls : go r'
                Nothing -> GlobLiteral ("[" <> cls) : go r
        Just ('{', rest) ->
          let (inner, r) = T.breakOn "}" rest
              alts = filter (not . T.null) (T.splitOn "," inner)
           in case (alts, T.uncons r) of
                (a : as, Just (_, r')) -> GlobBraces (a NE.:| as) : go r'
                _ -> GlobLiteral ("{" <> inner) : go r
        Just (c, rest) ->
          let (lit, r) = spanLiteral rest (T.singleton c)
           in GlobLiteral lit : go r

    spanLiteral t acc =
      case T.uncons t of
        Just (c, rest)
          | c `elem` ("*?{[" :: String) -> (acc, t)
          | otherwise -> spanLiteral rest (acc <> T.singleton c)
        Nothing -> (acc, T.empty)

    finishLiteral parts acc =
      case parts of
        [] -> reverse acc
        (GlobLiteral t1 : GlobLiteral t2 : xs) -> finishLiteral (GlobLiteral (t1 <> t2) : xs) acc
        (x : xs) -> finishLiteral xs (x : acc)

renderExtglobForFish :: String -> [Token] -> Maybe Text
renderExtglobForFish op parts =
  case op of
    "@" -> Just ("{" <> T.intercalate "," (extglobAlternatives parts) <> "}")
    _ -> Nothing

renderExtglobRaw :: String -> [Token] -> Text
renderExtglobRaw op parts =
  toText op <> "(" <> T.intercalate "|" (extglobAlternatives parts) <> ")"

extglobAlternatives :: [Token] -> [Text]
extglobAlternatives parts =
  let splitAlts = splitExtglobParts parts
      alts =
        if length splitAlts == 1 && length parts > 1
          then map tokenToLiteralText parts
          else map (T.concat . map tokenToLiteralText) splitAlts
   in filter (not . T.null) alts

splitExtglobParts :: [Token] -> [[Token]]
splitExtglobParts toks = go toks [] []
  where
    go [] current acc =
      reverse (reverse current : acc)
    go (t : ts) current acc
      | isSeparator t = go ts [] (reverse current : acc)
      | otherwise = go ts (t : current) acc

    isSeparator tok =
      case tok of
        T_Pipe {} -> True
        T_ParamSubSpecialChar _ "|" -> True
        _ -> tokenToLiteralText tok == "|"

patternExprFromToken :: Token -> FishExpr TStr
patternExprFromToken tok =
  ExprLiteral (patternTextFromToken tok)

patternTextFromToken :: Token -> Text
patternTextFromToken = \case
  T_NormalWord _ parts -> renderGlobWordRaw parts
  T_Glob _ s -> toText s
  T_Literal _ s -> toText s
  T_Extglob _ op parts -> renderExtglobRaw op parts
  T_SingleQuoted _ s -> toText s
  T_DoubleQuoted _ parts -> T.concat (map tokenToLiteralText parts)
  other ->
    let txt = tokenRawText other
     in if T.null txt then tokenToLiteralText other else txt

{-# LANGUAGE LambdaCase #-}

module Language.Fish.Translator.Commands.Tests
  ( isSingleBracketTest,
    isSingleBracketTokens,
    isDoubleBracketTest,
    isDoubleBracketTokens,
    translateDoubleBracketArgs,
    translateDoubleBracketArgsM,
    translateConditionToken,
    translateConditionTokenM,
    translateBinaryCondition,
    translateBinaryConditionExprs,
    translateRegexTokenToExpr,
    translateRegexTokenToExprM,
    normalizeTestExprs,
  )
where

import Data.List.NonEmpty qualified as NE
import Language.Fish.AST
import Language.Fish.Translator.Cond
  ( condBinaryCommand,
    condFromTokenMWith,
    condFromTokenWith,
    condToCommand,
  )
import Language.Fish.Translator.Hoist (Hoisted (..))
import Language.Fish.Translator.Hoist.Monad (HoistedM, hoistM)
import Language.Fish.Translator.Token (tokenHasExpansion, tokenToLiteralText)
import Language.Fish.Translator.Variables
  ( translateTokenToExpr,
    translateTokenToExprM,
    translateTokenToExprOrRedirect,
  )
import ShellCheck.AST

isSingleBracketTest :: Token -> [Token] -> Maybe (FishCommand TStatus)
isSingleBracketTest bracketToken args =
  let bracketTxt = tokenToLiteralText bracketToken
   in if bracketTxt == "["
        then case NE.nonEmpty args of
          Just neArgs ->
            let lastToken = NE.last neArgs
                lastText = tokenToLiteralText lastToken
                middle = NE.init neArgs
             in if lastText == "]"
                  then Just (Command "test" (normalizeTestExprs (map translateTokenToExprOrRedirect middle)))
                  else Nothing
          Nothing -> Nothing
        else Nothing

normalizeTestExprs :: [ExprOrRedirect] -> [ExprOrRedirect]
normalizeTestExprs = map normalize
  where
    normalize expr =
      case expr of
        ExprVal (ExprLiteral "==") -> ExprVal (ExprLiteral "=")
        ExprVal (ExprListLiteral [ExprLiteral "=="]) -> ExprVal (ExprListLiteral [ExprLiteral "="])
        _ -> expr

isSingleBracketTokens :: Token -> [Token] -> Maybe [Token]
isSingleBracketTokens bracketToken args =
  let bracketTxt = tokenToLiteralText bracketToken
   in if bracketTxt == "["
        then case NE.nonEmpty args of
          Just neArgs ->
            let lastToken = NE.last neArgs
                lastText = tokenToLiteralText lastToken
                middle = NE.init neArgs
             in if lastText == "]" then Just middle else Nothing
          Nothing -> Nothing
        else Nothing

isDoubleBracketTest :: Token -> [Token] -> Maybe (FishCommand TStatus)
isDoubleBracketTest bracketToken args =
  let bracketTxt = tokenToLiteralText bracketToken
   in if bracketTxt == "[["
        then case NE.nonEmpty args of
          Just neArgs ->
            let lastToken = NE.last neArgs
                lastText = tokenToLiteralText lastToken
                middle = NE.init neArgs
             in if lastText == "]]"
                  then translateDoubleBracketArgs middle
                  else Nothing
          Nothing -> Nothing
        else Nothing

isDoubleBracketTokens :: Token -> [Token] -> Maybe [Token]
isDoubleBracketTokens bracketToken args =
  let bracketTxt = tokenToLiteralText bracketToken
   in if bracketTxt == "[["
        then case NE.nonEmpty args of
          Just neArgs ->
            let lastToken = NE.last neArgs
                lastText = tokenToLiteralText lastToken
                middle = NE.init neArgs
             in if lastText == "]]" then Just middle else Nothing
          Nothing -> Nothing
        else Nothing

translateDoubleBracketArgs :: [Token] -> Maybe (FishCommand TStatus)
translateDoubleBracketArgs toks =
  case toks of
    [lhs, opTok, rhs] ->
      Just (translateBinaryCondition (tokenToLiteralText opTok) lhs rhs)
    _ -> Nothing

translateDoubleBracketArgsM :: [Token] -> HoistedM (FishCommand TStatus)
translateDoubleBracketArgsM =
  translateDoubleBracketArgsHoistedM

translateDoubleBracketArgsHoistedM :: [Token] -> HoistedM (FishCommand TStatus)
translateDoubleBracketArgsHoistedM toks =
  case toks of
    [lhs, opTok, rhs] -> do
      let op = tokenToLiteralText opTok
      Hoisted preL lhsExpr <- translateTokenToExprM lhs
      Hoisted preR rhsExpr <-
        if op == "=~"
          then translateRegexTokenToExprM rhs
          else translateTokenToExprM rhs
      hoistM (preL <> preR) (condBinaryCommand op lhsExpr rhsExpr)
    _ -> hoistM [] (Command "true" [])

translateConditionTokenM :: Token -> HoistedM (FishCommand TStatus)
translateConditionTokenM tok =
  fmap (fmap condToCommand) (condFromTokenMWith translateTokenToExprM translateRegexTokenToExprM literalExpr tok)
  where
    literalExpr = ExprLiteral . tokenToLiteralText

translateConditionToken :: Token -> FishCommand TStatus
translateConditionToken tok =
  condToCommand (condFromTokenWith translateTokenToExpr regexExpr literalExpr tok)
  where
    regexExpr t =
      if tokenHasExpansion t
        then translateTokenToExpr t
        else ExprLiteral (tokenToLiteralText t)
    literalExpr = ExprLiteral . tokenToLiteralText

translateBinaryCondition :: Text -> Token -> Token -> FishCommand TStatus
translateBinaryCondition op lhs rhs =
  case op of
    "=~" ->
      let rhsExpr = translateRegexTokenToExpr rhs
       in translateBinaryConditionExprs op (translateTokenToExpr lhs) rhsExpr
    _ ->
      translateBinaryConditionExprs op (translateTokenToExpr lhs) (translateTokenToExpr rhs)

translateBinaryConditionExprs :: Text -> FishExpr TStr -> FishExpr TStr -> FishCommand TStatus
translateBinaryConditionExprs = condBinaryCommand

translateRegexTokenToExpr :: Token -> FishExpr TStr
translateRegexTokenToExpr tok
  | tokenHasExpansion tok = translateTokenToExpr tok
  | otherwise = ExprLiteral (tokenToLiteralText tok)

translateRegexTokenToExprM :: Token -> HoistedM (FishExpr TStr)
translateRegexTokenToExprM =
  translateRegexTokenToExprHoistedM

translateRegexTokenToExprHoistedM :: Token -> HoistedM (FishExpr TStr)
translateRegexTokenToExprHoistedM tok
  | tokenHasExpansion tok = translateTokenToExprM tok
  | otherwise = hoistM [] (ExprLiteral (tokenToLiteralText tok))

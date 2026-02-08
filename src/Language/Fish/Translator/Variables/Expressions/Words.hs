{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Variables.Expressions.Words
  ( isNoSplitParamExpansion,
    isSimpleListExpansion,
    wordNeedsSplit,
    translateDoubleQuotedExprWith,
    translateDoubleQuotedExprMWith,
    translateWordPartsToExprWith,
    translateWordPartsToExprMWith,
    unwrapBraced,
  )
where

import Language.Fish.AST
import Language.Fish.Translator.Hoist (Hoisted (..))
import Language.Fish.Translator.Hoist.Monad (HoistedM, hoistM)
import Language.Fish.Translator.Token (wordHasExpansion)
import Language.Fish.Translator.Variables.ParamExpansion (noSplitParamExpansion)
import ShellCheck.AST

isNoSplitParamExpansion :: Token -> Bool
isNoSplitParamExpansion = noSplitParamExpansion

isSimpleListExpansion :: [Token] -> Bool
isSimpleListExpansion parts =
  case parts of
    [T_DollarBraced _ _ word] -> isNoSplitParamExpansion word
    _ -> False

wordNeedsSplit :: [Token] -> Bool
wordNeedsSplit parts =
  wordHasExpansion parts && not (isSimpleListExpansion parts)

translateDoubleQuotedExprWith :: (Token -> FishExpr TStr) -> [Token] -> FishExpr TStr
translateDoubleQuotedExprWith translateToken parts =
  case map translateToken parts of
    [] -> ExprLiteral ""
    (x : xs) -> foldl' ExprStringConcat x xs

translateDoubleQuotedExprMWith ::
  (Token -> HoistedM (FishExpr TStr)) ->
  [Token] ->
  HoistedM (FishExpr TStr)
translateDoubleQuotedExprMWith translateToken =
  translateDoubleQuotedExprHoistedWith translateToken

translateDoubleQuotedExprHoistedWith ::
  (Token -> HoistedM (FishExpr TStr)) ->
  [Token] ->
  HoistedM (FishExpr TStr)
translateDoubleQuotedExprHoistedWith translateToken parts =
  case parts of
    [] -> hoistM [] (ExprLiteral "")
    _ -> do
      translated <- mapM translateToken parts
      let Hoisted pre exprs = sequenceA translated
      case exprs of
        [] -> hoistM pre (ExprLiteral "")
        (x : xs) -> hoistM pre (foldl' ExprStringConcat x xs)

translateWordPartsToExprWith :: (Token -> FishExpr TStr) -> [Token] -> FishExpr TStr
translateWordPartsToExprWith = translateDoubleQuotedExprWith

translateWordPartsToExprMWith ::
  (Token -> HoistedM (FishExpr TStr)) ->
  [Token] ->
  HoistedM (FishExpr TStr)
translateWordPartsToExprMWith = translateDoubleQuotedExprMWith

unwrapBraced :: Token -> Token
unwrapBraced = \case
  T_DollarBraced _ _ inner -> inner
  other -> other

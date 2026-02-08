{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Variables.Index
  ( parseArithExprAdjusted,
    parseArithExpr,
    parseIndexSpec,
    indexedVarText,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Language.Fish.AST
import Language.Fish.Translator.Token (tokenToLiteralText)
import Language.Fish.Translator.Variables.Arithmetic (arithArgsFromText, arithArgsFromToken)
import ShellCheck.AST

parseArithExprAdjusted :: Text -> FishExpr TInt
parseArithExprAdjusted txt =
  fromMaybe (ExprNumLiteral 1) (parseArithExprWith True txt)

parseArithExpr :: Text -> Maybe (FishExpr TInt)
parseArithExpr = parseArithExprWith False

parseArithExprWith :: Bool -> Text -> Maybe (FishExpr TInt)
parseArithExprWith adjust txt =
  let trimmed = T.strip txt
   in if T.null trimmed
        then Nothing
        else case parseInt trimmed of
          Just n ->
            let adjusted = if adjust then adjustIndex n else n
             in Just (ExprNumLiteral adjusted)
          Nothing -> do
            args <- arithArgsFromText trimmed
            let base = ExprMath args
            pure (if adjust then adjustIndexExpr base else base)

adjustIndexExpr :: FishExpr TInt -> FishExpr TInt
adjustIndexExpr expr =
  case expr of
    ExprNumLiteral n -> ExprNumLiteral (adjustIndex n)
    ExprMath args -> ExprMath (appendMathArgs args)
    _ -> expr

appendMathArgs :: NonEmpty (FishExpr TStr) -> NonEmpty (FishExpr TStr)
appendMathArgs args = args <> (ExprLiteral "+" NE.:| [ExprLiteral "1"])

parseIndexSpec :: Text -> Maybe (FishIndex TStr (TList TStr))
parseIndexSpec rawTxt =
  let txt = T.strip rawTxt
   in if T.null txt || txt == "@" || txt == "*"
        then Nothing
        else
          if T.isInfixOf ".." txt
            then parseIndexRange txt
            else
              if T.isInfixOf "," txt
                then parseIndexList txt
                else parseIndexSingle txt
  where
    parseIndexSingle t = do
      n <- parseIndexExpr t
      pure (IndexList (n NE.:| []))

    parseIndexList t = do
      parts <- NE.nonEmpty (map T.strip (T.splitOn "," t))
      exprs <- traverse parseIndexExpr parts
      pure (IndexList exprs)

    parseIndexRange t =
      case T.splitOn ".." t of
        [startTxt, endTxt] ->
          let startIx = parseIndexPart startTxt
              endIx = parseIndexPart endTxt
           in if isNothing startIx && isNothing endIx
                then Nothing
                else Just (IndexRange startIx endIx)
        _ -> Nothing

    parseIndexPart t =
      if T.null (T.strip t)
        then Nothing
        else parseIndexExpr t

parseIndexExpr :: Text -> Maybe (FishExpr TInt)
parseIndexExpr = parseArithExprWith True

indexedVarText :: Text -> [Token] -> Maybe Text
indexedVarText name indices =
  case mapMaybe indexTokenText indices of
    [] -> Nothing
    parts -> Just (name <> foldMap (\t -> "[" <> t <> "]") parts)

indexTokenText :: Token -> Maybe Text
indexTokenText tok =
  case tok of
    T_UnparsedIndex _ _ raw ->
      Just (indexTextFromRaw (toText raw))
    TA_Sequence {} -> Just (indexTextFromArithToken tok)
    TA_Expansion {} -> Just (indexTextFromArithToken tok)
    TA_Parenthesis {} -> Just (indexTextFromArithToken tok)
    TA_Unary {} -> Just (indexTextFromArithToken tok)
    TA_Binary {} -> Just (indexTextFromArithToken tok)
    TA_Trinary {} -> Just (indexTextFromArithToken tok)
    TA_Assignment {} -> Just (indexTextFromArithToken tok)
    TA_Variable {} -> Just (indexTextFromArithToken tok)
    T_Literal _ s ->
      Just (adjustIndexText (toText s))
    _ ->
      let txt = tokenToLiteralText tok
       in if T.null txt then Nothing else Just (adjustIndexText txt)

indexTextFromArithToken :: Token -> Text
indexTextFromArithToken tok =
  let args = arithArgsFromToken tok
   in case NE.toList args of
        [ExprLiteral txt]
          | Just n <- parseInt txt -> show (adjustIndex n)
        _ ->
          mathExprText (appendMathArgs args)

indexTextFromRaw :: Text -> Text
indexTextFromRaw raw =
  case parseInt raw of
    Just n -> show (adjustIndex n)
    Nothing ->
      case arithArgsFromText raw of
        Just args -> mathExprText (appendMathArgs args)
        Nothing -> raw

mathExprText :: NonEmpty (FishExpr TStr) -> Text
mathExprText args =
  "(math " <> renderMathArgs args <> ")"

renderMathArgs :: NonEmpty (FishExpr TStr) -> Text
renderMathArgs args =
  T.intercalate " " (map renderMathArg (NE.toList args))

renderMathArg :: FishExpr TStr -> Text
renderMathArg = \case
  ExprLiteral txt -> txt
  ExprVariable (VarScalar name) -> "$" <> name
  _ -> "<expr>"

adjustIndexText :: Text -> Text
adjustIndexText txt =
  case parseInt txt of
    Just n -> show (adjustIndex n)
    Nothing -> txt

adjustIndex :: Int -> Int
adjustIndex n
  | n >= 0 = n + 1
  | otherwise = n

parseInt :: Text -> Maybe Int
parseInt t = readMaybe (toString (T.strip t))

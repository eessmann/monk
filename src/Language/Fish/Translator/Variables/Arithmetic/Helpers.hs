{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Variables.Arithmetic.Helpers
  ( UnaryFixity (..),
    stripUnaryOp,
    stripUnaryOpText,
    ensureArithArgs,
    arithCompareOp,
    stripArithWrappers,
    arithHasSideEffects,
    arithTempName,
    arithTempNameWith,
    arithVarName,
    assignmentArgs,
    mathSubstFromArgs,
    mathSubstFromArgsNoScale,
    wrapParens,
    arithArgsFromText,
  )
where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Language.Fish.AST
import ShellCheck.AST

arithCompareOp :: Text -> Maybe Text
arithCompareOp op =
  case op of
    "==" -> Just "-eq"
    "!=" -> Just "-ne"
    "<" -> Just "-lt"
    "<=" -> Just "-le"
    ">" -> Just "-gt"
    ">=" -> Just "-ge"
    _ -> Nothing

stripArithWrappers :: Token -> Token
stripArithWrappers = \case
  TA_Parenthesis _ inner -> stripArithWrappers inner
  TA_Expansion _ (inner : _) -> stripArithWrappers inner
  TA_Sequence _ [inner] -> stripArithWrappers inner
  other -> other

data UnaryFixity
  = UnaryPrefix
  | UnaryPostfix
  | UnaryBare
  deriving stock (Eq, Show)

stripUnaryOp :: String -> (Text, UnaryFixity)
stripUnaryOp op =
  let raw = toText op
      (postfix, rest) =
        case T.uncons raw of
          Just ('|', r) -> (True, r)
          _ -> (False, raw)
      (prefix, core) =
        case T.unsnoc rest of
          Just (r, '|') -> (True, r)
          _ -> (False, rest)
      fixity
        | postfix = UnaryPostfix
        | prefix = UnaryPrefix
        | otherwise = UnaryBare
   in (core, fixity)

stripUnaryOpText :: String -> Text
stripUnaryOpText = fst . stripUnaryOp

ensureArithArgs :: [FishExpr TStr] -> [FishExpr TStr]
ensureArithArgs [] = [ExprLiteral "0"]
ensureArithArgs xs = xs

arithHasSideEffects :: Token -> Bool
arithHasSideEffects = \case
  TA_Assignment {} -> True
  TA_Unary _ op _ ->
    let (opTxt, _) = stripUnaryOp op
     in opTxt == "++" || opTxt == "--"
  TA_Sequence _ ts -> any arithHasSideEffects ts
  TA_Expansion _ ts -> any arithHasSideEffects ts
  TA_Parenthesis _ t -> arithHasSideEffects t
  TA_Binary _ _ l r -> arithHasSideEffects l || arithHasSideEffects r
  TA_Trinary _ a b c -> arithHasSideEffects a || arithHasSideEffects b || arithHasSideEffects c
  _ -> False

arithTempName :: Token -> Text
arithTempName tok =
  let raw = toText (show (getId tok) :: String)
      digits = T.filter isDigit raw
      suffix =
        if T.null digits
          then raw
          else digits
   in "__monk_arith_tmp_" <> if T.null suffix then "0" else suffix

arithTempNameWith :: Token -> Text -> Text
arithTempNameWith tok suffix =
  arithTempName tok <> "_" <> suffix

arithVarName :: Token -> Maybe Text
arithVarName = \case
  TA_Variable _ name _ -> Just (toText name)
  TA_Expansion _ inner -> listToMaybe inner >>= arithVarName
  _ -> Nothing

assignmentArgs :: Text -> Text -> NonEmpty (FishExpr TStr) -> NonEmpty (FishExpr TStr)
assignmentArgs opTxt var rhsArgs
  | opTxt == "=" = rhsArgs
  | T.isSuffixOf "=" opTxt =
      let op = T.dropEnd 1 opTxt
       in ExprVariable (VarScalar var) NE.:| (ExprLiteral op : NE.toList rhsArgs)
  | otherwise = ExprVariable (VarScalar var) NE.:| (ExprLiteral opTxt : NE.toList rhsArgs)

mathSubstFromArgs :: NonEmpty (FishExpr TStr) -> FishExpr (TList TStr)
mathSubstFromArgs args =
  let scaleArgs = [ExprVal (ExprLiteral "--scale"), ExprVal (ExprLiteral "0")]
   in ExprCommandSubst (Stmt (Command "math" (scaleArgs <> map ExprVal (NE.toList args))) NE.:| [])

mathSubstFromArgsNoScale :: NonEmpty (FishExpr TStr) -> FishExpr (TList TStr)
mathSubstFromArgsNoScale args =
  ExprCommandSubst (Stmt (Command "math" (map ExprVal (NE.toList args))) NE.:| [])

wrapParens :: [FishExpr TStr] -> [FishExpr TStr]
wrapParens exprs = [ExprLiteral "("] <> exprs <> [ExprLiteral ")"]

arithArgsFromText :: Text -> Maybe (NonEmpty (FishExpr TStr))
arithArgsFromText txt =
  NE.nonEmpty (map arithTokenToExpr (tokenizeArithText txt))

arithTokenToExpr :: Text -> FishExpr TStr
arithTokenToExpr tok
  | Just name <- stripDollarVar tok = ExprVariable (VarScalar name)
  | isVarToken tok = ExprVariable (VarScalar tok)
  | otherwise = ExprLiteral tok

stripDollarVar :: Text -> Maybe Text
stripDollarVar tok =
  case T.uncons tok of
    Just ('$', rest) | isVarToken rest -> Just rest
    _ -> Nothing

isVarToken :: Text -> Bool
isVarToken tok =
  case T.uncons tok of
    Just (c, _) | isAlpha c || c == '_' -> T.all isIdentChar tok
    _ -> False
  where
    isIdentChar ch = isAlphaNum ch || ch == '_'

tokenizeArithText :: Text -> [Text]
tokenizeArithText txt = go (T.unpack txt) []
  where
    go [] acc = reverse acc
    go (c : cs) acc
      | isSpace c = go cs acc
      | c == '$' =
          let (name, rest) = span isIdentChar cs
              tok = if null name then "$" else '$' : name
           in go rest (toText tok : acc)
      | isAlpha c || c == '_' =
          let (ident, rest) = span isIdentChar cs
           in go rest (toText (c : ident) : acc)
      | isDigit c =
          let (digits, rest) = span isDigit cs
           in go rest (toText (c : digits) : acc)
      | otherwise =
          case cs of
            (d : ds)
              | isTwoCharOp c d ->
                  go ds (toText [c, d] : acc)
              | otherwise ->
                  go cs (toText [c] : acc)
            [] -> go cs (toText [c] : acc)

    isIdentChar ch = isAlphaNum ch || ch == '_'
    isTwoCharOp a b =
      [a, b]
        `elem` [ "++",
                 "--",
                 "+=",
                 "-=",
                 "*=",
                 "/=",
                 "%=",
                 "<<",
                 ">>",
                 "<=",
                 ">=",
                 "==",
                 "!=",
                 "&&",
                 "||"
               ]

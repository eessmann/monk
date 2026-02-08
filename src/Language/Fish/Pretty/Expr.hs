{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Fish.Pretty.Expr
  ( prettyFishExprWith,
    escapeFishString,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Language.Fish.AST
import Prettyprinter

prettyFishExprWith :: forall ann t. (FishStatement -> Doc ann) -> FishExpr t -> Doc ann
prettyFishExprWith prettyStmt = go
  where
    go :: forall t1. FishExpr t1 -> Doc ann
    go = \case
      ExprLiteral txt -> escapeFishString txt
      ExprNumLiteral i -> pretty i
      ExprVariable varRef -> prettyVarRef varRef
      ExprSpecialVar sv -> prettySpecialVar sv
      ExprStringConcat e1 e2 -> go e1 <> go e2
      ExprStringOp op e -> parens (prettyStringOp op <+> go e)
      ExprJoinList e ->
        parens
          ( "string join"
              <+> escapeFishString " "
              <+> go e
              <+> ";"
              <+> "or"
              <+> "printf"
              <+> escapeFishString ""
          )
      ExprMath args ->
        parens ("math" <+> "--scale" <+> "0" <+> hsep (map go (NE.toList args)))
      ExprCommandSubst stmts ->
        "(" <> align (vsep (map prettyStmt (NE.toList stmts))) <> ")"
      ExprListLiteral [] -> mempty
      ExprListLiteral xs -> hsep (map go xs)
      ExprListConcat a b -> go a <+> go b
      ExprGlob g -> prettyGlob g
      ExprProcessSubst stmts -> prettyProcessSubst stmts

    prettyVarRef :: forall t1. FishVarRef t1 -> Doc ann
    prettyVarRef = \case
      VarAll name
        | name == "#" -> "(count $argv)"
        | otherwise -> "$" <> pretty name
      VarScalar name
        | name == "#" -> "(count $argv)"
        | otherwise -> "$" <> pretty name
      VarIndex name idx -> "$" <> pretty name <> brackets (prettyFishIndex idx)

    prettyFishIndex :: forall a b. FishIndex a b -> Doc ann
    prettyFishIndex = \case
      IndexSingle e -> go e
      IndexRange e1 e2 ->
        let start = maybe mempty go e1
            end = maybe mempty go e2
         in start <> ".." <> end
      IndexList xs -> hsep (map go (NE.toList xs))

    prettyGlob (GlobPattern parts) = hcat (map prettyGlobPart parts)

    prettyGlobPart = \case
      GlobLiteral txt -> pretty txt
      GlobStar -> "*"
      GlobStarStar -> "**"
      GlobQuestion -> "?"
      GlobCharClass pat -> brackets (pretty pat)
      GlobBraces xs -> braces (hcat (punctuate "," (map pretty (NE.toList xs))))

    prettyStringOp = \case
      StrLength -> "string length"
      StrLower -> "string lower"
      StrUpper -> "string upper"
      StrEscape -> "string escape"
      StrUnescape -> "string unescape"
      StrSplit d -> "string split" <+> escapeFishString d
      StrJoin d -> "string join" <+> escapeFishString d
      StrReplace o n -> "string replace" <+> escapeFishString o <+> escapeFishString n
      StrMatch p -> "string match" <+> escapeFishString p

    prettySpecialVar :: forall t1. SpecialVarRef t1 -> Doc ann
    prettySpecialVar = \case
      SVStatus -> "$status"
      SVPipestatus -> "$pipestatus"
      SVArgv -> "$argv"
      SVPID -> "$fish_pid"
      SVLastPID -> "$last_pid"
      SVHostname -> "$hostname"
      SVUser -> "$USER"
      SVHome -> "$HOME"
      SVPWD -> "$PWD"

    prettyProcessSubst stmts =
      let docBody = case NE.toList stmts of
            [s] -> prettyStmt s
            xs -> "begin" <> hardline <> indent 2 (vsep (map prettyStmt xs)) <> hardline <> "end"
       in parens (docBody <+> "|" <+> "psub")

-- | Escape strings for Fish shell.
-- Prefers single quotes. Uses double quotes if single quotes are present.
-- Escapes relevant characters inside double quotes.
escapeFishString :: Text -> Doc ann
escapeFishString s
  | T.any (`elem` ("\'" :: String)) s = doubleQuoted s
  | otherwise = singleQuoted s
  where
    singleQuoted t = "'" <> pretty (escapeSingle t) <> "'"
    doubleQuoted t = "\"" <> pretty (escapeDouble t) <> "\""

    -- Fish needs \ and $ escaped inside double quotes
    escapeDouble :: Text -> Text
    escapeDouble =
      T.concatMap
        ( \c -> case c of
            '"' -> "\\\""
            '\\' -> "\\\\"
            '$' -> "\\$"
            _ -> T.singleton c
        )
    -- Fish needs \ and ' escaped inside single quotes (by ending quote, escaping, starting quote)
    -- This is complex, so simpler to just use double quotes if ' is present.
    -- If we MUST use single quotes, it looks like: 'foo'\\''bar'
    escapeSingle :: Text -> Text
    escapeSingle = T.replace "'" "'\\''"

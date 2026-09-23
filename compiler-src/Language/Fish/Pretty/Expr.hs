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

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Language.Fish.AST
import Language.Fish.DSL.Internal (Script (..))
import Language.Fish.DSL.Name (identifierText)
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

prettyFishExprWith :: forall ann t. (FishStatement -> Doc ann) -> FishExpr t -> Doc ann
prettyFishExprWith prettyStmt = go
  where
    go :: forall t1. FishExpr t1 -> Doc ann
    go = \case
      ExprLiteral txt -> escapeFishString txt
      ExprEmbeddedScript (MkScript statements) ->
        escapeFishString (renderStrict (layoutPretty defaultLayoutOptions (vsep (map prettyStmt (filter nonemptyStatement statements)))))
      ExprNumLiteral i -> pretty i
      ExprVariable varRef -> prettyVarRef varRef
      ExprQuotedVariable varRef -> "\"" <> prettyVarRef varRef <> "\""
      ExprSpecialVar sv -> prettySpecialVar sv
      expression@(ExprStringConcat _ _) ->
        case concatParts expression of
          [] -> escapeFishString ""
          [part] -> go part
          parts -> hcat (map prettyConcatPart parts)
      ExprStringOp op e -> parens (prettyStringOp op <+> go e)
      ExprJoinList e ->
        quotedSubstitution
          ( "string join"
              <+> escapeFishString " "
              <+> "--"
              <+> go e
              <+> ";"
              <+> "or"
              <+> "printf"
              <+> escapeFishString ""
          )
      ExprFileRelative path ->
        quotedSubstitution
          ( "path resolve"
              <+> ( parens ("path dirname" <+> parens "status current-filename")
                      <> escapeFishString ("/" <> path)
                  )
          )
      ExprMath args ->
        parens ("math" <+> "--scale" <+> "0" <+> hsep (map go (NE.toList args)))
      ExprCommandSubst stmts ->
        "(" <> nest 2 (vsep (map prettyStmt (NE.toList stmts))) <> ")"
      ExprQuotedCommandSubst stmts ->
        "\"$(" <> nest 2 (vsep (map prettyStmt (NE.toList stmts))) <> ")\""
      ExprListLiteral [] -> mempty
      ExprListLiteral xs -> hsep (map go xs)
      ExprListConcat a b -> go a <+> go b
      ExprGlob g -> prettyGlob g
      ExprProcessSubst stmts -> "\"$" <> prettyProcessSubst stmts <> "\""

    quotedSubstitution document = "\"$(" <> document <> ")\""

    -- Literal runs form one word. In mixed words, retain a quote boundary so
    -- a suffix cannot become part of the preceding variable's identifier.
    prettyConcatPart :: FishExpr TStr -> Doc ann
    prettyConcatPart (ExprLiteral value) = quoteFishString value
    prettyConcatPart other = go other

    nonemptyStatement EmptyStmt = False
    nonemptyStatement (StmtList []) = False
    nonemptyStatement _ = True

    prettyVarRef :: forall t1. FishVarRef t1 -> Doc ann
    prettyVarRef = \case
      VarAll name -> "$" <> pretty (identifierText name)
      VarScalar name -> "$" <> pretty (identifierText name)
      VarIndex name idx -> "$" <> pretty (identifierText name) <> brackets (prettyFishIndex idx)

    prettyFishIndex :: forall a b. FishIndex a b -> Doc ann
    prettyFishIndex = \case
      IndexSingle e -> go e
      IndexRange e1 e2 ->
        let start = maybe mempty go e1
            end = maybe mempty go e2
         in start <> ".." <> end
      IndexList xs -> hsep (map go (NE.toList xs))

    prettyGlob (MkGlobPattern parts) = hcat (map prettyGlobPart parts)

    prettyGlobPart = \case
      GlobLiteral txt -> escapeFishString txt
      GlobStar -> "*"
      GlobStarStar -> "**"
      GlobQuestion -> "?"
      GlobCharClass pat -> escapeFishString ("[" <> pat <> "]")
      GlobBraces xs -> braces (hcat (punctuate "," (map escapeFishString (NE.toList xs))))

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
      SVHostname -> "\"$hostname\""
      SVUser -> "\"$USER\""
      SVHome -> "\"$HOME\""
      SVPWD -> "\"$PWD\""

    prettyProcessSubst stmts =
      let docBody = case NE.toList stmts of
            [s] -> prettyStmt s
            xs -> "begin" <> hardline <> indent 2 (vsep (map prettyStmt xs)) <> hardline <> "end"
       in parens (docBody <+> "|" <+> "psub")

-- | Merge adjacent literal fragments structurally, preserving dynamic order.
-- An entirely empty concatenation still renders as one empty argument.
concatParts :: FishExpr TStr -> [FishExpr TStr]
concatParts expression = filter nonempty (merge (flatten expression []))
  where
    flatten :: FishExpr TStr -> [FishExpr TStr] -> [FishExpr TStr]
    flatten (ExprStringConcat left right) rest = flatten left (flatten right rest)
    flatten part rest = part : rest
    merge :: [FishExpr TStr] -> [FishExpr TStr]
    merge (ExprLiteral left : ExprLiteral right : rest) = merge (ExprLiteral (left <> right) : rest)
    merge (part : rest) = part : merge rest
    merge [] = []
    nonempty :: FishExpr TStr -> Bool
    nonempty (ExprLiteral value) = not (T.null value)
    nonempty _ = True

-- | Escape a complete literal word, omitting quotes only for a conservative
-- ASCII subset. Keywords stay quoted even in command position; assignment
-- prefixes, expansions, glob syntax and control operators never pass through.
escapeFishString :: Text -> Doc ann
escapeFishString s
  | not (T.null s), T.all safeCharacter s, s `notElem` keywords = pretty s
  | otherwise = quoteFishString s
  where
    safeCharacter c = isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ("_./-:+@" :: String)
    keywords = ["and", "begin", "break", "builtin", "case", "command", "continue", "else", "end", "exec", "for", "function", "if", "in", "not", "or", "return", "switch", "then", "time", "while"]

quoteFishString :: Text -> Doc ann
quoteFishString s
  -- Literal line breaks in a Doc acquire layout indentation inside a block.
  -- Fish's unquoted escaped newline joins adjacent quoted fragments into the
  -- same word without exposing its contents to layout or expansion.
  | T.any (== '\0') s = hcat (punctuate "\\x00" (map (\part -> if T.null part then mempty else quoteFishString part) (T.splitOn "\0" s)))
  | T.any (== '\n') s = hcat (punctuate "\\n" (map (\part -> if T.null part then mempty else quoteFishString part) (T.splitOn "\n" s)))
  | T.any (`elem` ("\\\\'" :: String)) s = doubleQuoted s
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

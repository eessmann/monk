{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-} -- Recommended for prettyprinter

module Language.Fish.Pretty (prettyFish, renderFish) where

import Prelude hiding (show, print, group) -- Using Relude
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Language.Fish.AST
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

--------------------------------------------------------------------------------
-- 1. Top-level pretty-printing
--------------------------------------------------------------------------------

-- | Pretty-print an entire Fish script (list of statements).
prettyFish :: [FishStatement] -> Doc ann
prettyFish = vsep . map prettyFishStatement

-- | Render the Doc to final Text.
renderFish :: [FishStatement] -> Text
renderFish = renderStrict . layoutPretty defaultLayoutOptions . prettyFish

--------------------------------------------------------------------------------
-- 2. FishStatement
--------------------------------------------------------------------------------

prettyFishStatement :: FishStatement -> Doc ann
prettyFishStatement = \case
  Stmt cmd -> prettyFishCommand cmd
  StmtList stmts -> vsep (map prettyFishStatement stmts)
  Comment txt -> "#" <> pretty txt -- Keep comments without leading space for simplicity
  Freestanding (FreestandingArgumentList args) -> -- Updated for new AST
    hsep (map prettyFishExpr args)
  SemiNl -> ";" <> hardline -- Represents a semicolon potentially followed by newline

--------------------------------------------------------------------------------
-- 3. FishCommand
--------------------------------------------------------------------------------

prettyFishCommand :: FishCommand t -> Doc ann
prettyFishCommand = \case
  Command txt args ->
    pretty txt <+> hsep (map prettyExprOrRedirect args) -- Use prettyExprOrRedirect
  Set scope var expr -> -- Use FishExpr
    "set" <+> prettyScope scope <+> pretty var <+> prettyFishExpr expr
  Function fishFn ->
    prettyFunction fishFn
  For var listArgs body -> -- NonEmpty list of expressions
    "for" <+> pretty var <+> "in" <+> hsep (map prettyFishExpr (NE.toList listArgs)) <> hardline <>
      indent 2 (vsep (map prettyFishStatement (NE.toList body))) <> hardline <> "end"
  While cond body -> -- Use NonEmpty body
    "while" <+> prettyFishExpr cond <> hardline <>
      indent 2 (vsep (map prettyFishStatement (NE.toList body))) <> hardline <> "end"
  Begin stmts -> -- Use NonEmpty body
    "begin" <> hardline <> indent 2 (vsep (map prettyFishStatement (NE.toList stmts)))
      <> hardline <> "end"
  If cond thn els -> -- Use NonEmpty body for 'then'
    "if" <+> prettyFishExpr cond <> hardline <>
      indent 2 (vsep (map prettyFishStatement (NE.toList thn))) <>
      (if null els then mempty
       else hardline <> "else" <> hardline <> indent 2 (vsep (map prettyFishStatement els))) <>
      hardline <> "end"
  Switch expr cases -> -- Use FishExpr, NonEmpty cases
    "switch" <+> prettyFishExpr expr <> hardline <>
      indent 2 (vsep (map prettyCaseItem (NE.toList cases))) <> hardline <> "end"
  Break ->
    "break"
  Continue ->
    "continue"
  Return mexpr -> -- Optional status expression
    case mexpr of
      Nothing -> "return"
      Just e  -> "return" <+> prettyFishExpr e
  Source fileExpr -> -- Use FishExpr
    "source" <+> prettyFishExpr fileExpr
  Brace stmts -> -- Use NonEmpty body
    "{" <> hardline <> indent 2 (vsep (map prettyFishStatement (NE.toList stmts)))
      <> hardline <> "}"
  -- Heredocs are not supported in fish; removed
  Read var ->
    "read" <+> pretty var
  Echo args -> -- Use NonEmpty FishExpr
    "echo" <+> hsep (map prettyFishExpr (NE.toList args))
  Printf fmt args -> -- Use FishExpr
    "printf" <+> prettyFishExpr fmt <+> hsep (map prettyFishExpr args)
  Pipeline jp -> prettyJobPipeline jp
  JobConj jc -> prettyJobConjunction jc
  Semicolon cmd1 cmd2 ->
     prettyFishCommand cmd1 <> ";" <> line <> prettyFishCommand cmd2 -- Allow break after ;
  Not cmd -> -- Takes FishCommand TStatus
    "not" <+> prettyFishCommand cmd
  Background cmd ->
    prettyFishCommand cmd <+> "&"
  Decorated dec cmd ->
    prettyDecoration dec <+> prettyFishCommand cmd
  TryCatch tryStmts catchStmts -> -- Use NonEmpty bodies
    "try" <> hardline <>
      indent 2 (vsep (map prettyFishStatement (NE.toList tryStmts))) <> hardline <>
      "catch" <> hardline <>
      indent 2 (vsep (map prettyFishStatement (NE.toList catchStmts))) <> hardline <>
      "end"

--------------------------------------------------------------------------------
-- 4. Job conjunction, scopes, function, etc.
--------------------------------------------------------------------------------

-- New Fish job model pretty printers
prettyJobPipeline :: FishJobPipeline -> Doc ann
prettyJobPipeline (FishJobPipeline time vars stmt conts bg) =
  let timeDoc = if time then "time" <> space else mempty
      varsDoc = if null vars then mempty else hsep (map prettyVarAssign vars) <> space
      headDoc = group (timeDoc <> varsDoc <> prettyFishStatement stmt)
      restDocs = map (\(PipeTo v s) -> space <> "|" <+> (if null v then mempty else hsep (map prettyVarAssign v) <> space) <> prettyFishStatement s) conts
      pipesDoc = mconcat restDocs
   in group (headDoc <> pipesDoc <> if bg then space <> "&" else mempty)

prettyVarAssign :: VariableAssignment -> Doc ann
prettyVarAssign (VariableAssignment name mval) =
  case mval of
    Nothing -> pretty name <> "="
    Just v  -> pretty name <> "=" <> prettyFishExpr v

prettyJobConjunction :: FishJobConjunction -> Doc ann
prettyJobConjunction (FishJobConjunction mdec job conts semi) =
  let headDoc = case mdec of
                  Nothing -> prettyJobPipeline job
                  Just d  -> prettyConjunction d <+> prettyJobPipeline job
      rest = map prettyJCont conts
      semiDoc = if semi then ";" else mempty
   in headDoc <> mconcat rest <> semiDoc
  where
    prettyJCont = \case
      JCAnd jp -> hardline <> prettyConjunction ConjAnd <+> prettyJobPipeline jp
      JCOr jp  -> hardline <> prettyConjunction ConjOr  <+> prettyJobPipeline jp

prettyConjunction :: Conjunction -> Doc ann
prettyConjunction = \case
  ConjAnd -> "and"
  ConjOr  -> "or"

prettyScope :: VariableScope -> Doc ann
prettyScope = \case
  ScopeLocal -> "--local"
  ScopeGlobal -> "--global"
  ScopeExported -> "--export"
  ScopeUniversal -> "--universal"

prettyFunction :: FishFunction -> Doc ann
prettyFunction (FishFunction name flags params body) =
  let flagsDoc = case flags of
                   [] -> mempty
                   fs -> space <> hsep (map prettyFunctionFlag fs)
      paramsDoc = if null params then mempty else " --argument" <+> hsep (map prettyFishExpr params)
  in "function" <+> pretty name <> flagsDoc <> paramsDoc
      <> hardline <> indent 2 (vsep (map prettyFishStatement (NE.toList body)))
      <> hardline <> "end"

prettyFunctionFlag :: FunctionFlag -> Doc ann
prettyFunctionFlag = \case
  FuncDescription txt -> "--description" <+> escapeFishString txt -- Escape description
  FuncOnEvent evt -> "--on-event" <+> pretty evt
  FuncHelp -> "--help"
  FuncInheritVariable -> "--inherit-variable"
  FuncUnknownFlag txt -> pretty txt -- Assume flags are safe or already formatted

--------------------------------------------------------------------------------
-- 5. ExprOrRedirect (Replaces ArgOrRedirect)
--------------------------------------------------------------------------------

prettyExprOrRedirect :: ExprOrRedirect -> Doc ann
prettyExprOrRedirect = \case
  ExprVal expr -> prettyFishExpr expr -- Use prettyFishExpr
  RedirectVal op expr -> prettyRedirectOp op <+> prettyFishExpr expr -- Use prettyFishExpr

--------------------------------------------------------------------------------
-- 6. Expressions (Replaces Arguments)
--------------------------------------------------------------------------------

prettyFishExpr :: FishExpr t -> Doc ann
prettyFishExpr = \case
  ExprLiteral txt -> escapeFishString txt
  ExprNumLiteral i -> pretty i
  ExprBoolLiteral b -> if b then "true" else "false" -- Fish uses true/false literals
  ExprVariable var Nothing -> "$" <> pretty var
  ExprVariable var (Just idx) -> "$" <> pretty var <> brackets (prettyFishIndex idx)
  ExprStringConcat e1 e2 -> prettyFishExpr e1 <> prettyFishExpr e2 -- Concatenation is implicit
  ExprNumArith op e1 e2 ->
    -- Fish uses `math` command for arithmetic
    parens ("math" <+> prettyFishExpr e1 <+> prettyArithOp op <+> prettyFishExpr e2)
  ExprBoolExpr bExpr ->
    -- Boolean expressions often evaluated directly or via `test`
    prettyBoolExpr bExpr
  ExprCommandSubstStr stmts ->
    "(" <> align (vsep (map prettyFishStatement (NE.toList stmts))) <> ")"
  ExprCommandSubst stmts ->
    "(" <> align (vsep (map prettyFishStatement (NE.toList stmts))) <> ")"


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
    escapeDouble = T.concatMap (\c -> case c of
                                         '"'  -> "\\\"" -- Though we avoid embedding " by choosing ' or "
                                         '\\' -> "\\\\"
                                         '$'  -> "\\$"
                                         _    -> T.singleton c)
    -- Fish needs \ and ' escaped inside single quotes (by ending quote, escaping, starting quote)
    -- This is complex, so simpler to just use double quotes if ' is present.
    -- If we MUST use single quotes, it looks like: 'foo'\'bar'
    escapeSingle :: Text -> Text
    escapeSingle = T.replace "'" "'\\''" -- Replace ' with '\''

--------------------------------------------------------------------------------
-- 7. Numeric & Boolean Sub-AST Printing
--------------------------------------------------------------------------------

-- This might not be directly used if ExprNumArith prints using `math`
prettyNumExpr :: NumExpr -> Doc ann
prettyNumExpr = \case
  NumLit i -> pretty i
  NumVar var -> "$" <> pretty var
  NumArth op e1 e2 -> parens (prettyNumExpr e1 <+> prettyArithOp op <+> prettyNumExpr e2)

prettyArithOp :: ArithOp -> Doc ann
prettyArithOp = \case
  OpPlus -> "+"
  OpMinus -> "-"
  OpMultiply -> "\\*" -- Need to escape '*' in Fish for math
  OpDivide -> "/"
  OpModulo -> "%"

prettyBoolExpr :: BoolExpr -> Doc ann
prettyBoolExpr = \case
  BoolVar var -> "$" <> pretty var  -- Add the missing pattern
  BoolAnd b1 b2 -> group (prettyFishExpr b1 <+> "&&" <> line <> prettyFishExpr b2)
  BoolOr b1 b2 -> group (prettyFishExpr b1 <+> "||" <> line <> prettyFishExpr b2)
  BoolNot b -> parens ("not" <+> prettyFishExpr b)
  -- BoolTestOp needs to become `test` or a direct evaluation if possible
  BoolTestOp op e1 e2 ->
    -- Use `test` command
    "test" <+> prettyFishExpr e1 <+> prettyTestOpFish op <+> prettyFishExpr e2
  BoolCommand cmd ->
    -- Commands used in boolean context are executed directly
    prettyFishCommand cmd

--------------------------------------------------------------------------------
-- 8. Indexing, CaseItem, Redirection, Decoration, and TestOperator
--------------------------------------------------------------------------------

prettyFishIndex :: FishIndex -> Doc ann
prettyFishIndex = \case
  SingleIndex e -> prettyFishExpr e
  RangeIndex e1 e2 -> prettyFishExpr e1 <> ".." <> prettyFishExpr e2 -- Fish uses '..' for ranges

prettyCaseItem :: CaseItem -> Doc ann
prettyCaseItem (CaseItem pats body) = -- Use NonEmpty patterns/body
  "case" <+> hsep (punctuate " |" (map prettyFishExpr (NE.toList pats))) <> hardline <> -- Patterns separated by |
    indent 2 (vsep (map prettyFishStatement (NE.toList body)))

prettyRedirectOp :: RedirectOp -> Doc ann
prettyRedirectOp = \case
  RedirectOut -> ">"
  RedirectOutAppend -> ">>"
  RedirectIn -> "<"
  RedirectErr -> "2>"
  RedirectErrAppend -> "2>>"
  RedirectBoth -> "&>"
  RedirectBothAppend -> "&>>"

prettyDecoration :: Decoration -> Doc ann
prettyDecoration = \case
  DecBuiltin -> "builtin"
  DecCommand -> "command"
  DecExec -> "exec"

prettyTestOpFish :: TestOperator -> Doc ann
prettyTestOpFish = \case
  Eq -> "="
  Neq -> "!="
  Gt -> "-gt"
  Lt -> "-lt"
  Ge -> "-ge"
  Le -> "-le"
  IsFile -> "-f"
  IsDir -> "-d"
  IsSymlink -> "-L"
  Exists -> "-e"
  IsReadable -> "-r"
  IsWritable -> "-w"
  IsExecutable -> "-x"
  IsEmpty -> "-z"
  NotEmpty -> "-n"

--------------------------------------------------------------------------------
-- Removed Freestanding helper
--------------------------------------------------------------------------------
-- getFreestandingArgumentList :: FreestandingArgumentList -> [FishExpr TStr]
-- getFreestandingArgumentList (FreestandingArgumentList xs) = xs

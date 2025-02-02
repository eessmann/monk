module Language.Fish.Pretty (prettyFish, renderFish) where

import Data.List.NonEmpty qualified as NE
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
prettyFishStatement (Stmt cmd) = prettyFishCommand cmd
prettyFishStatement (StmtList stmts) = vsep (map prettyFishStatement stmts)
prettyFishStatement (Comment txt) = "#" <+> pretty txt
prettyFishStatement (Freestanding args) =
  hsep (map prettyFishArg (getFreestandingArgumentList args))
prettyFishStatement SemiNl = mempty

--------------------------------------------------------------------------------
-- 3. FishCommand
--------------------------------------------------------------------------------

prettyFishCommand :: FishCommand t -> Doc ann
prettyFishCommand = \case
  Command txt args ->
    pretty txt <+> hsep (map prettyArgOrRedirect args)
  Set scope var arg ->
    "set" <+> prettyScope scope <+> pretty var <+> prettyFishArg arg
  Function fishFn ->
    prettyFunction fishFn
  For var listArg body ->
    "for" <+> pretty var <+> "in" <+> prettyFishArg listArg <> hardline <>
      indent 2 (vsep (map prettyFishStatement body)) <> hardline <> "end"
  While cond body ->
    "while" <+> prettyFishExpr cond <> hardline <>
      indent 2 (vsep (map prettyFishStatement body)) <> hardline <> "end"
  Begin stmts ->
    "begin" <> hardline <> indent 2 (vsep (map prettyFishStatement stmts))
      <> hardline <> "end"
  If cond thn els ->
    "if" <+> prettyFishExpr cond <> hardline <>
      indent 2 (vsep (map prettyFishStatement thn)) <>
      (if null els then mempty
       else hardline <> "else" <> hardline <> indent 2 (vsep (map prettyFishStatement els))) <>
      hardline <> "end"
  Switch arg cases ->
    "switch" <+> prettyFishArg arg <> hardline <>
      indent 2 (vsep (map prettyCaseItem cases)) <> hardline <> "end"
  Break ->
    "break"
  Continue ->
    "continue"
  Return val ->
    "return" <+> prettyFishArg val
  Source fileArg ->
    "source" <+> prettyFishArg fileArg
  Brace stmts ->
    "{" <> hardline <> indent 2 (vsep (map prettyFishStatement stmts))
      <> hardline <> "}"
  HereDoc typeArg contentArg ->
    "here-doc" <+> parens (prettyFishArg typeArg) <+> "<<" <+> parens (prettyFishArg contentArg)
  SubcommandOutput stmts ->
    "(" <> align (vsep (map prettyFishStatement stmts)) <> ")"
  Read var ->
    "read" <+> pretty var
  Echo arg ->
    "echo" <+> prettyFishArg arg
  Printf fmt args ->
    "printf" <+> prettyFishArg fmt <+> hsep (map prettyFishArg args)
  Redirect cmd op redirArg ->
    prettyFishCommand cmd <+> prettyRedirectOp op <+> prettyFishArg redirArg
  Pipeline neCmds ->
    let cmds = NE.toList neCmds
     in hsep (punctuate (" |") (map prettyFishCommand cmds))
  JobControl conj leftCmd rightCmd ->
    prettyFishCommand leftCmd <+> prettyJobConjunction conj <+> prettyFishCommand rightCmd
  Semicolon cmd1 cmd2 ->
    prettyFishCommand cmd1 <> ";" <+> prettyFishCommand cmd2
  Not cmd ->
    "not" <+> prettyFishCommand cmd
  Background cmd ->
    prettyFishCommand cmd <+> "&"
  Disown cmd ->
    prettyFishCommand cmd <+> "& disown"
  Decorated dec cmd ->
    prettyDecoration dec <+> prettyFishCommand cmd
  TryCatch tryStmts catchStmts ->
    "try" <> hardline <>
      indent 2 (vsep (map prettyFishStatement tryStmts)) <> hardline <>
      "catch" <> hardline <>
      indent 2 (vsep (map prettyFishStatement catchStmts)) <> hardline <>
      "end"

--------------------------------------------------------------------------------
-- 4. Job conjunction, scopes, function, etc.
--------------------------------------------------------------------------------

prettyJobConjunction :: JobConjunction -> Doc ann
prettyJobConjunction = \case
  ConjAnd -> "&&"
  ConjOr -> "||"

prettyScope :: VariableScope -> Doc ann
prettyScope = \case
  ScopeLocal -> "--local"
  ScopeGlobal -> "--global"
  ScopeExported -> "--export"
  ScopeUniversal -> "--universal"

prettyFunction :: FishFunction -> Doc ann
prettyFunction (FishFunction name flags params body) =
  "function" <+> pretty name <+> hsep (map prettyFunctionFlag flags)
    <> (if null params then mempty else " -a " <> hsep (map prettyFishArg params))
    <> hardline <> indent 2 (vsep (map prettyFishStatement body))
    <> hardline <> "end"

prettyFunctionFlag :: FunctionFlag -> Doc ann
prettyFunctionFlag = \case
  FuncDescription txt -> "--description" <+> dquotes (pretty txt)
  FuncOnEvent evt -> "--on-event" <+> pretty evt
  FuncHelp -> "--help"
  FuncInheritVariable -> "--inherit-variable"
  FuncUnknownFlag txt -> pretty txt

--------------------------------------------------------------------------------
-- 5. ArgOrRedirect
--------------------------------------------------------------------------------

prettyArgOrRedirect :: FishArgOrRedirect -> Doc ann
prettyArgOrRedirect = \case
  Arg arg -> prettyFishArg arg
  Redir op arg -> prettyRedirectOp op <+> prettyFishArg arg

--------------------------------------------------------------------------------
-- 6. Arguments
--------------------------------------------------------------------------------

prettyFishArg :: FishArg t -> Doc ann
prettyFishArg = \case
  ArgLiteral txt -> escapeFishString txt
  ArgNumber i -> pretty i
  ArgVariable var -> "$" <> pretty var
  ArgConcat a1 a2 -> prettyFishArg a1 <> prettyFishArg a2
  ArgList xs -> parens (hsep (map prettyFishArg xs))
  ArgSubstituted cmd -> parens (prettyFishCommand cmd)

-- | Improved fish string escaping: if the string contains a single quote, we use double quotes (and escape
-- embedded double quotes and backslashes). Otherwise we use single quotes.
escapeFishString :: Text -> Doc ann
escapeFishString s =
  if T.any (== '\'') s
    then dquotes (pretty (escapeDouble s))
    else squotes (pretty s)
  where
    escapeDouble :: Text -> Text
    escapeDouble = T.concatMap (\c -> case c of
                                          '"'  -> "\\\""
                                          '\\' -> "\\\\"
                                          _    -> T.singleton c)
    squotes d = "'" <> d <> "'"

--------------------------------------------------------------------------------
-- 7. Expressions
--------------------------------------------------------------------------------

prettyFishExpr :: FishExpr t -> Doc ann
prettyFishExpr = \case
  ExprStringLit s -> escapeFishString (T.pack s)
  ExprStringVar var Nothing -> "$" <> pretty var
  ExprStringVar var (Just idx) -> "$" <> pretty var <> brackets (prettyFishIndex idx)
  ExprStringConcat e1 e2 -> prettyFishExpr e1 <> prettyFishExpr e2
  ExprListLit xs -> list (map prettyFishExpr xs)
  ExprNum nExpr -> prettyNumExpr nExpr
  ExprBool bExpr -> prettyBoolExpr bExpr
  ExprSubstituted cmd -> parens (prettyFishCommand cmd)

--------------------------------------------------------------------------------
-- 8. Numeric & Boolean sub-AST
--------------------------------------------------------------------------------

prettyNumExpr :: NumExpr -> Doc ann
prettyNumExpr = \case
  NumLiteral i -> pretty i
  NumVariable var -> "$" <> pretty var
  NumArith op e1 e2 -> parens (prettyNumExpr e1 <+> prettyArithOp op <+> prettyNumExpr e2)

prettyArithOp :: ArithOp -> Doc ann
prettyArithOp = \case
  OpPlus -> "+"
  OpMinus -> "-"
  OpMultiply -> "*"
  OpDivide -> "/"
  OpModulo -> "%"

prettyBoolExpr :: BoolExpr -> Doc ann
prettyBoolExpr = \case
  BoolLiteral True -> "true"
  BoolLiteral False -> "false"
  BoolVariable var -> "$" <> pretty var
  BoolAnd b1 b2 -> parens (prettyBoolExpr b1 <+> "&&" <+> prettyBoolExpr b2)
  BoolOr b1 b2 -> parens (prettyBoolExpr b1 <+> "||" <+> prettyBoolExpr b2)
  BoolNot b -> "(not" <+> prettyBoolExpr b <> ")"
  BoolTestOp op a1 a2 -> hsep ["test", prettyFishArg a1, prettyTestOp op, prettyFishArg a2]
  BoolCommand cmd -> parens (prettyFishCommand cmd)

--------------------------------------------------------------------------------
-- 9. Indexing, CaseItem, Redirection, Decoration, and TestOperator
--------------------------------------------------------------------------------

prettyFishIndex :: FishIndex -> Doc ann
prettyFishIndex = \case
  SingleIndex e -> prettyFishExpr e
  RangeIndex e1 e2 -> prettyFishExpr e1 <> ".." <> prettyFishExpr e2

prettyCaseItem :: CaseItem -> Doc ann
prettyCaseItem (CaseItem pats body) =
  "case" <+> hsep (map prettyFishArg pats) <> hardline <>
    indent 2 (vsep (map prettyFishStatement body))

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

prettyTestOp :: TestOperator -> Doc ann
prettyTestOp = \case
  Eq -> "="
  Neq -> "!="
  Gt -> ">"
  Lt -> "<"
  Ge -> ">="
  Le -> "<="
  IsFile -> "-f"
  IsDir -> "-d"
  IsSymlink -> "-h"
  StringContains -> "string contains"
  StringMatch -> "string match"

--------------------------------------------------------------------------------
-- 10. Freestanding argument list
--------------------------------------------------------------------------------

getFreestandingArgumentList :: FreestandingArgumentList -> [FishArg TStr]
getFreestandingArgumentList (FreestandingArgumentList xs) = xs
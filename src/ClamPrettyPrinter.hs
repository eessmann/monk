module ClamPrettyPrinter (prettyFish, renderFish) where

import Data.List.NonEmpty qualified as NE
import FishAST
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

--------------------------------------------------------------------------------
-- 1. Top-level pretty-printing
--------------------------------------------------------------------------------

-- | Pretty-print an entire Fish script (list of statements).
prettyFish :: [FishStatement] -> Doc ann
prettyFish = vsep . map prettyFishStatement

-- | Convert the doc to a final 'String' (or you could render to Text, etc.).
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
prettyFishStatement SemiNl = mempty -- or hardline if you prefer

--------------------------------------------------------------------------------
-- 3. FishCommand
--------------------------------------------------------------------------------

-- | Pretty-print any FishCommand, ignoring the type index since we only need text output.
prettyFishCommand :: FishCommand t -> Doc ann
prettyFishCommand = \case
  ----------------------------------------------------------------
  -- Basic commands
  ----------------------------------------------------------------
  Command txt args ->
    pretty txt <+> hsep (map prettyArgOrRedirect args)
  Set scope var arg ->
    "set"
      <+> prettyScope scope
      <+> pretty var
      <+> prettyFishArg arg
  Function fishFn ->
    prettyFunction fishFn
  For var listArg body ->
    "for"
      <+> pretty var
      <+> "in"
      <+> (prettyFishArg listArg)
      <> hardline
      <> indent 2 (vsep (map prettyFishStatement body))
      <> hardline
      <> "end"
  While cond body ->
    "while"
      <+> prettyFishExpr cond
      <> hardline
      <> indent 2 (vsep (map prettyFishStatement body))
      <> hardline
      <> "end"
  Begin body ->
    "begin"
      <> hardline
      <> indent 2 (vsep (map prettyFishStatement body))
      <> hardline
      <> "end"
  If cond thn els ->
    "if"
      <+> prettyFishExpr cond
      <> hardline
      <> indent 2 (vsep (map prettyFishStatement thn))
      <> ( if null els
             then mempty
             else
               hardline
                 <> "else"
                 <> hardline
                 <> indent 2 (vsep (map prettyFishStatement els))
         )
      <> hardline
      <> "end"
  Switch arg cases ->
    "switch"
      <+> prettyFishArg arg
      <> hardline
      <> indent 2 (vsep (map prettyCaseItem cases))
      <> hardline
      <> "end"
  Break ->
    "break"
  Continue ->
    "continue"
  Return val ->
    "return" <+> prettyFishArg val
  Source fileArg ->
    "source" <+> prettyFishArg fileArg
  Brace stmts ->
    "{"
      <> hardline
      <> indent 2 (vsep (map prettyFishStatement stmts))
      <> hardline
      <> "}"
  HereDoc typeArg contentArg ->
    -- You might want to print something like:  cat <<EOF ... EOF
    -- For simplicity, just illustrate:
    parens (prettyFishArg typeArg) <+> "<<" <+> parens (prettyFishArg contentArg)
  ----------------------------------------------------------------
  -- IO / Redirection
  ----------------------------------------------------------------
  Read var ->
    "read" <+> pretty var
  Echo arg ->
    "echo" <+> prettyFishArg arg
  Printf fmt args ->
    "printf" <+> prettyFishArg fmt <+> hsep (map prettyFishArg args)
  Redirect cmd op redirArg ->
    prettyFishCommand cmd <+> prettyRedirectOp op <+> prettyFishArg redirArg
  ----------------------------------------------------------------
  -- Combining Commands / Job Control
  ----------------------------------------------------------------
  Pipeline neCmds ->
    -- For example: cmd1 | cmd2 | cmd3
    let cmds = NE.toList neCmds
     in hsep (punctuate (space <> "|") (map prettyFishCommand cmds))
  JobControl conj leftCmd rightCmd ->
    -- fish doesn't exactly use '&&' or '||' in the same way as POSIX shells,
    -- but let's approximate:
    prettyFishCommand leftCmd <+> prettyJobConjunction conj <+> prettyFishCommand rightCmd
  Semicolon cmd1 cmd2 ->
    prettyFishCommand cmd1 <> ";" <+> prettyFishCommand cmd2
  Not cmd ->
    "not" <+> prettyFishCommand cmd
  Background cmd ->
    prettyFishCommand cmd <+> "&"
  Disown cmd ->
    prettyFishCommand cmd <+> "& disown"
  Decorated decoration cmd ->
    prettyDecoration decoration <+> prettyFishCommand cmd
  TryCatch tryStmts catchStmts ->
    -- Fish has `try/catch` syntax:
    --     try
    --       ...
    --     catch
    --       ...
    --     end
    "try"
      <> hardline
      <> indent 2 (vsep (map prettyFishStatement tryStmts))
      <> hardline
      <> "catch"
      <> hardline
      <> indent 2 (vsep (map prettyFishStatement catchStmts))
      <> hardline
      <> "end"

--------------------------------------------------------------------------------
-- 4. Job Conjunction, Scopes, Function, etc.
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
  hsep (["function", pretty name] ++ map prettyFunctionFlag flags ++ ["-a"] ++ map prettyFishArg params)
    <> hardline
    <> indent 2 (vsep (map prettyFishStatement body))
    <> hardline
    <> "end"

prettyFunctionFlag :: FunctionFlag -> Doc ann
prettyFunctionFlag = \case
  FuncDescription txt -> "--description" <+> dquotes (pretty txt)
  FuncOnEvent evt -> "--on-event" <+> pretty evt
  FuncHelp -> "--help"
  FuncInheritVariable -> "--inherit-variable"
  FuncUnknownFlag txt -> pretty txt -- or handle differently

--------------------------------------------------------------------------------
-- 5. ArgOrRedirect
--------------------------------------------------------------------------------

prettyArgOrRedirect :: FishArgOrRedirect -> Doc ann
prettyArgOrRedirect = \case
  Arg arg -> prettyFishArg arg
  Redir op redArg -> prettyRedirectOp op <+> prettyFishArg redArg

--------------------------------------------------------------------------------
-- 6. Arguments
--------------------------------------------------------------------------------

prettyFishArg :: FishArg t -> Doc ann
prettyFishArg = \case
  ArgLiteral txt -> pretty txt
  ArgNumber i -> pretty i
  ArgVariable var -> "$" <> pretty var
  ArgConcat a1 a2 -> prettyFishArg a1 <> prettyFishArg a2
  ArgList xs -> hsep (map prettyFishArg xs)
  ArgSubstituted c -> parens (prettyFishCommand c)

-- If you want a friendlier list syntax, e.g. space-separated within parentheses:
-- ArgList xs -> parens (hsep (map prettyFishArg xs))

--------------------------------------------------------------------------------
-- 7. Expressions
--------------------------------------------------------------------------------

prettyFishExpr :: FishExpr t -> Doc ann
prettyFishExpr = \case
  ExprStringLit str -> dquotes (pretty (escapeString str))
  ExprStringVar var mbIx ->
    let base = "$" <> pretty var
     in case mbIx of
          Nothing -> base
          Just idx -> base <> brackets (prettyFishIndex idx)
  ExprStringConcat e1 e2 ->
    prettyFishExpr e1 <> prettyFishExpr e2
  ExprListLit xs ->
    list (map prettyFishExpr xs)
  ExprNum nExpr ->
    prettyNumExpr nExpr
  ExprBool bExpr ->
    prettyBoolExpr bExpr
  ExprSubstituted cmd ->
    parens (prettyFishCommand cmd)

--------------------------------------------------------------------------------
-- 8. Numeric & Boolean sub-AST
--------------------------------------------------------------------------------

prettyNumExpr :: NumExpr -> Doc ann
prettyNumExpr = \case
  NumLiteral i -> pretty i
  NumVariable var -> "$" <> pretty var
  NumArith op e1 e2 ->
    parens (prettyNumExpr e1 <+> prettyArithOp op <+> prettyNumExpr e2)

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
  BoolAnd b1 b2 ->
    parens (prettyBoolExpr b1 <+> "&&" <+> prettyBoolExpr b2)
  BoolOr b1 b2 ->
    parens (prettyBoolExpr b1 <+> "||" <+> prettyBoolExpr b2)
  BoolNot b ->
    "(not" <+> prettyBoolExpr b <> ")"
  BoolTestOp op a1 a2 ->
    hsep ["test", prettyFishArg a1, prettyTestOp op, prettyFishArg a2]
  BoolCommand cmd ->
    parens (prettyFishCommand cmd)

--------------------------------------------------------------------------------
-- 9. Indexing, CaseItem, Redirection, Decoration
--------------------------------------------------------------------------------

prettyFishIndex :: FishIndex -> Doc ann
prettyFishIndex = \case
  SingleIndex e -> prettyFishExpr e
  RangeIndex e1 e2 -> prettyFishExpr e1 <> ".." <> prettyFishExpr e2

prettyCaseItem :: CaseItem -> Doc ann
prettyCaseItem (CaseItem pats body) =
  "case"
    <+> hsep (map prettyFishArg pats)
    <> hardline
    <> indent 2 (vsep (map prettyFishStatement body))

prettyRedirectOp :: RedirectOp -> Doc ann
prettyRedirectOp = \case
  RedirectOut -> ">"
  RedirectOutAppend -> ">>"
  RedirectIn -> "<"
  RedirectErr -> "^>"
  RedirectErrAppend -> "^>>"
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
-- 10. Helper utilities
--------------------------------------------------------------------------------

-- | Extract arg list from FreestandingArgumentList
getFreestandingArgumentList :: FreestandingArgumentList -> [FishArg TStr]
getFreestandingArgumentList (FreestandingArgumentList xs) = xs

-- | Very naive string escaping
escapeString :: String -> String
escapeString [] = []
escapeString ('\"' : xs) = '\\' : '\"' : escapeString xs
escapeString ('\\' : xs) = '\\' : '\\' : escapeString xs
escapeString ('\n' : xs) = '\\' : 'n' : escapeString xs
escapeString ('\t' : xs) = '\\' : 't' : escapeString xs
escapeString (x : xs) = x : escapeString xs

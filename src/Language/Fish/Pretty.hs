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
  For var listExpr body -> -- Use FishExpr, NonEmpty body
    "for" <+> pretty var <+> "in" <+> prettyFishExpr listExpr <> hardline <>
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
  Return expr -> -- Use FishExpr
    "return" <+> prettyFishExpr expr
  Source fileExpr -> -- Use FishExpr
    "source" <+> prettyFishExpr fileExpr
  Brace stmts -> -- Use NonEmpty body
    "{" <> hardline <> indent 2 (vsep (map prettyFishStatement (NE.toList stmts)))
      <> hardline <> "}"
  HereDoc typeExpr contentExpr -> -- Use FishExpr
    "<<" <+> prettyFishExpr typeExpr <> hardline <> prettyFishExpr contentExpr -- Common heredoc syntax
  SubcommandOutputStr stmts -> -- Use NonEmpty body
    "(" <> align (vsep (map prettyFishStatement (NE.toList stmts))) <> ")"
  SubcommandOutputList stmts -> -- Print same as string output
    "(" <> align (vsep (map prettyFishStatement (NE.toList stmts))) <> ")"
  Read var ->
    "read" <+> pretty var
  Echo args -> -- Use NonEmpty FishExpr
    "echo" <+> hsep (map prettyFishExpr (NE.toList args))
  Printf fmt args -> -- Use FishExpr
    "printf" <+> prettyFishExpr fmt <+> hsep (map prettyFishExpr args)
  Redirect cmd op redirExpr -> -- Use FishExpr
    group (prettyFishCommand cmd <+> prettyRedirectOp op <+> prettyFishExpr redirExpr) -- Group redirection
  Pipeline neCmds ->
    let cmds = NE.toList neCmds
     in group (hsep (punctuate (" |" <> line) (map prettyFishCommand cmds))) -- Allow breaking after pipe
  JobControl conj leftCmd rightCmd ->
    group (prettyFishCommand leftCmd <+> prettyJobConjunction conj <> line <> prettyFishCommand rightCmd) -- Allow break after &&/||
  Semicolon cmd1 cmd2 ->
     prettyFishCommand cmd1 <> ";" <> line <> prettyFishCommand cmd2 -- Allow break after ;
  Not cmd -> -- Takes FishCommand TStatus
    "not" <+> prettyFishCommand cmd
  Background cmd ->
    prettyFishCommand cmd <+> "&"
  Disown cmd ->
    -- Fish doesn't have a direct `disown` command applied like this.
    -- Typically done via `cmd &; disown` or separate `disown %jobid`.
    -- This AST node might need rethinking during translation or printing.
    -- Printing as sequence for now:
    prettyFishCommand cmd <+> "&;" <+> "disown"
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
    -- Use FishExpr for params, NonEmpty for body
    <> (if null params then mempty else " --argument" <+> hsep (map prettyFishExpr params))
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
  ExprListLit xs -> parens (hsep (map prettyFishExpr xs)) -- List expansion syntax
  ExprNumArith op e1 e2 ->
    -- Fish uses `math` command for arithmetic
    parens ("math" <+> prettyFishExpr e1 <+> prettyArithOp op <+> prettyFishExpr e2)
  ExprBoolExpr bExpr ->
    -- Boolean expressions often evaluated directly or via `test`
    prettyBoolExpr bExpr
  ExprSubstituted cmd ->
    -- Print the command itself, assuming it returns the correct type (e.g., string)
    prettyFishCommand cmd -- Already wrapped in parens by command substitution constructors


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
    "test" <+> prettyFishExpr e1 <+> prettyTestOp op <+> prettyFishExpr e2
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

prettyTestOp :: TestOperator -> Doc ann
prettyTestOp = \case
  Eq -> "="   -- Fish uses = for string and numeric equality in `test`
  Neq -> "!=" -- Fish uses != for string and numeric inequality in `test`
  Gt -> "-gt"
  Lt -> "-lt"
  Ge -> "-ge"
  Le -> "-le"
  StrEq -> "=" -- Deprecated -eq maps to =
  StrNeq -> "!=" -- Deprecated -ne maps to !=
  IsFile -> "-f"
  IsDir -> "-d"
  IsSymlink -> "-L" -- Fish uses -L for symlink check
  Exists -> "-e"
  IsReadable -> "-r"
  IsWritable -> "-w"
  IsExecutable -> "-x"
  IsEmpty -> "-z"
  NotEmpty -> "-n"
  -- These need specific command forms in Fish, `test` doesn't directly support them
  StringContains -> error "`test` cannot directly print StringContains" -- Requires `string match -q -- '*pattern*'`
  StringMatchRegex -> error "`test` cannot directly print StringMatchRegex" -- Requires `string match -qr`
  StringMatchGlob -> error "`test` cannot directly print StringMatchGlob" -- Requires `string match -q`

--------------------------------------------------------------------------------
-- Removed Freestanding helper
--------------------------------------------------------------------------------
-- getFreestandingArgumentList :: FreestandingArgumentList -> [FishExpr TStr]
-- getFreestandingArgumentList (FreestandingArgumentList xs) = xs
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
prettyFish = vsep . map prettyFishStatement . filter (not . isEmptyStatement)

-- | Render the Doc to final Text.
renderFish :: [FishStatement] -> Text
renderFish = renderStrict . layoutPretty defaultLayoutOptions . prettyFish

--------------------------------------------------------------------------------
-- 2. FishStatement
--------------------------------------------------------------------------------

prettyFishStatement :: FishStatement -> Doc ann
prettyFishStatement = \case
  Stmt cmd -> prettyFishCommand cmd
  StmtList stmts -> vsep (map prettyFishStatement (filter (not . isEmptyStatement) stmts))
  Comment txt -> "#" <> pretty txt -- Keep comments without leading space for simplicity
  EmptyStmt -> mempty

isEmptyStatement :: FishStatement -> Bool
isEmptyStatement = \case
  EmptyStmt -> True
  StmtList [] -> True
  _ -> False

--------------------------------------------------------------------------------
-- 3. FishCommand
--------------------------------------------------------------------------------

prettyFishCommand :: FishCommand t -> Doc ann
prettyFishCommand = \case
  Command txt args ->
    let argsDoc = hsep (map prettyExprOrRedirect args)
     in if null args then pretty txt else pretty txt <+> argsDoc
  Set flags var expr ->
    let parts = ["set"] ++ map prettySetFlag flags ++ [pretty var]
    in case expr of
         ExprListLiteral [] -> hsep parts
         _ -> hsep parts <+> prettyFishExpr expr
  Function fishFn ->
    prettyFunction fishFn
  For var listExpr body suffix ->
    "for" <+> pretty var <+> "in" <+> prettyFishExpr listExpr <> hardline <>
      indent 2 (vsep (map prettyFishStatement (NE.toList body))) <> hardline <>
      "end" <> prettyBlockSuffix suffix
  While cond body suffix ->
    "while" <+> align (prettyJobList cond) <> hardline <>
      indent 2 (vsep (map prettyFishStatement (NE.toList body))) <> hardline <>
      "end" <> prettyBlockSuffix suffix
  Begin stmts suffix ->
    "begin" <> hardline <> indent 2 (vsep (map prettyFishStatement (NE.toList stmts)))
      <> hardline <> "end" <> prettyBlockSuffix suffix
  If cond thn els suffix ->
    "if" <+> align (prettyJobList cond) <> hardline <>
      indent 2 (vsep (map prettyFishStatement (NE.toList thn))) <>
      (if null els then mempty
       else hardline <> "else" <> hardline <> indent 2 (vsep (map prettyFishStatement els))) <>
      hardline <> "end" <> prettyBlockSuffix suffix
  Switch expr cases suffix ->
    "switch" <+> prettyFishExpr expr <> hardline <>
      indent 2 (vsep (map prettyCaseItem (NE.toList cases))) <> hardline <>
      "end" <> prettyBlockSuffix suffix
  Break ->
    "break"
  Continue ->
    "continue"
  Return mexpr -> -- Optional status expression
    case mexpr of
      Nothing -> "return"
      Just e  -> "return" <+> prettyFishExpr e
  Exit mx -> case mx of
    Nothing -> "exit"
    Just e  -> "exit" <+> prettyFishExpr e
  Source fileExpr -> "source" <+> prettyFishExpr fileExpr
  Eval e -> "eval" <+> prettyFishExpr e
  -- Heredocs are not supported in fish; removed
  Read flags vars ->
    "read" <+> hsep (map prettyReadFlag flags) <+> hsep (map pretty vars)
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
  Wait mp -> case mp of
    Nothing -> "wait"
    Just p  -> "wait" <+> prettyFishExpr p
  Exec c args -> "exec" <+> prettyFishExpr c <+> hsep (map prettyExprOrRedirect args)
  Decorated dec cmd ->
    prettyDecoration dec <+> prettyFishCommand cmd

prettyBlockSuffix :: [ExprOrRedirect] -> Doc ann
prettyBlockSuffix [] = mempty
prettyBlockSuffix xs = space <> hsep (map prettyExprOrRedirect xs)

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
prettyJobConjunction (FishJobConjunction mdec job conts) =
  let headDoc = case mdec of
                  Nothing -> prettyJobPipeline job
                  Just d  -> prettyConjunction d <+> prettyJobPipeline job
      rest = map prettyJCont conts
   in headDoc <> mconcat rest
  where
    prettyJCont = \case
      JCAnd jp -> space <> hardline <> prettyConjunction ConjAnd <+> prettyJobPipeline jp
      JCOr jp  -> space <> hardline <> prettyConjunction ConjOr  <+> prettyJobPipeline jp

prettyJobList :: FishJobList -> Doc ann
prettyJobList (FishJobList jobs) = vsep (map prettyJobConjunction (NE.toList jobs))

prettyConjunction :: Conjunction -> Doc ann
prettyConjunction = \case
  ConjAnd -> "and"
  ConjOr  -> "or"

prettyFunction :: FishFunction -> Doc ann
prettyFunction (FishFunction name flags params body) =
  let parts = ["function", pretty name]
              <> map prettyFunctionFlag flags
              <> (if null params then [] else ["--argument-names", hsep (map pretty params)])
  in hsep parts
      <> hardline <> indent 2 (vsep (map prettyFishStatement (NE.toList body)))
      <> hardline <> "end"

prettyFunctionFlag :: FunctionFlag -> Doc ann
prettyFunctionFlag = \case
  FuncDescription txt -> "--description" <+> escapeFishString txt -- Escape description
  FuncOnEvent evt -> "--on-event" <+> pretty evt
  FuncOnVariable var -> "--on-variable" <+> pretty var
  FuncOnJobExit job -> "--on-job-exit" <+> pretty job
  FuncOnProcessExit pid -> "--on-process-exit" <+> pretty pid
  FuncWraps cmd -> "--wraps" <+> pretty cmd
  FuncHelp -> "--help"
  FuncInheritVariable -> "--inherit-variable"
  FuncUnknownFlag txt -> pretty txt -- Assume flags are safe or already formatted

--------------------------------------------------------------------------------
-- 5. ExprOrRedirect (Replaces ArgOrRedirect)
--------------------------------------------------------------------------------

prettyExprOrRedirect :: ExprOrRedirect -> Doc ann
prettyExprOrRedirect = \case
  ExprVal expr -> prettyFishExpr expr -- Use prettyFishExpr
  RedirectVal redir -> prettyRedirect redir

prettyVarRef :: FishVarRef t -> Doc ann
prettyVarRef = \case
  VarAll name -> "$" <> pretty name
  VarScalar name -> "$" <> pretty name
  VarIndex name idx -> "$" <> pretty name <> brackets (prettyFishIndex idx)

--------------------------------------------------------------------------------
-- 6. Expressions (Replaces Arguments)
--------------------------------------------------------------------------------

prettyFishExpr :: FishExpr t -> Doc ann
prettyFishExpr = \case
  ExprLiteral txt -> escapeFishString txt
  ExprNumLiteral i -> pretty i
  ExprVariable varRef -> prettyVarRef varRef
  ExprSpecialVar sv -> prettySpecialVar sv
  ExprStringConcat e1 e2 -> prettyFishExpr e1 <> prettyFishExpr e2 -- Concatenation is implicit
  ExprStringOp op e -> parens (prettyStringOp op <+> prettyFishExpr e)
  ExprJoinList e -> parens ("string join" <+> escapeFishString " " <+> prettyFishExpr e)
  ExprMath args ->
    parens ("math" <+> hsep (map prettyFishExpr (NE.toList args)))
  ExprCommandSubst stmts ->
    "(" <> align (vsep (map prettyFishStatement (NE.toList stmts))) <> ")"
  ExprListLiteral [] -> mempty
  ExprListLiteral xs -> hsep (map prettyFishExpr xs)
  ExprListConcat a b -> prettyFishExpr a <+> prettyFishExpr b
  ExprGlob g -> prettyGlob g
  ExprProcessSubst stmts -> prettyProcessSubst stmts


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
-- 7. Indexing, CaseItem, Redirection, and Decoration
--------------------------------------------------------------------------------

prettyFishIndex :: FishIndex a b -> Doc ann
prettyFishIndex = \case
  IndexSingle e -> prettyFishExpr e
  IndexRange e1 e2 ->
    let start = maybe mempty prettyFishExpr e1
        end = maybe mempty prettyFishExpr e2
    in start <> ".." <> end
  IndexList xs -> hsep (map prettyFishExpr (NE.toList xs))

prettyCaseItem :: CaseItem -> Doc ann
prettyCaseItem (CaseItem pats body) = -- Use NonEmpty patterns/body
  "case" <+> hsep (punctuate " |" (map prettyFishExpr (NE.toList pats))) <> hardline <> -- Patterns separated by |
    indent 2 (vsep (map prettyFishStatement (NE.toList body)))

prettyRedirect :: Redirect -> Doc ann
prettyRedirect redir =
  case redir of
    Redirect RedirectBoth op target@(RedirectFile _)
      | op `elem` [RedirectOut, RedirectOutAppend, RedirectClobber] ->
          prettyRedirectSource RedirectStdout <> prettyRedirectOp op <> prettyRedirectTarget target <+> "2>&1"
    Redirect src op target ->
      prettyRedirectSource src <> prettyRedirectOp op <> prettyRedirectTarget target

prettyRedirectSource :: RedirectSource -> Doc ann
prettyRedirectSource = \case
  RedirectStdout -> mempty
  RedirectStderr -> "2"
  RedirectStdin -> mempty
  RedirectBoth -> "&"
  RedirectFD n -> pretty n

prettyRedirectOp :: RedirectOp -> Doc ann
prettyRedirectOp = \case
  RedirectOut -> ">"
  RedirectOutAppend -> ">>"
  RedirectIn -> "<"
  RedirectClobber -> ">|"
  RedirectReadWrite -> "<>"

prettyRedirectTarget :: RedirectTarget -> Doc ann
prettyRedirectTarget = \case
  RedirectFile expr -> space <> prettyFishExpr expr
  RedirectTargetFD n -> "&" <> pretty n
  RedirectClose -> "&-"

prettyDecoration :: Decoration -> Doc ann
prettyDecoration = \case
  DecBuiltin -> "builtin"
  DecCommand -> "command"
  DecExec -> "exec"

prettyReadFlag :: ReadFlag -> Doc ann
prettyReadFlag = \case
  ReadPrompt t -> "--prompt" <+> escapeFishString t
  ReadLocal -> "--local"
  ReadGlobal -> "--global"
  ReadUniversal -> "--universal"
  ReadExport -> "--export"
  ReadArray -> "--array"

prettySetFlag :: SetFlag -> Doc ann
prettySetFlag = \case
  SetLocal -> "--local"
  SetGlobal -> "--global"
  SetUniversal -> "--universal"
  SetExport -> "--export"
  SetUnexport -> "--unexport"
  SetAppend -> "--append"
  SetPrepend -> "--prepend"
  SetErase -> "--erase"
  SetPath -> "--path"
  SetQuery -> "--query"

prettyGlob :: GlobPattern -> Doc ann
prettyGlob (GlobPattern parts) = hcat (map prettyGlobPart parts)

prettyGlobPart :: GlobPart -> Doc ann
prettyGlobPart = \case
  GlobLiteral txt -> pretty txt
  GlobStar -> "*"
  GlobStarStar -> "**"
  GlobQuestion -> "?"
  GlobCharClass pat -> brackets (pretty pat)
  GlobBraces xs -> braces (hcat (punctuate "," (map pretty (NE.toList xs))))

prettyStringOp :: StringOp -> Doc ann
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

prettySpecialVar :: SpecialVarRef t -> Doc ann
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

prettyProcessSubst :: NE.NonEmpty FishStatement -> Doc ann
prettyProcessSubst stmts =
  let docBody = case NE.toList stmts of
        [s] -> prettyFishStatement s
        xs  -> "begin" <> hardline <> indent 2 (vsep (map prettyFishStatement xs)) <> hardline <> "end"
  in parens (docBody <+> "|" <+> "psub")

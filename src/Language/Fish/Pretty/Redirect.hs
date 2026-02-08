{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Pretty.Redirect
  ( prettyExprOrRedirectWith,
    prettyRedirectWith,
    prettyDecoration,
    prettyReadFlag,
    prettySetFlag,
  )
where

import Language.Fish.AST
import Language.Fish.Pretty.Expr (escapeFishString)
import Prettyprinter

prettyExprOrRedirectWith :: (forall t. FishExpr t -> Doc ann) -> ExprOrRedirect -> Doc ann
prettyExprOrRedirectWith prettyExpr = \case
  ExprVal expr -> prettyExpr expr
  RedirectVal redir -> prettyRedirectWith prettyExpr redir

prettyRedirectWith :: (forall t. FishExpr t -> Doc ann) -> Redirect -> Doc ann
prettyRedirectWith prettyExpr redir =
  case redir of
    Redirect RedirectBoth op target@(RedirectFile _)
      | op `elem` [RedirectOut, RedirectOutAppend, RedirectClobber] ->
          prettyRedirectSource RedirectStdout <> prettyRedirectOp op <> prettyRedirectTarget target <+> "2>&1"
    Redirect src op target ->
      prettyRedirectSource src <> prettyRedirectOp op <> prettyRedirectTarget target
  where
    prettyRedirectTarget = \case
      RedirectFile expr -> space <> prettyExpr expr
      RedirectTargetFD n -> "&" <> pretty n
      RedirectClose -> "&-"

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
  ReadNChars n -> "--nchars" <+> pretty n
  ReadTimeout t -> "--timeout" <+> pretty t
  ReadFD fd -> "--fd" <+> pretty fd

prettySetFlag :: SetFlag -> Doc ann
prettySetFlag = \case
  SetLocal -> "--local"
  SetFunction -> "--function"
  SetGlobal -> "--global"
  SetUniversal -> "--universal"
  SetExport -> "--export"
  SetUnexport -> "--unexport"
  SetAppend -> "--append"
  SetPrepend -> "--prepend"
  SetErase -> "--erase"
  SetPath -> "--path"
  SetQuery -> "--query"

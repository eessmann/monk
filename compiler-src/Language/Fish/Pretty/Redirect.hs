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
prettyRedirectWith prettyExpr = \case
  FileRedirect fd mode path -> descriptor (if mode == InputFile then ReadFrom else WriteTo) fd <> fileMode mode <+> prettyExpr path
  BothFileRedirect mode path -> outputMode mode <+> prettyExpr path <+> "2>&1"
  DuplicateRedirect fd direction target -> descriptor direction fd <> directionOp direction <> "&" <> pretty target
  CloseRedirect fd direction -> descriptor direction fd <> directionOp direction <> "&-"
  where
    descriptor ReadFrom 0 = mempty
    descriptor WriteTo 1 = mempty
    descriptor _ fd = pretty fd
    directionOp ReadFrom = "<"
    directionOp WriteTo = ">"
    fileMode = \case
      OverwriteFile -> ">"
      AppendFile -> ">>"
      InputFile -> "<"
      ClobberFile -> ">|"
    outputMode = \case
      OutputOverwrite -> ">"
      OutputAppend -> ">>"
      OutputClobber -> ">|"

prettyDecoration :: Decoration -> Doc ann
prettyDecoration = \case
  DecBuiltin -> "builtin"
  DecCommand -> "command"

prettyReadFlag :: ReadFlag -> Doc ann
prettyReadFlag = \case
  ReadPrompt t -> "--prompt" <+> escapeFishString t
  ReadLocal -> "--local"
  ReadGlobal -> "--global"
  ReadUniversal -> "--universal"
  ReadExport -> "--export"
  ReadSilent -> "--silent"
  ReadArray -> "--array"
  ReadNull -> "--null"
  ReadDelimiter t -> "--delimiter" <+> escapeFishString t
  ReadNChars n -> "--nchars" <+> escapeFishString n
  ReadTimeout t -> "--timeout" <+> escapeFishString t
  ReadFD fd -> "-u" <+> escapeFishString fd

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
  SetUnpath -> "--unpath"
  SetQuery -> "--query"

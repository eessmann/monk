{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Fish.Pretty.Command
  ( prettyFishCommandWith,
  )
where

import Data.List.NonEmpty qualified as NE
import Language.Fish.AST
import Language.Fish.Pretty.Expr (escapeFishString, prettyFishExprWith)
import Language.Fish.Pretty.Job
  ( prettyJobConjunctionWith,
    prettyJobListWith,
    prettyJobPipelineWith,
  )
import Language.Fish.Pretty.Pattern (prettyCaseItemWith)
import Language.Fish.Pretty.Redirect
  ( prettyDecoration,
    prettyExprOrRedirectWith,
    prettyReadFlag,
    prettySetFlag,
  )
import Prettyprinter

prettyFishCommandWith :: forall ann t. (FishStatement -> Doc ann) -> FishCommand t -> Doc ann
prettyFishCommandWith prettyStmt = \case
  Command txt args ->
    let argsDoc = hsep (map prettyExprOrRedirect args)
     in if null args then pretty txt else pretty txt <+> argsDoc
  Set flags var expr ->
    let parts = ["set"] ++ map prettySetFlag flags ++ [pretty var]
     in case expr of
          ExprListLiteral [] -> hsep parts
          _ -> hsep parts <+> prettyExpr expr
  Function fishFn ->
    prettyFunction fishFn
  For var listExpr body suffix ->
    "for"
      <+> pretty var
      <+> "in"
      <+> prettyExpr listExpr
      <> hardline
      <> indent 2 (vsep (map prettyStmt (NE.toList body)))
      <> hardline
      <> "end"
      <> prettyBlockSuffix suffix
  While cond body suffix ->
    "while"
      <+> align (prettyJobList cond)
      <> hardline
      <> indent 2 (vsep (map prettyStmt (NE.toList body)))
      <> hardline
      <> "end"
      <> prettyBlockSuffix suffix
  Begin stmts suffix ->
    "begin"
      <> hardline
      <> indent 2 (vsep (map prettyStmt (NE.toList stmts)))
      <> hardline
      <> "end"
      <> prettyBlockSuffix suffix
  If cond thn els suffix ->
    "if"
      <+> align (prettyJobList cond)
      <> hardline
      <> indent 2 (vsep (map prettyStmt (NE.toList thn)))
      <> ( if null els
             then mempty
             else hardline <> "else" <> hardline <> indent 2 (vsep (map prettyStmt els))
         )
      <> hardline
      <> "end"
      <> prettyBlockSuffix suffix
  Switch expr cases suffix ->
    "switch"
      <+> prettyExpr expr
      <> hardline
      <> indent 2 (vsep (map prettyCaseItem (NE.toList cases)))
      <> hardline
      <> "end"
      <> prettyBlockSuffix suffix
  Break ->
    "break"
  Continue ->
    "continue"
  Return mexpr ->
    case mexpr of
      Nothing -> "return"
      Just e -> "return" <+> prettyExpr e
  Exit mx -> case mx of
    Nothing -> "exit"
    Just e -> "exit" <+> prettyExpr e
  Source fileExpr -> "source" <+> prettyExpr fileExpr
  Eval e -> "eval" <+> prettyExpr e
  Read flags vars ->
    "read" <+> hsep (map prettyReadFlag flags) <+> hsep (map pretty vars)
  Echo args ->
    "echo" <+> hsep (map prettyExpr (NE.toList args))
  Printf fmt args ->
    "printf" <+> prettyExpr fmt <+> hsep (map prettyExpr args)
  Pipeline jp -> prettyJobPipeline jp
  JobConj jc -> prettyJobConjunction jc
  Semicolon cmd1 cmd2 ->
    prettyFishCommandWith prettyStmt cmd1 <> ";" <> line <> prettyFishCommandWith prettyStmt cmd2
  Not cmd ->
    "not" <+> prettyFishCommandWith prettyStmt cmd
  Background cmd ->
    prettyFishCommandWith prettyStmt cmd <+> "&"
  Wait mp -> case mp of
    Nothing -> "wait"
    Just p -> "wait" <+> prettyExpr p
  Exec c args -> "exec" <+> prettyExpr c <+> hsep (map prettyExprOrRedirect args)
  Decorated dec cmd ->
    prettyDecoration dec <+> prettyFishCommandWith prettyStmt cmd
  where
    prettyExpr :: forall t1. FishExpr t1 -> Doc ann
    prettyExpr = prettyFishExprWith prettyStmt

    prettyExprOrRedirect :: ExprOrRedirect -> Doc ann
    prettyExprOrRedirect = prettyExprOrRedirectWith prettyExpr
    prettyJobPipeline = prettyJobPipelineWith prettyStmt prettyExpr
    prettyJobConjunction = prettyJobConjunctionWith prettyStmt prettyExpr
    prettyJobList = prettyJobListWith prettyStmt prettyExpr
    prettyCaseItem = prettyCaseItemWith prettyStmt prettyExpr

    prettyBlockSuffix [] = mempty
    prettyBlockSuffix xs = space <> hsep (map prettyExprOrRedirect xs)

    prettyFunction (FishFunction name flags params body) =
      let parts =
            ["function", pretty name]
              <> map prettyFunctionFlag flags
              <> (if null params then [] else ["--argument-names", hsep (map pretty params)])
       in hsep parts
            <> hardline
            <> indent 2 (vsep (map prettyStmt (NE.toList body)))
            <> hardline
            <> "end"

    prettyFunctionFlag = \case
      FuncDescription txt -> "--description" <+> escapeFishString txt
      FuncOnEvent evt -> "--on-event" <+> pretty evt
      FuncOnVariable var -> "--on-variable" <+> pretty var
      FuncOnJobExit job -> "--on-job-exit" <+> pretty job
      FuncOnProcessExit pid -> "--on-process-exit" <+> pretty pid
      FuncWraps cmd -> "--wraps" <+> pretty cmd
      FuncHelp -> "--help"
      FuncInheritVariable -> "--inherit-variable"
      FuncUnknownFlag txt -> pretty txt

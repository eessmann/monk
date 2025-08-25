{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator
  ( translateRoot
  , translateToken
  ) where

import Prelude hiding (show)
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import Language.Fish.AST
import ShellCheck.AST
import GHC.Show (show)
import Language.Fish.Translator.Variables
import Language.Fish.Translator.Commands
import qualified Language.Fish.Translator.Control as Control
import qualified Language.Fish.Translator.IO as FIO

--------------------------------------------------------------------------------
-- 1. Main translation functions
--------------------------------------------------------------------------------

translateRoot :: Root -> FishStatement
translateRoot (Root topToken) = translateToken topToken

-- | Dispatch on a ShellCheck Token to produce a FishStatement.
translateToken :: Token -> FishStatement
translateToken token =
  case token of
    T_Script _ _ stmts -> wrapStmtList (map translateToken stmts)

    T_SimpleCommand _ assignments cmdToks -> translateSimpleCommand assignments cmdToks

    T_Pipeline _ bang cmds -> FIO.translatePipeline bang cmds

    T_IfExpression _ conditionBranches elseBranch ->
      translateIfExpression conditionBranches elseBranch

    T_WhileExpression _ cond body ->
      case Control.toNonEmptyStmtList (map translateToken body) of
        Just neBody -> Stmt (While (Control.translateBoolTokens cond) neBody)
        Nothing     -> Comment "Skipped empty while loop body"

    T_UntilExpression _ cond body ->
      case Control.toNonEmptyStmtList (map translateToken body) of
        Just neBody -> Stmt (While (Control.negateBoolExpr (Control.translateBoolTokens cond)) neBody)
        Nothing     -> Comment "Skipped empty until loop body"

    T_Function _ _ _ funcName body -> Control.translateFunction funcName body

    T_BraceGroup _ tokens ->
      case Control.toNonEmptyStmtList (map translateToken tokens) of
         Just neBody -> BraceStmt neBody []
         Nothing     -> Comment "Skipped empty brace group"

    T_Subshell _ tokens ->
      case Control.toNonEmptyStmtList (map translateToken tokens) of
         Just neBody -> Stmt (Begin neBody)
         Nothing     -> Comment "Skipped empty subshell"

    T_AndIf _ l r ->
      let lp = FIO.pipelineOf (translateTokenToStatusCmd l)
          rp = FIO.pipelineOf (translateTokenToStatusCmd r)
       in Stmt (JobConj (FishJobConjunction Nothing lp [JCAnd rp] False))
    T_OrIf _ l r ->
      let lp = FIO.pipelineOf (translateTokenToStatusCmd l)
          rp = FIO.pipelineOf (translateTokenToStatusCmd r)
       in Stmt (JobConj (FishJobConjunction Nothing lp [JCOr rp] False))

    T_Backgrounded _ bgToken -> Stmt (Background (translateTokenToStatusCmd bgToken))

    T_Annotation _ _ inner -> translateToken inner

    T_ForIn _ var tokens body ->
      case (NE.nonEmpty (map translateTokenToExpr tokens), Control.toNonEmptyStmtList (map translateToken body)) of
        (Just neArgs, Just neBody) -> Stmt (For (T.pack var) neArgs neBody)
        _ -> Comment "Skipped empty for loop body or list"

    T_CaseExpression _ switchExpr cases -> Control.translateCaseExpression switchExpr cases

    T_CoProc {} -> Comment "Unsupported: Coprocess (coproc)"

    _ -> Comment ("Skipped token at statement level: " <> T.pack (show token))

wrapStmtList :: [FishStatement] -> FishStatement
wrapStmtList [stmt] = stmt
wrapStmtList stmts  = StmtList stmts
--------------------------------------------------------------------------------
-- 2. Helpers
--------------------------------------------------------------------------------

wrapStmtList :: [FishStatement] -> FishStatement
wrapStmtList [stmt] = stmt
wrapStmtList stmts  = StmtList stmts

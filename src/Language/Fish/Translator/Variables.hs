{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Variables
  ( translateTokenToExpr
  , translateTokenToExprOrRedirect
  , translateAssignment
  , translateArithmetic
  , tokenToLiteralText
  ) where

import Relude
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Language.Fish.AST
import ShellCheck.AST
import ShellCheck.ASTLib (getLiteralStringDef)

-- | Utility: get literal text from a token if available
tokenToLiteralText :: Token -> Text
tokenToLiteralText = T.pack . getLiteralStringDef ""

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

translateTokenToExpr :: Token -> FishExpr TStr
translateTokenToExpr = \case
  T_Literal _ s -> ExprLiteral (T.pack s)
  T_SingleQuoted _ s -> ExprLiteral (T.pack s)
  T_DoubleQuoted _ parts -> ExprLiteral (T.concat (map tokenToLiteralText parts))
  -- Arithmetic tokens: convert to a command substitution calling `math`
  T_Arithmetic _ exprTok -> ExprCommandSubstStr (Stmt (translateArithmetic exprTok) NE.:| [])
  -- Backticks and $(...) command substitutions
  T_Backticked _ stmts ->
    case NE.nonEmpty (map translateStmt stmts) of
      Just neBody -> ExprCommandSubstStr neBody
      Nothing     -> ExprLiteral ""
  T_DollarExpansion _ stmts ->
    case NE.nonEmpty (map translateStmt stmts) of
      Just neBody -> ExprCommandSubstStr neBody
      Nothing     -> ExprLiteral ""
  -- ${ ... $(cmd) ... } style
  T_DollarBraceCommandExpansion _ _ stmts ->
    case NE.nonEmpty (map translateStmt stmts) of
      Just neBody -> ExprCommandSubstStr neBody
      Nothing     -> ExprLiteral ""
  -- Fallback: take literal interpretation where possible
  other -> ExprLiteral (tokenToLiteralText other)
  where
    translateStmt = \t -> case t of
      T_Script _ _ ts -> StmtList (map translateStmt ts)
      _               -> Stmt (Command (tokenToLiteralText t) [])

translateTokenToExprOrRedirect :: Token -> ExprOrRedirect
translateTokenToExprOrRedirect tok = ExprVal (translateTokenToExpr tok)

--------------------------------------------------------------------------------
-- Assignments
--------------------------------------------------------------------------------

translateAssignment :: Token -> Maybe FishStatement
translateAssignment tok =
  case tok of
    T_Assignment _ _ var _ val ->
      let fishVar = T.pack var
          fishScope = ScopeLocal
          fishValExpr = translateTokenToExpr val
       in Just $ Stmt (Set fishScope fishVar fishValExpr)
    _ -> Nothing

--------------------------------------------------------------------------------
-- Arithmetic
--------------------------------------------------------------------------------

translateArithmetic :: Token -> FishCommand TStatus
translateArithmetic exprToken =
  let mathArg = ExprVal (translateTokenToExpr exprToken)
   in Command "math" [mathArg, RedirectVal RedirectOut (ExprLiteral "/dev/null")]


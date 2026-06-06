{-# LANGUAGE LambdaCase #-}

module Language.Fish.Translator.Syntax
  ( module Language.Fish.AST,
    DSL.Script,
    Arg,
    argExpr,
    argRedirect,
    renderArg,
    renderArgs,
    statementToScript,
    attachRedirectsToCommand,
    attachRedirectsToStatement,
  )
where

import Data.List.NonEmpty qualified as NE
import Language.Fish.AST
import Language.Fish.DSL.Internal
  ( Arg (UnsafeArgExpr, UnsafeArgRedirect),
    ArgumentType,
    Expr (UnsafeExpr),
    lowerArg,
  )
import Language.Fish.DSL.Internal qualified as DSL

argExpr :: (ArgumentType t, Typeable t) => FishExpr t -> Arg
argExpr = UnsafeArgExpr . UnsafeExpr

argRedirect :: Redirect -> Arg
argRedirect = UnsafeArgRedirect

renderArg :: Arg -> ExprOrRedirect
renderArg = lowerArg

renderArgs :: [Arg] -> [ExprOrRedirect]
renderArgs = map renderArg

statementToScript :: FishStatement -> DSL.Script
statementToScript = \case
  StmtList stmts -> DSL.UnsafeScript (map DSL.UnsafeStmt stmts)
  stmt -> DSL.UnsafeScript [DSL.UnsafeStmt stmt]

attachRedirectsToCommand :: [ExprOrRedirect] -> FishCommand TStatus -> FishCommand TStatus
attachRedirectsToCommand redirs cmd =
  case cmd of
    Command name args -> Command name (args ++ redirs)
    Exec target args -> Exec target (args ++ redirs)
    Begin body suffix -> Begin body (suffix ++ redirs)
    If cond thn els suffix -> If cond thn els (suffix ++ redirs)
    Switch expr cases suffix -> Switch expr cases (suffix ++ redirs)
    While cond body suffix -> While cond body (suffix ++ redirs)
    For var listExpr body suffix -> For var listExpr body (suffix ++ redirs)
    other ->
      case redirs of
        [] -> other
        _ -> Begin (Stmt other NE.:| []) redirs

attachRedirectsToStatement :: [ExprOrRedirect] -> FishStatement -> FishStatement
attachRedirectsToStatement redirs stmt =
  case stmt of
    Stmt (Command name args) -> Stmt (Command name (args ++ redirs))
    Stmt (Exec target args) -> Stmt (Exec target (args ++ redirs))
    Stmt (Begin body suffix) -> Stmt (Begin body (suffix ++ redirs))
    Stmt (If cond thn els suffix) -> Stmt (If cond thn els (suffix ++ redirs))
    Stmt (Switch expr cases suffix) -> Stmt (Switch expr cases (suffix ++ redirs))
    Stmt (While cond body suffix) -> Stmt (While cond body (suffix ++ redirs))
    Stmt (For var listExpr body suffix) -> Stmt (For var listExpr body (suffix ++ redirs))
    StmtList stmts ->
      case redirs of
        [] -> stmt
        _ ->
          case NE.nonEmpty stmts of
            Just body -> Stmt (Begin body redirs)
            Nothing -> Comment "Skipped empty redirection block"
    other ->
      case redirs of
        [] -> other
        _ -> Stmt (Begin (other NE.:| []) redirs)

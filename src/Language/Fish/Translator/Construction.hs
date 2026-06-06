{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Language.Fish.Translator.Construction
  ( argExpr,
    argRedirect,
    renderArg,
    renderArgs,
    statementToScript,
    attachRedirectsToCommand,
    attachRedirectsToStatement,
    toRawExpr,
    toRawArg,
    toRawStatusCommand,
    toRawUnitCommand,
    toRawStmt,
    toRawBlock,
    toRawPipeline,
    toRawScript,
  )
where

import Data.List.NonEmpty qualified as NE
import Language.Fish.DSL qualified as DSL
import Language.Fish.DSL.Internal qualified as DSLI
import Language.Fish.DSL.Lower qualified as Lower
import Language.Fish.Translator.Types

argExpr :: (DSL.ArgumentType t, Typeable t) => FishExpr t -> DSL.Arg
argExpr = DSLI.UnsafeArgExpr . DSLI.UnsafeExpr

argRedirect :: Redirect -> DSL.Arg
argRedirect = DSLI.UnsafeArgRedirect

renderArg :: DSL.Arg -> ExprOrRedirect
renderArg = Lower.lowerArg

renderArgs :: [DSL.Arg] -> [ExprOrRedirect]
renderArgs = map renderArg

statementToScript :: FishStatement -> DSL.Script
statementToScript = \case
  StmtList stmts -> DSLI.UnsafeScript (map DSLI.UnsafeStmt stmts)
  stmt -> DSLI.UnsafeScript [DSLI.UnsafeStmt stmt]

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

toRawExpr :: DSL.Expr t -> FishExpr t
toRawExpr = Lower.lowerExpr

toRawArg :: DSL.Arg -> ExprOrRedirect
toRawArg = Lower.lowerArg

toRawStatusCommand :: DSL.Command 'DSL.ReturnsStatus -> FishCommand TStatus
toRawStatusCommand = Lower.lowerCommand

toRawUnitCommand :: DSL.Command 'DSL.ReturnsUnit -> FishCommand TUnit
toRawUnitCommand = Lower.lowerCommand

toRawStmt :: DSL.Stmt -> FishStatement
toRawStmt = Lower.lowerStmt

toRawBlock :: DSL.Block -> NonEmpty FishStatement
toRawBlock = Lower.lowerBlock

toRawPipeline :: DSL.Pipeline -> FishJobPipeline
toRawPipeline = DSLI.lowerPipelineValue

toRawScript :: DSL.Script -> [FishStatement]
toRawScript = Lower.lowerScript

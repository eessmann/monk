{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Variables.Arithmetic.Assignments
  ( planUnaryOp,
    planAssignment,
  )
where

import Data.List.NonEmpty qualified as NE
import Language.Fish.AST
import Language.Fish.Translator.Hoist (Hoisted (..))
import Language.Fish.Translator.Hoist.Monad (HoistedM, hoistM)
import Language.Fish.Translator.Monad (unsupported)
import Language.Fish.Translator.Variables.Common (scopeFlagsForVarM)
import Language.Fish.Translator.Variables.Arithmetic.Helpers
  ( UnaryFixity (..),
    arithTempName,
    arithVarName,
    assignmentArgs,
    ensureArithArgs,
    mathSubstFromArgs,
    stripUnaryOp,
    wrapParens,
  )
import ShellCheck.AST

planUnaryOp ::
  (Token -> HoistedM [FishExpr TStr]) ->
  Token ->
  String ->
  Token ->
  HoistedM [FishExpr TStr]
planUnaryOp arithArgsPlan tok op inner = do
  let (opTxt, fixity) = stripUnaryOp op
  if opTxt == "++" || opTxt == "--"
    then do
      let delta = if opTxt == "++" then "+" else "-"
      Hoisted preInner _ <- arithArgsPlan inner
      case arithVarName inner of
        Just var -> do
          flags <- scopeFlagsForVarM var
          let args = ExprVariable (VarScalar var) NE.:| [ExprLiteral delta, ExprLiteral "1"]
              setStmt = Stmt (Set flags var (mathSubstFromArgs args))
          case fixity of
            UnaryPostfix -> do
              let tmp = arithTempName tok
                  saveStmt =
                    Stmt
                      (Set [SetLocal] tmp (ExprListLiteral [ExprVariable (VarScalar var)]))
              hoistM (preInner <> [saveStmt, setStmt]) [ExprVariable (VarScalar tmp)]
            _ ->
              hoistM (preInner <> [setStmt]) [ExprVariable (VarScalar var)]
        Nothing -> do
          unsupported "Arithmetic ++/-- with non-variable; side effects may be lost"
          Hoisted pre args <- arithArgsPlan inner
          let baseArgs = ensureArithArgs args
              valueArgs =
                case fixity of
                  UnaryPostfix -> baseArgs
                  _ -> wrapParens (baseArgs <> [ExprLiteral delta, ExprLiteral "1"])
          hoistM pre valueArgs
    else do
      Hoisted pre args <- arithArgsPlan inner
      hoistM pre (ExprLiteral opTxt : ensureArithArgs args)

planAssignment ::
  (Token -> HoistedM [FishExpr TStr]) ->
  String ->
  Token ->
  Token ->
  HoistedM [FishExpr TStr]
planAssignment arithArgsPlan op lhs rhs =
  case arithVarName lhs of
    Just var -> do
      Hoisted preR argsR <- arithArgsPlan rhs
      flags <- scopeFlagsForVarM var
      let rhsArgs = fromMaybe (ExprLiteral "0" NE.:| []) (NE.nonEmpty (ensureArithArgs argsR))
          args = assignmentArgs (toText op) var rhsArgs
          setStmt = Stmt (Set flags var (mathSubstFromArgs args))
      hoistM (preR <> [setStmt]) [ExprVariable (VarScalar var)]
    Nothing -> do
      unsupported "Arithmetic assignment with unsupported lvalue; side effects may be lost"
      arithArgsPlan rhs

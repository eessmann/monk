{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Variables.Arithmetic.Expr
  ( arithArgsPlan,
    arithArgsFromTokenList,
  )
where

import Data.List.NonEmpty qualified as NE
import Language.Fish.AST
import Language.Fish.Translator.Hoist (Hoisted (..))
import Language.Fish.Translator.Hoist.Monad (HoistedM, hoistM)
import Language.Fish.Translator.Token (tokenToLiteralText)
import Language.Fish.Translator.Variables.Arithmetic.Assignments
  ( planAssignment,
    planUnaryOp,
  )
import Language.Fish.Translator.Variables.Arithmetic.Helpers
  ( arithHasSideEffects,
    arithTempNameWith,
    ensureArithArgs,
    stripUnaryOpText,
    wrapParens,
  )
import Language.Fish.Translator.Variables.Arithmetic.ShortCircuit
  ( arithShortCircuitPlan,
    arithTernaryPlan,
  )
import Language.Fish.Translator.Variables.Common
  ( paramNameFrom,
    specialVarName,
  )
import ShellCheck.AST

arithArgsPlan :: Token -> HoistedM [FishExpr TStr]
arithArgsPlan = go
  where
    go :: Token -> HoistedM [FishExpr TStr]
    go tok =
      case tok of
        TA_Sequence _ ts -> concatPlans ts
        TA_Expansion _ ts -> concatPlans ts
        TA_Parenthesis _ t -> do
          Hoisted pre args <- go t
          hoistM pre (wrapParens (ensureArithArgs args))
        TA_Unary _ op inner -> planUnaryOp go tok op inner
        TA_Binary _ op l r -> do
          let opTxt = toText op
          if (opTxt == "&&" || opTxt == "||") && arithHasSideEffects r
            then arithShortCircuitPlan go tok opTxt l r
            else do
              Hoisted preL argsL <- go l
              Hoisted preR argsR <- go r
              hoistM
                (preL <> preR)
                ( wrapParens
                    ( ensureArithArgs argsL
                        <> [ExprLiteral opTxt]
                        <> ensureArithArgs argsR
                    )
                )
        TA_Trinary _ a b c -> do
          if arithHasSideEffects b || arithHasSideEffects c
            then arithTernaryPlan go tok a b c
            else do
              Hoisted preA argsA <- go a
              Hoisted preB argsB <- go b
              Hoisted preC argsC <- go c
              hoistM
                (preA <> preB <> preC)
                ( wrapParens
                    ( ensureArithArgs argsA
                        <> [ExprLiteral "?"]
                        <> ensureArithArgs argsB
                        <> [ExprLiteral ":"]
                        <> ensureArithArgs argsC
                    )
                )
        TA_Assignment _ op lhs rhs -> planAssignment go op lhs rhs
        paramTok@(T_ParamSubSpecialChar _ name) ->
          case name of
            "#" -> do
              let tmp = arithTempNameWith paramTok "argc"
                  countExpr =
                    ExprCommandSubst
                      ( Stmt
                          (Command "count" [ExprVal (ExprVariable (VarAll "argv"))])
                          NE.:| []
                      )
                  setStmt = Stmt (Set [SetLocal] tmp countExpr)
              hoistM [setStmt] [ExprVariable (VarScalar tmp)]
            _ ->
              let varName = specialVarName (toText name)
               in hoistM [] [ExprVariable (VarScalar varName)]
        bracedTok@(T_DollarBraced _ _ inner) ->
          case paramNameFrom inner of
            Just name
              | name == "#" -> do
                  let tmp = arithTempNameWith bracedTok "argc"
                      countExpr =
                        ExprCommandSubst
                          ( Stmt
                              (Command "count" [ExprVal (ExprVariable (VarAll "argv"))])
                              NE.:| []
                          )
                      setStmt = Stmt (Set [SetLocal] tmp countExpr)
                  hoistM [setStmt] [ExprVariable (VarScalar tmp)]
              | otherwise ->
                  let varName = specialVarName name
                   in hoistM [] [ExprVariable (VarScalar varName)]
            Nothing ->
              let txt = tokenToLiteralText inner
               in hoistM [] [ExprLiteral txt]
        TA_Variable _ name _ -> hoistM [] [ExprVariable (VarScalar (toText name))]
        T_Literal _ s -> hoistM [] [ExprLiteral (toText s)]
        T_SingleQuoted _ s -> hoistM [] [ExprLiteral (toText s)]
        other ->
          let txt = tokenToLiteralText other
           in hoistM [] [ExprLiteral txt]

    concatPlans ts = do
      parts <- mapM go ts
      let Hoisted pre argLists = sequenceA parts
      hoistM pre (concat argLists)

arithArgsFromTokenList :: Token -> [FishExpr TStr]
arithArgsFromTokenList = \case
  TA_Sequence _ ts -> concatMap arithArgsFromTokenList ts
  TA_Expansion _ ts -> concatMap arithArgsFromTokenList ts
  TA_Parenthesis _ t -> wrapParens (arithArgsFromTokenList t)
  TA_Unary _ op t -> ExprLiteral (stripUnaryOpText op) : arithArgsFromTokenList t
  TA_Binary _ op l r ->
    wrapParens (arithArgsFromTokenList l <> [ExprLiteral (toText op)] <> arithArgsFromTokenList r)
  TA_Trinary _ a b c ->
    wrapParens (arithArgsFromTokenList a <> [ExprLiteral "?"] <> arithArgsFromTokenList b <> [ExprLiteral ":"] <> arithArgsFromTokenList c)
  TA_Assignment _ op l r ->
    wrapParens (arithArgsFromTokenList l <> [ExprLiteral (toText op)] <> arithArgsFromTokenList r)
  TA_Variable _ name _ -> [ExprVariable (VarScalar (toText name))]
  T_Literal _ s -> [ExprLiteral (toText s)]
  T_SingleQuoted _ s -> [ExprLiteral (toText s)]
  tok ->
    let txt = tokenToLiteralText tok
     in [ExprLiteral txt]

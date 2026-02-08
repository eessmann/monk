{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Variables.Substitution
  ( commandSubstExprListWith,
    commandSubstExprStrWith,
    translateSubstTokenWith,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Language.Fish.AST
import Language.Fish.Translator.Cond (condFromTokenWith, condToCommand)
import Language.Fish.Translator.Hoist (beginIfNeeded)
import Language.Fish.Translator.Token (tokenHasExpansion, tokenToLiteralText)
import Language.Fish.Translator.Variables.Arithmetic (translateArithmetic)
import ShellCheck.AST

commandSubstExprListWith :: (Token -> FishStatement) -> [Token] -> FishExpr (TList TStr)
commandSubstExprListWith translateStmt stmts =
  case NE.nonEmpty (map translateStmt stmts) of
    Just neBody -> ExprCommandSubst neBody
    Nothing -> ExprListLiteral []

commandSubstExprStrWith :: (Token -> FishStatement) -> [Token] -> FishExpr TStr
commandSubstExprStrWith translateStmt stmts =
  case NE.nonEmpty (map translateStmt stmts) of
    Just neBody -> ExprJoinList (ExprCommandSubst neBody)
    Nothing -> ExprLiteral ""

translateSubstTokenWith ::
  ([SetFlag] -> Token -> [FishStatement]) ->
  (Token -> FishExpr TStr) ->
  (Token -> ExprOrRedirect) ->
  Token ->
  FishStatement
translateSubstTokenWith translateAssign translateExpr translateExprOrRedirect = go
  where
    go = \case
      T_Script _ _ ts -> StmtList (map go ts)
      T_SimpleCommand _ assignments cmdToks ->
        translateSubstSimpleCommand assignments cmdToks
      T_Pipeline _ bang cmds ->
        Stmt (translateSubstPipeline bang cmds)
      T_AndIf _ l r ->
        let lp = substPipelineOf (translateSubstStatusCmd l)
            rp = substPipelineOf (translateSubstStatusCmd r)
         in Stmt (JobConj (FishJobConjunction Nothing lp [JCAnd rp]))
      T_OrIf _ l r ->
        let lp = substPipelineOf (translateSubstStatusCmd l)
            rp = substPipelineOf (translateSubstStatusCmd r)
         in Stmt (JobConj (FishJobConjunction Nothing lp [JCOr rp]))
      T_Backgrounded _ tok ->
        Stmt (Background (translateSubstStatusCmd tok))
      T_BraceGroup _ tokens ->
        substBegin tokens
      T_Subshell _ tokens ->
        substBegin tokens
      T_Redirecting _ _ inner ->
        go inner
      T_Arithmetic _ exprTok ->
        Stmt (translateArithmetic exprTok)
      T_Condition _ _ condTok ->
        Stmt (translateSubstConditionToken condTok)
      other ->
        Stmt (Command (tokenToLiteralText other) [])

    substBegin tokens =
      case NE.nonEmpty (map go tokens) of
        Just body -> Stmt (Begin body [])
        Nothing -> Comment "Skipped empty substitution block"

    translateSubstSimpleCommand assignments cmdToks =
      let envFlags = [SetLocal, SetExport]
          envAssigns = concatMap (translateAssign envFlags) assignments
          cmd = translateSubstCommandTokens cmdToks
       in case (envAssigns, cmd) of
            ([], Just fishCmd) -> Stmt fishCmd
            ([], Nothing) -> Comment "Skipped empty command in substitution"
            (_, Just fishCmd) ->
              Stmt (beginIfNeeded envAssigns fishCmd)
            (_, Nothing) ->
              case NE.nonEmpty envAssigns of
                Just body -> Stmt (Begin body [])
                Nothing -> Comment "Skipped empty command in substitution"

    translateSubstCommandTokens cmdTokens =
      case cmdTokens of
        [] -> Nothing
        (c : args) ->
          let name = tokenToLiteralText c
              argExprs = map translateExprOrRedirect args
           in if T.null name
                then Nothing
                else Just (Command name argExprs)

    translateSubstStatusCmd tok =
      case tok of
        T_SimpleCommand _ assignments cmdToks ->
          translateSubstCommandTokensToStatus assignments cmdToks
        T_Pipeline _ bang cmds ->
          translateSubstPipeline bang cmds
        T_Condition _ _ condTok ->
          translateSubstConditionToken condTok
        T_Redirecting _ _ inner ->
          translateSubstStatusCmd inner
        T_AndIf _ l r ->
          let lp = substPipelineOf (translateSubstStatusCmd l)
              rp = substPipelineOf (translateSubstStatusCmd r)
           in JobConj (FishJobConjunction Nothing lp [JCAnd rp])
        T_OrIf _ l r ->
          let lp = substPipelineOf (translateSubstStatusCmd l)
              rp = substPipelineOf (translateSubstStatusCmd r)
           in JobConj (FishJobConjunction Nothing lp [JCOr rp])
        _ -> Command "true" []

    translateSubstCommandTokensToStatus assignments cmdTokens =
      let baseCmd = fromMaybe (Command "true" []) (translateSubstCommandTokens cmdTokens)
       in if null assignments
            then baseCmd
            else
              let envFlags = [SetLocal, SetExport]
                  envAssigns = concatMap (translateAssign envFlags) assignments
               in beginIfNeeded envAssigns baseCmd

    translateSubstPipeline bang cmds =
      case mapMaybe translateSubstTokenToMaybeStatusCmd cmds of
        [] -> Command "true" []
        (c : cs) ->
          let pipe = Pipeline (substJobPipelineFromList (c : cs))
           in if hasBang bang then Not pipe else pipe

    translateSubstTokenToMaybeStatusCmd token =
      case token of
        T_SimpleCommand _ assignments rest ->
          Just (translateSubstCommandTokensToStatus assignments rest)
        T_Condition {} -> Just (translateSubstStatusCmd token)
        T_Redirecting _ _ inner -> translateSubstTokenToMaybeStatusCmd inner
        T_Pipeline _ bang cmds -> Just (translateSubstPipeline bang cmds)
        T_AndIf _ l r ->
          let lp = substPipelineOf (translateSubstStatusCmd l)
              rp = substPipelineOf (translateSubstStatusCmd r)
           in Just (JobConj (FishJobConjunction Nothing lp [JCAnd rp]))
        T_OrIf _ l r ->
          let lp = substPipelineOf (translateSubstStatusCmd l)
              rp = substPipelineOf (translateSubstStatusCmd r)
           in Just (JobConj (FishJobConjunction Nothing lp [JCOr rp]))
        _ -> Nothing

    hasBang = any (\tok -> tokenToLiteralText tok == "!")

    substJobPipelineFromList [] = substPipelineOf (Command "true" [])
    substJobPipelineFromList (c : cs) =
      FishJobPipeline
        { jpTime = False,
          jpVariables = [],
          jpStatement = Stmt c,
          jpCont = map (\cmd -> PipeTo {jpcVariables = [], jpcStatement = Stmt cmd}) cs,
          jpBackgrounded = False
        }

    substPipelineOf cmd =
      FishJobPipeline {jpTime = False, jpVariables = [], jpStatement = Stmt cmd, jpCont = [], jpBackgrounded = False}

    translateSubstConditionToken tok =
      condToCommand (condFromTokenWith translateExpr regexExpr literalExpr tok)
      where
        regexExpr t =
          if tokenHasExpansion t
            then translateExpr t
            else ExprLiteral (tokenToLiteralText t)
        literalExpr = ExprLiteral . tokenToLiteralText

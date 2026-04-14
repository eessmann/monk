{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Variables.Substitution
  ( commandSubstExprListWith,
    commandSubstExprListMWith,
    commandSubstExprStrWith,
    commandSubstExprStrMWith,
    translateSubstTokenWith,
    translateSubstTokenMWith,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Language.Fish.AST
import Language.Fish.Translator.Cond
  ( condFromTokenMWith,
    condFromTokenWith,
    condToCommand,
  )
import Language.Fish.Translator.Hoist (Hoisted (..), beginIfNeeded)
import Language.Fish.Translator.Hoist.Monad (HoistedM, hoistM)
import Language.Fish.Translator.Monad (TranslateM, withCommandSubstScope)
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

commandSubstExprListMWith :: (Token -> TranslateM FishStatement) -> [Token] -> TranslateM (FishExpr (TList TStr))
commandSubstExprListMWith translateStmt stmts =
  withCommandSubstScope $ do
    body <- mapM translateStmt stmts
    pure $
      case NE.nonEmpty body of
        Just neBody -> ExprCommandSubst neBody
        Nothing -> ExprListLiteral []

commandSubstExprStrMWith :: (Token -> TranslateM FishStatement) -> [Token] -> TranslateM (FishExpr TStr)
commandSubstExprStrMWith translateStmt stmts =
  withCommandSubstScope $ do
    body <- mapM translateStmt stmts
    pure $
      case NE.nonEmpty body of
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

translateSubstTokenMWith ::
  ([SetFlag] -> Token -> TranslateM [FishStatement]) ->
  (Token -> HoistedM (FishExpr TStr)) ->
  (Token -> HoistedM ExprOrRedirect) ->
  Token ->
  TranslateM FishStatement
translateSubstTokenMWith translateAssignM translateExprM translateExprOrRedirectM = go
  where
    go = \case
      T_Script _ _ ts -> StmtList <$> mapM go ts
      T_SimpleCommand _ assignments cmdToks ->
        translateSubstSimpleCommandM assignments cmdToks
      T_Pipeline _ bang cmds ->
        Stmt <$> translateSubstPipelineM bang cmds
      T_AndIf _ l r -> do
        lp <- substPipelineOf <$> translateSubstStatusCmdM l
        rp <- substPipelineOf <$> translateSubstStatusCmdM r
        pure (Stmt (JobConj (FishJobConjunction Nothing lp [JCAnd rp])))
      T_OrIf _ l r -> do
        lp <- substPipelineOf <$> translateSubstStatusCmdM l
        rp <- substPipelineOf <$> translateSubstStatusCmdM r
        pure (Stmt (JobConj (FishJobConjunction Nothing lp [JCOr rp])))
      T_Backgrounded _ tok ->
        Stmt . Background <$> translateSubstStatusCmdM tok
      T_BraceGroup _ tokens ->
        substBeginM tokens
      T_Subshell _ tokens ->
        substBeginM tokens
      T_Redirecting _ _ inner ->
        go inner
      T_Arithmetic _ exprTok ->
        pure (Stmt (translateArithmetic exprTok))
      T_Condition _ _ condTok ->
        Stmt <$> translateSubstConditionTokenM condTok
      other ->
        pure (Stmt (Command (tokenToLiteralText other) []))

    substBeginM tokens = do
      body <- mapM go tokens
      pure $
        case NE.nonEmpty body of
          Just neBody -> Stmt (Begin neBody [])
          Nothing -> Comment "Skipped empty substitution block"

    translateSubstSimpleCommandM assignments cmdToks = do
      let envFlags = [SetLocal, SetExport]
      envAssigns <- fmap concat (mapM (translateAssignM envFlags) assignments)
      cmd <- translateSubstCommandTokensM cmdToks
      pure $
        case (envAssigns, cmd) of
          ([], Just fishCmd) -> Stmt fishCmd
          ([], Nothing) -> Comment "Skipped empty command in substitution"
          (_, Just fishCmd) ->
            Stmt (beginIfNeeded envAssigns fishCmd)
          (_, Nothing) ->
            case NE.nonEmpty envAssigns of
              Just body -> Stmt (Begin body [])
              Nothing -> Comment "Skipped empty command in substitution"

    translateSubstCommandTokensM cmdTokens =
      case cmdTokens of
        [] -> pure Nothing
        (c : args) -> do
          translatedArgs <- mapM translateExprOrRedirectM args
          let Hoisted preArgs argExprs = sequenceA translatedArgs
              name = tokenToLiteralText c
          pure $
            if T.null name
              then Nothing
              else Just (beginIfNeeded preArgs (Command name argExprs))

    translateSubstStatusCmdM tok =
      case tok of
        T_SimpleCommand _ assignments cmdToks ->
          translateSubstCommandTokensToStatusM assignments cmdToks
        T_Pipeline _ bang cmds ->
          translateSubstPipelineM bang cmds
        T_Condition _ _ condTok ->
          translateSubstConditionTokenM condTok
        T_Redirecting _ _ inner ->
          translateSubstStatusCmdM inner
        T_AndIf _ l r -> do
          lp <- substPipelineOf <$> translateSubstStatusCmdM l
          rp <- substPipelineOf <$> translateSubstStatusCmdM r
          pure (JobConj (FishJobConjunction Nothing lp [JCAnd rp]))
        T_OrIf _ l r -> do
          lp <- substPipelineOf <$> translateSubstStatusCmdM l
          rp <- substPipelineOf <$> translateSubstStatusCmdM r
          pure (JobConj (FishJobConjunction Nothing lp [JCOr rp]))
        _ -> pure (Command "true" [])

    translateSubstCommandTokensToStatusM assignments cmdToks = do
      baseCmd <- fromMaybe (Command "true" []) <$> translateSubstCommandTokensM cmdToks
      if null assignments
        then pure baseCmd
        else do
          let envFlags = [SetLocal, SetExport]
          envAssigns <- fmap concat (mapM (translateAssignM envFlags) assignments)
          pure (beginIfNeeded envAssigns baseCmd)

    translateSubstPipelineM bang cmds = do
      mCmds <- mapM translateSubstTokenToMaybeStatusCmdM cmds
      pure $
        case catMaybes mCmds of
          [] -> Command "true" []
          (c : cs) ->
            let pipe = Pipeline (substJobPipelineFromList (c : cs))
             in if hasBang bang then Not pipe else pipe

    translateSubstTokenToMaybeStatusCmdM token =
      case token of
        T_SimpleCommand _ assignments rest ->
          Just <$> translateSubstCommandTokensToStatusM assignments rest
        T_Condition {} -> Just <$> translateSubstStatusCmdM token
        T_Redirecting _ _ inner -> translateSubstTokenToMaybeStatusCmdM inner
        T_Pipeline _ bang cmds -> Just <$> translateSubstPipelineM bang cmds
        T_AndIf _ l r -> do
          lp <- substPipelineOf <$> translateSubstStatusCmdM l
          rp <- substPipelineOf <$> translateSubstStatusCmdM r
          pure (Just (JobConj (FishJobConjunction Nothing lp [JCAnd rp])))
        T_OrIf _ l r -> do
          lp <- substPipelineOf <$> translateSubstStatusCmdM l
          rp <- substPipelineOf <$> translateSubstStatusCmdM r
          pure (Just (JobConj (FishJobConjunction Nothing lp [JCOr rp])))
        _ -> pure Nothing

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

    translateSubstConditionTokenM tok = do
      Hoisted _ cond <-
        condFromTokenMWith translateExprM regexExprM literalExpr tok
      pure (condToCommand cond)
      where
        regexExprM t =
          if tokenHasExpansion t
            then translateExprM t
            else hoistM [] (ExprLiteral (tokenToLiteralText t))
        literalExpr = ExprLiteral . tokenToLiteralText

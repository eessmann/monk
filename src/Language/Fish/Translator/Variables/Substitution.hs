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
import Language.Fish.Translator.Args
  ( Arg,
    argRedirect,
    attachArgsToCommand,
    attachArgsToStatement,
  )
import Language.Fish.Translator.Cond
  ( condFromTokenMWith,
    condFromTokenWith,
    condToCommand,
  )
import Language.Fish.Translator.Hoist (Hoisted (..), beginIfNeeded)
import Language.Fish.Translator.Hoist.Monad (HoistedM, hoistM)
import Language.Fish.Translator.Monad
  ( TranslateM,
    WarningCode (..),
    unsupported,
    withCommandSubstScope,
  )
import Language.Fish.Translator.Pipeline (jobPipelineFromList)
import Language.Fish.Translator.Redirections.Core
  ( translateFdRedirectMWith,
    translateFdRedirectWith,
  )
import Language.Fish.Translator.Statement
  ( noteBestEffortSubshell,
    statusCommandBlock,
    statusConjunction,
    translateSubshellStatusCommand,
  )
import Language.Fish.Translator.Token
  ( stripSeparatorTokens,
    tokenHasExpansion,
    tokenToLiteralText,
    tokensHaveBang,
  )
import Language.Fish.Translator.Types
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

data RedirectPlan = MkRedirectPlan [FishStatement] [Arg]

falseStatusCommand :: FishCommand TStatus
falseStatusCommand = Command "false" []

translateSubstRedirects ::
  (Token -> FishExpr TStr) ->
  [Token] ->
  [Arg]
translateSubstRedirects translateExpr =
  mapMaybe (fmap argRedirect . translateSubstRedirectToken translateExpr)

translateSubstRedirectsM ::
  (Token -> HoistedM (FishExpr TStr)) ->
  [Token] ->
  TranslateM RedirectPlan
translateSubstRedirectsM translateExprM redirs = do
  parts <- mapM (translateSubstRedirectTokenM translateExprM) redirs
  let MkHoisted pre maybeRedirs = sequenceA parts
  pure (MkRedirectPlan pre (map argRedirect (catMaybes maybeRedirs)))

translateSubstRedirectToken ::
  (Token -> FishExpr TStr) ->
  Token ->
  Maybe Redirect
translateSubstRedirectToken translateExpr = \case
  T_FdRedirect _ src redirTok -> translateSubstFdRedirect translateExpr src redirTok
  _ -> Nothing

translateSubstRedirectTokenM ::
  (Token -> HoistedM (FishExpr TStr)) ->
  Token ->
  TranslateM (Hoisted (Maybe Redirect))
translateSubstRedirectTokenM translateExprM = \case
  T_FdRedirect _ src redirTok -> translateSubstFdRedirectM translateExprM src redirTok
  _ -> pure (MkHoisted [] Nothing)

translateSubstFdRedirect ::
  (Token -> FishExpr TStr) ->
  String ->
  Token ->
  Maybe Redirect
translateSubstFdRedirect = translateFdRedirectWith

translateSubstFdRedirectM ::
  (Token -> HoistedM (FishExpr TStr)) ->
  String ->
  Token ->
  TranslateM (Hoisted (Maybe Redirect))
translateSubstFdRedirectM = translateFdRedirectMWith

attachSubstRedirs :: [Arg] -> FishStatement -> FishStatement
attachSubstRedirs = attachArgsToStatement

attachSubstStatusRedirs :: [Arg] -> FishCommand TStatus -> FishCommand TStatus
attachSubstStatusRedirs = attachArgsToCommand

wrapSubstPrelude :: [FishStatement] -> FishStatement -> FishStatement
wrapSubstPrelude [] stmt = stmt
wrapSubstPrelude pre stmt = Stmt (Begin (NE.fromList (pre <> [stmt])) [])

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
        Stmt (translateSubstConjunction ConjAnd l r)
      T_OrIf _ l r ->
        Stmt (translateSubstConjunction ConjOr l r)
      T_Backgrounded _ tok ->
        Stmt (Background (translateSubstStatusCmd tok))
      T_BraceGroup _ tokens ->
        substBegin tokens
      T_Subshell _ tokens ->
        substBegin tokens
      T_Redirecting _ redirs inner ->
        attachSubstRedirs (translateSubstRedirects translateExpr redirs) (go inner)
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
        T_Banged _ inner ->
          Not (translateSubstStatusCmd inner)
        T_Condition _ _ condTok ->
          translateSubstConditionToken condTok
        T_BraceGroup _ tokens ->
          translateSubstStatusBlock tokens
        T_Subshell _ tokens ->
          translateSubstSubshellStatus tokens
        T_Redirecting _ redirs inner ->
          attachSubstStatusRedirs (translateSubstRedirects translateExpr redirs) (translateSubstStatusCmd inner)
        T_AndIf _ l r ->
          translateSubstConjunction ConjAnd l r
        T_OrIf _ l r ->
          translateSubstConjunction ConjOr l r
        _ -> falseStatusCommand

    translateSubstCommandTokensToStatus assignments cmdTokens =
      let baseCmd = fromMaybe (Command "true" []) (translateSubstCommandTokens cmdTokens)
       in if null assignments
            then baseCmd
            else
              let envFlags = [SetLocal, SetExport]
                  envAssigns = concatMap (translateAssign envFlags) assignments
               in beginIfNeeded envAssigns baseCmd

    translateSubstPipeline bang cmds =
      case map translateSubstStatusCmd cmds of
        [] -> Command "true" []
        (c : cs) ->
          let pipe = Pipeline (jobPipelineFromList (c NE.:| cs))
           in if tokensHaveBang bang then Not pipe else pipe

    translateSubstStatusBlock tokens =
      statusCommandBlock
        (map translateSubstStatusCmd (stripSeparatorTokens tokens))

    translateSubstSubshellStatus = translateSubstStatusBlock

    translateSubstConjunction conjunction lhs rhs =
      statusConjunction conjunction (translateSubstStatusCmd lhs) (translateSubstStatusCmd rhs)

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
      T_AndIf _ l r ->
        Stmt <$> translateSubstConjunctionM ConjAnd l r
      T_OrIf _ l r ->
        Stmt <$> translateSubstConjunctionM ConjOr l r
      T_Backgrounded _ tok ->
        Stmt . Background <$> translateSubstStatusCmdM tok
      T_BraceGroup _ tokens ->
        substBeginM tokens
      T_Subshell _ tokens -> do
        noteBestEffortSubshell
        substBeginM tokens
      T_Redirecting _ redirs inner -> do
        MkRedirectPlan pre redirArgs <- translateSubstRedirectsM translateExprM redirs
        wrapSubstPrelude pre . attachSubstRedirs redirArgs <$> go inner
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
          let MkHoisted preArgs argExprs = sequenceA translatedArgs
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
        T_Banged _ inner ->
          Not <$> translateSubstStatusCmdM inner
        T_Condition _ _ condTok ->
          translateSubstConditionTokenM condTok
        T_BraceGroup _ tokens ->
          translateSubstStatusBlockM tokens
        T_Subshell _ tokens ->
          translateSubstSubshellStatusM tokens
        T_Redirecting _ redirs inner -> do
          MkRedirectPlan pre redirArgs <- translateSubstRedirectsM translateExprM redirs
          beginIfNeeded pre . attachSubstStatusRedirs redirArgs <$> translateSubstStatusCmdM inner
        T_AndIf _ l r ->
          translateSubstConjunctionM ConjAnd l r
        T_OrIf _ l r ->
          translateSubstConjunctionM ConjOr l r
        _ -> unsupportedSubstStatusCmdM tok

    translateSubstCommandTokensToStatusM assignments cmdToks = do
      baseCmd <- fromMaybe (Command "true" []) <$> translateSubstCommandTokensM cmdToks
      if null assignments
        then pure baseCmd
        else do
          let envFlags = [SetLocal, SetExport]
          envAssigns <- fmap concat (mapM (translateAssignM envFlags) assignments)
          pure (beginIfNeeded envAssigns baseCmd)

    translateSubstPipelineM bang cmds = do
      cmds' <- mapM translateSubstStatusCmdM cmds
      pure $
        case cmds' of
          [] -> Command "true" []
          (c : cs) ->
            let pipe = Pipeline (jobPipelineFromList (c NE.:| cs))
             in if tokensHaveBang bang then Not pipe else pipe

    translateSubstStatusBlockM tokens = do
      cmds <- mapM translateSubstStatusCmdM (stripSeparatorTokens tokens)
      pure (statusCommandBlock cmds)

    translateSubstSubshellStatusM tokens = do
      cmds <- mapM translateSubstStatusCmdM (stripSeparatorTokens tokens)
      translateSubshellStatusCommand cmds

    translateSubstConjunctionM conjunction lhs rhs =
      statusConjunction conjunction
        <$> translateSubstStatusCmdM lhs
        <*> translateSubstStatusCmdM rhs

    translateSubstConditionTokenM tok = do
      MkHoisted _ cond <-
        condFromTokenMWith translateExprM regexExprM literalExpr tok
      pure (condToCommand cond)
      where
        regexExprM t =
          if tokenHasExpansion t
            then translateExprM t
            else hoistM [] (ExprLiteral (tokenToLiteralText t))
        literalExpr = ExprLiteral . tokenToLiteralText

unsupportedSubstStatusCmdM :: Token -> TranslateM (FishCommand TStatus)
unsupportedSubstStatusCmdM tok = do
  unsupported UnsupportedConstruct (Just ("unsupported token in status context: " <> substStatusTokenDescription tok))
  pure falseStatusCommand

substStatusTokenDescription :: Token -> Text
substStatusTokenDescription = \case
  T_CaseExpression {} -> "case expression"
  T_IfExpression {} -> "if expression"
  T_WhileExpression {} -> "while expression"
  T_UntilExpression {} -> "until expression"
  T_ForIn {} -> "for loop"
  T_SelectIn {} -> "select loop"
  T_Function {} -> "function definition"
  T_CoProc {} -> "coprocess (coproc)"
  T_CoProcBody {} -> "coprocess body (coproc)"
  T_Backgrounded {} -> "background job"
  T_Script {} -> "script"
  _ -> "unknown ShellCheck token"

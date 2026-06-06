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
import Language.Fish.Translator.Pipeline (jobPipelineFromList)
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

data RedirectPlan = MkRedirectPlan [FishStatement] [ExprOrRedirect]

translateSubstRedirects ::
  (Token -> FishExpr TStr) ->
  [Token] ->
  [ExprOrRedirect]
translateSubstRedirects translateExpr =
  mapMaybe (fmap RedirectVal . translateSubstRedirectToken translateExpr)

translateSubstRedirectsM ::
  (Token -> HoistedM (FishExpr TStr)) ->
  [Token] ->
  TranslateM RedirectPlan
translateSubstRedirectsM translateExprM redirs = do
  parts <- mapM (translateSubstRedirectTokenM translateExprM) redirs
  let MkHoisted pre maybeRedirs = sequenceA parts
  pure (MkRedirectPlan pre (map RedirectVal (catMaybes maybeRedirs)))

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
translateSubstFdRedirect translateExpr src = \case
  T_IoFile _ op file -> do
    (redirOp, dir) <- redirectOpFromToken op
    let source = sourceFromFd src dir
    pure (MkRedirect source redirOp (RedirectFile (translateExpr file)))
  T_IoDuplicate _ op target -> do
    (redirOp, dir) <- redirectOpFromToken op
    targetRef <- redirectTargetFromDup target
    pure (MkRedirect (sourceFromFd src dir) redirOp targetRef)
  T_HereString _ word ->
    pure (MkRedirect (sourceFromFd src InputRedirect) RedirectIn (RedirectFile (hereStringExpr translateExpr [word])))
  T_HereDoc _ _ _ _ toks ->
    pure (MkRedirect (sourceFromFd src InputRedirect) RedirectIn (RedirectFile (hereDocExpr translateExpr toks)))
  _ -> Nothing

translateSubstFdRedirectM ::
  (Token -> HoistedM (FishExpr TStr)) ->
  String ->
  Token ->
  TranslateM (Hoisted (Maybe Redirect))
translateSubstFdRedirectM translateExprM src = \case
  T_IoFile _ op file ->
    case redirectOpFromToken op of
      Just (redirOp, dir) -> do
        MkHoisted pre expr <- translateExprM file
        let source = sourceFromFd src dir
        pure (MkHoisted pre (Just (MkRedirect source redirOp (RedirectFile expr))))
      Nothing -> pure (MkHoisted [] Nothing)
  T_IoDuplicate _ op target ->
    case redirectOpFromToken op of
      Just (redirOp, dir) ->
        pure (MkHoisted [] (MkRedirect (sourceFromFd src dir) redirOp <$> redirectTargetFromDup target))
      Nothing -> pure (MkHoisted [] Nothing)
  T_HereString _ word -> do
    MkHoisted pre expr <- hereStringExprM translateExprM [word]
    pure (MkHoisted pre (Just (MkRedirect (sourceFromFd src InputRedirect) RedirectIn (RedirectFile expr))))
  T_HereDoc _ _ _ _ toks -> do
    MkHoisted pre expr <- hereDocExprM translateExprM toks
    pure (MkHoisted pre (Just (MkRedirect (sourceFromFd src InputRedirect) RedirectIn (RedirectFile expr))))
  _ -> pure (MkHoisted [] Nothing)

data RedirectDir = InputRedirect | OutputRedirect

redirectOpFromToken :: Token -> Maybe (RedirectOp, RedirectDir)
redirectOpFromToken = \case
  T_Less {} -> Just (RedirectIn, InputRedirect)
  T_Greater {} -> Just (RedirectOut, OutputRedirect)
  T_DGREAT {} -> Just (RedirectOutAppend, OutputRedirect)
  T_CLOBBER {} -> Just (RedirectClobber, OutputRedirect)
  T_LESSGREAT {} -> Just (RedirectReadWrite, InputRedirect)
  T_GREATAND {} -> Just (RedirectOut, OutputRedirect)
  T_LESSAND {} -> Just (RedirectIn, InputRedirect)
  _ -> Nothing

sourceFromFd :: String -> RedirectDir -> RedirectSource
sourceFromFd src dir =
  case src of
    "" -> case dir of
      InputRedirect -> RedirectStdin
      OutputRedirect -> RedirectStdout
    "&" -> RedirectBoth
    _ | Just n <- readMaybe src -> RedirectFD n
    _ -> RedirectStdout

redirectTargetFromDup :: String -> Maybe RedirectTarget
redirectTargetFromDup tgt =
  case tgt of
    "-" -> Just RedirectClose
    _ | Just n <- readMaybe tgt -> Just (RedirectTargetFD n)
    _ -> Nothing

hereDocExpr ::
  (Token -> FishExpr TStr) ->
  [Token] ->
  FishExpr TStr
hereDocExpr = hereExpr "%s"

hereStringExpr ::
  (Token -> FishExpr TStr) ->
  [Token] ->
  FishExpr TStr
hereStringExpr = hereExpr "%s\\n"

hereExpr ::
  Text ->
  (Token -> FishExpr TStr) ->
  [Token] ->
  FishExpr TStr
hereExpr fmt translateExpr toks =
  let expr = concatHereDoc translateExpr toks
      printfStmt =
        Stmt
          ( Command
              "printf"
              [ ExprVal (ExprLiteral fmt),
                ExprVal expr
              ]
          )
   in ExprProcessSubst (printfStmt NE.:| [])

hereDocExprM ::
  (Token -> HoistedM (FishExpr TStr)) ->
  [Token] ->
  HoistedM (FishExpr TStr)
hereDocExprM = hereExprM "%s"

hereStringExprM ::
  (Token -> HoistedM (FishExpr TStr)) ->
  [Token] ->
  HoistedM (FishExpr TStr)
hereStringExprM = hereExprM "%s\\n"

hereExprM ::
  Text ->
  (Token -> HoistedM (FishExpr TStr)) ->
  [Token] ->
  HoistedM (FishExpr TStr)
hereExprM fmt translateExprM toks = do
  MkHoisted pre expr <- concatHereDocM translateExprM toks
  let printfStmt =
        Stmt
          ( Command
              "printf"
              [ ExprVal (ExprLiteral fmt),
                ExprVal expr
              ]
          )
  pure (MkHoisted pre (ExprProcessSubst (printfStmt NE.:| [])))

concatHereDoc ::
  (Token -> FishExpr TStr) ->
  [Token] ->
  FishExpr TStr
concatHereDoc _ [] = ExprLiteral ""
concatHereDoc translateExpr (t : ts) =
  foldl' ExprStringConcat (translateExpr t) (map translateExpr ts)

concatHereDocM ::
  (Token -> HoistedM (FishExpr TStr)) ->
  [Token] ->
  HoistedM (FishExpr TStr)
concatHereDocM _ [] = pure (MkHoisted [] (ExprLiteral ""))
concatHereDocM translateExprM toks = do
  parts <- mapM translateExprM toks
  let MkHoisted pre exprs = sequenceA parts
      expr =
        case exprs of
          [] -> ExprLiteral ""
          (x : xs) -> foldl' ExprStringConcat x xs
  pure (MkHoisted pre expr)

attachSubstRedirs :: [ExprOrRedirect] -> FishStatement -> FishStatement
attachSubstRedirs redirs stmt =
  case redirs of
    [] -> stmt
    _ ->
      case stmt of
        Stmt (Command name args) -> Stmt (Command name (args ++ redirs))
        Stmt (Exec cmd args) -> Stmt (Exec cmd (args ++ redirs))
        Stmt (Begin body suffix) -> Stmt (Begin body (suffix ++ redirs))
        Stmt (If cond thn els suffix) -> Stmt (If cond thn els (suffix ++ redirs))
        Stmt (Switch expr cases suffix) -> Stmt (Switch expr cases (suffix ++ redirs))
        Stmt (While cond body suffix) -> Stmt (While cond body (suffix ++ redirs))
        Stmt (For var listExpr body suffix) -> Stmt (For var listExpr body (suffix ++ redirs))
        StmtList stmts ->
          case NE.nonEmpty stmts of
            Just body -> Stmt (Begin body redirs)
            Nothing -> Comment "Skipped empty redirection block"
        other -> Stmt (Begin (other NE.:| []) redirs)

attachSubstStatusRedirs :: [ExprOrRedirect] -> FishCommand TStatus -> FishCommand TStatus
attachSubstStatusRedirs redirs cmd =
  case redirs of
    [] -> cmd
    _ ->
      case cmd of
        Command name args -> Command name (args ++ redirs)
        Exec cmdExpr args -> Exec cmdExpr (args ++ redirs)
        Begin body suffix -> Begin body (suffix ++ redirs)
        If cond thn els suffix -> If cond thn els (suffix ++ redirs)
        Switch expr cases suffix -> Switch expr cases (suffix ++ redirs)
        While cond body suffix -> While cond body (suffix ++ redirs)
        For var listExpr body suffix -> For var listExpr body (suffix ++ redirs)
        other -> Begin (Stmt other NE.:| []) redirs

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
          let pipe = Pipeline (jobPipelineFromList (c : cs))
           in if tokensHaveBang bang then Not pipe else pipe

    translateSubstTokenToMaybeStatusCmd token =
      case token of
        T_SimpleCommand _ assignments rest ->
          Just (translateSubstCommandTokensToStatus assignments rest)
        T_Condition {} -> Just (translateSubstStatusCmd token)
        T_BraceGroup _ tokens -> Just (translateSubstStatusBlock tokens)
        T_Subshell _ tokens -> Just (translateSubstSubshellStatus tokens)
        T_Redirecting _ redirs inner ->
          attachSubstStatusRedirs (translateSubstRedirects translateExpr redirs) <$> translateSubstTokenToMaybeStatusCmd inner
        T_Pipeline _ bang cmds -> Just (translateSubstPipeline bang cmds)
        T_AndIf _ l r ->
          Just (translateSubstConjunction ConjAnd l r)
        T_OrIf _ l r ->
          Just (translateSubstConjunction ConjOr l r)
        _ -> Nothing

    translateSubstStatusBlock tokens =
      statusCommandBlock
        (mapMaybe translateSubstTokenToMaybeStatusCmd (stripSeparatorTokens tokens))

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
            let pipe = Pipeline (jobPipelineFromList (c : cs))
             in if tokensHaveBang bang then Not pipe else pipe

    translateSubstTokenToMaybeStatusCmdM token =
      case token of
        T_SimpleCommand _ assignments rest ->
          Just <$> translateSubstCommandTokensToStatusM assignments rest
        T_Condition {} -> Just <$> translateSubstStatusCmdM token
        T_BraceGroup _ tokens -> Just <$> translateSubstStatusBlockM tokens
        T_Subshell _ tokens -> Just <$> translateSubstSubshellStatusM tokens
        T_Redirecting _ redirs inner -> do
          MkRedirectPlan pre redirArgs <- translateSubstRedirectsM translateExprM redirs
          fmap (beginIfNeeded pre . attachSubstStatusRedirs redirArgs) <$> translateSubstTokenToMaybeStatusCmdM inner
        T_Pipeline _ bang cmds -> Just <$> translateSubstPipelineM bang cmds
        T_AndIf _ l r ->
          Just <$> translateSubstConjunctionM ConjAnd l r
        T_OrIf _ l r ->
          Just <$> translateSubstConjunctionM ConjOr l r
        _ -> pure Nothing

    translateSubstStatusBlockM tokens = do
      mCmds <- mapM translateSubstTokenToMaybeStatusCmdM (stripSeparatorTokens tokens)
      pure (statusCommandBlock (catMaybes mCmds))

    translateSubstSubshellStatusM tokens = do
      mCmds <- mapM translateSubstTokenToMaybeStatusCmdM (stripSeparatorTokens tokens)
      translateSubshellStatusCommand (catMaybes mCmds)

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

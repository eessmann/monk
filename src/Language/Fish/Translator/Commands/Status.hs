module Language.Fish.Translator.Commands.Status
  ( translateTokensToStatusCmd,
    translateTokenToStatusCmd,
    translateTokensToStatusCmdM,
    translateTokenToStatusCmdM,
    translatePipelineToStatusM,
    translateTokenToMaybeStatusCmdM,
    translateProcessSubstitutionConsumerM,
    stmtToStatusCommand,
  )
where

import Prelude hiding (gets)
import Control.Monad.State.Strict (gets)
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Text qualified as T
import Language.Fish.AST
import Language.Fish.Translator.Args (renderArgs)
import Language.Fish.Translator.Commands.CommandTokens (translateTokensToStatusCmd)
import Language.Fish.Translator.Commands.Tests (translateConditionTokenM)
import Language.Fish.Translator.Commands.Time (stripTimePrefix)
import Language.Fish.Translator.Commands.SimpleCommand (translateSimpleCommandMWith)
import Language.Fish.Translator.Hoist (Hoisted (..), beginIfNeeded)
import Language.Fish.Translator.Monad
  ( TranslateM,
    TranslationContext (..),
    WarningCode (..),
    context,
    unsupported,
  )
import Language.Fish.Translator.Pipeline
  ( applyPipefailIfEnabled,
    jobPipelineFromListWithTime,
    pipelineOf,
  )
import Language.Fish.Translator.Redirections (translateRedirectTokenM)
import Language.Fish.Translator.Rename (renameStatementVariable)
import Language.Fish.Translator.Statement
  ( statusCommandBlock,
    statusConjunction,
    toNonEmptyStmtList,
    translateSubshellStatusCommand,
  )
import Language.Fish.Translator.Token
  ( stripSeparatorTokens,
    tokensHaveBang,
  )
import Language.Fish.Translator.Variables (translateArithmeticStatusM, translateTokenToListExprM)
import Language.Fish.Translator.Variables.ProcessSubst (procSubOutRedirectCommand)
import ShellCheck.AST

translateTokenToStatusCmd :: Token -> FishCommand TStatus
translateTokenToStatusCmd = translateTokensToStatusCmd . pure

translateTokensToStatusCmdM :: [Token] -> TranslateM (FishCommand TStatus)
translateTokensToStatusCmdM tokens =
  case tokens of
    [] -> pure (Command "true" [])
    [tok] -> translateTokenToStatusCmdM tok
    _ -> do
      let toks = stripSeparatorTokens tokens
      cmds <- mapM translateTokenToStatusCmdM toks
      case NE.nonEmpty (map Stmt cmds) of
        Just body -> pure (Begin body [])
        Nothing -> pure (Command "true" [])

translateTokenToStatusCmdM :: Token -> TranslateM (FishCommand TStatus)
translateTokenToStatusCmdM tok =
  case tok of
    T_SimpleCommand _ assignments cmdToks -> do
      locals <- gets (localVars . context)
      inFunc <- gets (inFunction . context)
      let localFlag = if inFunc then SetFunction else SetLocal
      let scopeFlags name =
            if Set.member name locals
              then [localFlag]
              else [SetGlobal]
      stmt <- translateSimpleCommandMWith False scopeFlags assignments cmdToks
      pure (stmtToStatusCommand stmt)
    T_Pipeline _ bang cmds ->
      translatePipelineToStatusM bang cmds
    T_Condition _ _ condTok ->
      do
        MkHoisted pre cmd <- translateConditionTokenM condTok
        pure (beginIfNeeded pre cmd)
    T_Subshell _ tokens ->
      translateSubshellStatusM tokens
    T_BraceGroup _ tokens ->
      translateStatusBlockM tokens
    T_Redirecting _ redirs inner ->
      case exactOutputProcessSubstitution redirs of
        Just procSubBody -> do
          producer <- translateTokenToStatusCmdM inner
          consumer <- translateProcessSubstitutionConsumerM procSubBody
          pure (procSubOutRedirectCommand producer consumer)
        Nothing -> do
          cmd <- translateTokenToStatusCmdM inner
          parts <- mapM translateRedirectTokenM redirs
          let MkHoisted pre mRedirs = sequenceA parts
          pure (beginIfNeeded pre (attachRedirsToStatus (renderArgs (catMaybes mRedirs)) cmd))
    T_Arithmetic _ exprTok ->
      translateArithmeticStatusM exprTok
    T_AndIf _ l r ->
      translateStatusConjunction ConjAnd l r
    T_OrIf _ l r ->
      translateStatusConjunction ConjOr l r
    _ ->
      pure (Command "true" [])

translatePipelineToStatusM :: [Token] -> [Token] -> TranslateM (FishCommand TStatus)
translatePipelineToStatusM bang cmds = do
  let (timed, cmds') = stripTimePrefix cmds
  mCmds <- mapM translateTokenToMaybeStatusCmdM cmds'
  case catMaybes mCmds of
    [] -> pure (Command "true" [])
    (c : cs) -> do
      let pipe = Pipeline (jobPipelineFromListWithTime timed (c : cs))
      pipe' <- applyPipefailIfEnabled pipe
      pure (if tokensHaveBang bang then Not pipe' else pipe')

translateTokenToMaybeStatusCmdM :: Token -> TranslateM (Maybe (FishCommand TStatus))
translateTokenToMaybeStatusCmdM token =
  case token of
    T_SimpleCommand {} -> do
      cmd <- translateTokenToStatusCmdM token
      pure (Just cmd)
    T_Condition {} -> Just <$> translateTokenToStatusCmdM token
    T_BraceGroup _ tokens -> Just <$> translateStatusBlockM tokens
    T_Subshell _ tokens -> Just <$> translateSubshellStatusM tokens
    T_Redirecting _ redirs inner ->
      case exactOutputProcessSubstitution redirs of
        Just procSubBody -> do
          producer <- translateTokenToStatusCmdM inner
          consumer <- translateProcessSubstitutionConsumerM procSubBody
          pure (Just (procSubOutRedirectCommand producer consumer))
        Nothing -> do
          cmd <- translateTokenToStatusCmdM inner
          parts <- mapM translateRedirectTokenM redirs
          let MkHoisted pre mRedirs = sequenceA parts
          pure (Just (beginIfNeeded pre (attachRedirsToStatus (renderArgs (catMaybes mRedirs)) cmd)))
    T_Pipeline _ bang cmds -> Just <$> translatePipelineToStatusM bang cmds
    T_AndIf _ l r ->
      Just <$> translateStatusConjunction ConjAnd l r
    T_OrIf _ l r ->
      Just <$> translateStatusConjunction ConjOr l r
    _ -> pure Nothing

stmtToStatusCommand :: FishStatement -> FishCommand TStatus
stmtToStatusCommand stmt =
  case stmt of
    Stmt cmd ->
      case commandToStatus cmd of
        Just statusCmd -> statusCmd
        Nothing -> Begin (stmt NE.:| []) []
    StmtList stmts ->
      case toNonEmptyStmtList stmts of
        Just body -> Begin body []
        Nothing -> Command "true" []
    Comment _ -> Command "true" []
    EmptyStmt -> Command "true" []

commandToStatus :: FishCommand t -> Maybe (FishCommand TStatus)
commandToStatus cmd =
  case cmd of
    Command {} -> Just cmd
    For {} -> Just cmd
    While {} -> Just cmd
    Begin {} -> Just cmd
    If {} -> Just cmd
    Switch {} -> Just cmd
    Exit {} -> Just cmd
    Source {} -> Just cmd
    Eval {} -> Just cmd
    Read {} -> Just cmd
    Pipeline {} -> Just cmd
    JobConj {} -> Just cmd
    Semicolon _ cmd2 -> commandToStatus cmd2
    Not {} -> Just cmd
    Background {} -> Just cmd
    Wait {} -> Just cmd
    Exec {} -> Just cmd
    Decorated dec inner ->
      case commandToStatus inner of
        Just innerStatus -> Just (Decorated dec innerStatus)
        Nothing -> Nothing
    Return {} -> Just cmd
    _ -> Nothing

translateProcessSubstitutionConsumerM :: [Token] -> TranslateM FishStatement
translateProcessSubstitutionConsumerM tokens = do
  stmts <- mapM translateProcessSubstitutionConsumerTokenM (stripSeparatorTokens tokens)
  pure (consumerStatementFromList stmts)

translateProcessSubstitutionConsumerTokenM :: Token -> TranslateM FishStatement
translateProcessSubstitutionConsumerTokenM tok =
  case tok of
    T_Script _ _ stmts ->
      translateProcessSubstitutionConsumerM stmts
    T_Pipeline _ bang cmds ->
      translateConsumerPipelineM bang cmds
    T_BraceGroup _ tokens -> do
      bodyStmt <- translateProcessSubstitutionConsumerM tokens
      pure $
        case stmtListBody [bodyStmt] of
          Just body -> Stmt (Begin body [])
          Nothing -> Comment "Skipped empty brace group"
    T_WhileExpression _ cond body -> do
      bodyStmt <- translateProcessSubstitutionConsumerM body
      condJob <- translateCondTokensM cond
      pure $
        case stmtListBody [bodyStmt] of
          Just neBody -> Stmt (While condJob neBody [])
          Nothing -> Comment "Skipped empty while loop body"
    T_UntilExpression _ cond body -> do
      bodyStmt <- translateProcessSubstitutionConsumerM body
      condJob <- translateCondTokensM cond
      pure $
        case stmtListBody [bodyStmt] of
          Just neBody -> Stmt (While (negateJobList condJob) neBody [])
          Nothing -> Comment "Skipped empty until loop body"
    T_IfExpression _ branches elseBranch ->
      translateConsumerIf branches elseBranch
    T_AndIf _ l r ->
      Stmt <$> translateConsumerConjunction ConjAnd l r
    T_OrIf _ l r ->
      Stmt <$> translateConsumerConjunction ConjOr l r
    T_ForIn _ var tokens body -> do
      argParts <- mapM translateTokenToListExprM tokens
      let MkHoisted pre args = sequenceA argParts
          (pre', args') =
            if null tokens
              then ([], [ExprVariable (VarAll "argv")])
              else (pre, args)
      bodyStmt <- translateProcessSubstitutionConsumerM body
      let loopVar = fishLoopVarName var
          bodyStmts =
            if loopVar == T.pack var
              then [bodyStmt]
              else [renameStatementVariable (T.pack var) loopVar bodyStmt]
      pure $
        case (NE.nonEmpty args', stmtListBody bodyStmts) of
          (Just neArgs, Just neBody) ->
            let (x NE.:| xs) = neArgs
                listExpr = foldl' ExprListConcat x xs
                forStmt = Stmt (For loopVar listExpr neBody [])
             in consumerStatementFromList (pre' <> [forStmt])
          _ -> Comment "Skipped empty for loop body or list"
    T_Redirecting _ redirs inner ->
      case exactOutputProcessSubstitution redirs of
        Just procSubBody -> Stmt <$> do
          producer <- translateTokenToStatusCmdM inner
          consumer <- translateProcessSubstitutionConsumerM procSubBody
          pure (procSubOutRedirectCommand producer consumer)
        Nothing -> do
          innerStmt <- translateProcessSubstitutionConsumerTokenM inner
          parts <- mapM translateRedirectTokenM redirs
          let MkHoisted pre mRedirs = sequenceA parts
              redirExprs = renderArgs (catMaybes mRedirs)
              attached = attachRedirsToStatement redirExprs innerStmt
          pure (consumerStatementFromList (pre <> [attached]))
    _ -> do
      mStatus <- translateTokenToMaybeStatusCmdM tok
      case mStatus of
        Just statusCmd -> pure (Stmt statusCmd)
        Nothing -> unsupportedProcessSubstitutionConsumer

translateConsumerConjunction :: Conjunction -> Token -> Token -> TranslateM (FishCommand TStatus)
translateConsumerConjunction conjunction lhs rhs =
  statusConjunction conjunction
    <$> translateConsumerStatusCmdM lhs
    <*> translateConsumerStatusCmdM rhs

translateConsumerStatusCmdM :: Token -> TranslateM (FishCommand TStatus)
translateConsumerStatusCmdM tok =
  stmtToStatusCommand <$> translateProcessSubstitutionConsumerTokenM tok

unsupportedProcessSubstitutionConsumer :: TranslateM FishStatement
unsupportedProcessSubstitutionConsumer = do
  unsupported ProcessSubstitutionIssue (Just unsupportedConsumerMessage)
  pure (Stmt (Command "true" []))

unsupportedConsumerMessage :: Text
unsupportedConsumerMessage =
  "output process substitution consumer requires manual review"

translateConsumerPipelineM :: [Token] -> [Token] -> TranslateM FishStatement
translateConsumerPipelineM bang cmds = do
  let (timed, cmds') = stripTimePrefix cmds
  stages <- mapM translateProcessSubstitutionConsumerTokenM cmds'
  pure $
    case filter (not . isEmptyStatement) stages of
      [] -> Stmt (Command "true" [])
      (firstStage : restStages) ->
        let pipe =
              MkFishJobPipeline
                { jpTime = timed,
                  jpVariables = [],
                  jpStatement = firstStage,
                  jpCont = map (PipeTo []) restStages,
                  jpBackgrounded = False
                }
            cmd = Pipeline pipe
         in Stmt (if tokensHaveBang bang then Not cmd else cmd)

translateConsumerIf :: [([Token], [Token])] -> [Token] -> TranslateM FishStatement
translateConsumerIf branches elseBranch = do
  elseStmt <- translateProcessSubstitutionConsumerM elseBranch
  Stmt <$> go branches [elseStmt]
  where
    go [] elseStmts =
      pure $
        case stmtListBody elseStmts of
          Just neElse -> Begin neElse []
          Nothing -> Begin (Stmt (Command "true" []) NE.:| []) []
    go ((condTokens, thenTokens) : rest) elseStmts = do
      condition <- translateCondTokensM condTokens
      thenStmt <- translateProcessSubstitutionConsumerM thenTokens
      nestedElse <- go rest elseStmts
      pure $
        case stmtListBody [thenStmt] of
          Just neThen -> If condition neThen [Stmt nestedElse] []
          Nothing -> If condition (Comment "Empty 'then' block" NE.:| []) [Stmt nestedElse] []

consumerStatementFromList :: [FishStatement] -> FishStatement
consumerStatementFromList stmts =
  case filter (not . isEmptyStatement) stmts of
    [stmt] -> stmt
    filtered ->
      case stmtListBody filtered of
        Just body -> Stmt (Begin body [])
        Nothing -> Stmt (Command "true" [])

stmtListBody :: [FishStatement] -> Maybe (NonEmpty FishStatement)
stmtListBody = toNonEmptyStmtList . filter (not . isEmptyStatement)

isEmptyStatement :: FishStatement -> Bool
isEmptyStatement = \case
  EmptyStmt -> True
  StmtList [] -> True
  _ -> False

negateJobList :: FishJobList -> FishJobList
negateJobList (MkFishJobList conjs) =
  let body = NE.map (Stmt . JobConj) conjs
      negCmd = Not (Begin body [])
   in MkFishJobList (MkFishJobConjunction Nothing (pipelineOf negCmd) [] NE.:| [])

translateCondTokensM :: [Token] -> TranslateM FishJobList
translateCondTokensM tokens =
  jobListFromStatus <$> translateTokensToStatusCmdM tokens

jobListFromStatus :: FishCommand TStatus -> FishJobList
jobListFromStatus cmd =
  case cmd of
    JobConj jc -> MkFishJobList (jc NE.:| [])
    Pipeline jp -> MkFishJobList (MkFishJobConjunction Nothing jp [] NE.:| [])
    _ -> MkFishJobList (MkFishJobConjunction Nothing (pipelineOf cmd) [] NE.:| [])

fishLoopVarName :: String -> Text
fishLoopVarName = \case
  "_" -> "__monk_underscore"
  name -> T.pack name

exactOutputProcessSubstitution :: [Token] -> Maybe [Token]
exactOutputProcessSubstitution =
  \case
    [T_FdRedirect _ src redirTok]
      | src == "" || src == "1" -> outputProcSubRedirectBody redirTok
    _ -> Nothing

outputProcSubRedirectBody :: Token -> Maybe [Token]
outputProcSubRedirectBody = \case
  T_IoFile _ op file
    | isOutputRedirectOp op -> procSubWordBody file
  _ -> Nothing

procSubWordBody :: Token -> Maybe [Token]
procSubWordBody = \case
  T_ProcSub _ ">" body -> Just body
  T_NormalWord _ [T_ProcSub _ ">" body] -> Just body
  _ -> Nothing

isOutputRedirectOp :: Token -> Bool
isOutputRedirectOp = \case
  T_Greater {} -> True
  _ -> False

translateStatusBlockM :: [Token] -> TranslateM (FishCommand TStatus)
translateStatusBlockM tokens = do
  cmds <- mapM translateTokenToStatusCmdM (stripSeparatorTokens tokens)
  pure (statusCommandBlock cmds)

translateSubshellStatusM :: [Token] -> TranslateM (FishCommand TStatus)
translateSubshellStatusM tokens = do
  cmds <- mapM translateTokenToStatusCmdM (stripSeparatorTokens tokens)
  translateSubshellStatusCommand cmds

translateStatusConjunction :: Conjunction -> Token -> Token -> TranslateM (FishCommand TStatus)
translateStatusConjunction conjunction lhs rhs =
  statusConjunction conjunction
    <$> translateTokenToStatusCmdM lhs
    <*> translateTokenToStatusCmdM rhs

attachRedirsToStatus :: [ExprOrRedirect] -> FishCommand TStatus -> FishCommand TStatus
attachRedirsToStatus redirs cmd =
  case cmd of
    Command name args -> Command name (args ++ redirs)
    Exec c args -> Exec c (args ++ redirs)
    Begin body suffix -> Begin body (suffix ++ redirs)
    If cond thn els suffix -> If cond thn els (suffix ++ redirs)
    Switch expr cases suffix -> Switch expr cases (suffix ++ redirs)
    While cond body suffix -> While cond body (suffix ++ redirs)
    For var listExpr body suffix -> For var listExpr body (suffix ++ redirs)
    other ->
      case redirs of
        [] -> other
        _ -> Begin (Stmt other NE.:| []) redirs

attachRedirsToStatement :: [ExprOrRedirect] -> FishStatement -> FishStatement
attachRedirsToStatement redirs stmt =
  case stmt of
    Stmt cmd ->
      case commandToStatus cmd of
        Just statusCmd -> Stmt (attachRedirsToStatus redirs statusCmd)
        Nothing ->
          case redirs of
            [] -> stmt
            _ -> Stmt (Begin (stmt NE.:| []) redirs)
    StmtList stmts ->
      case redirs of
        [] -> stmt
        _ ->
          case toNonEmptyStmtList stmts of
            Just body -> Stmt (Begin body redirs)
            Nothing -> Comment "Skipped empty redirection block"
    other ->
      case redirs of
        [] -> other
        _ -> Stmt (Begin (other NE.:| []) redirs)

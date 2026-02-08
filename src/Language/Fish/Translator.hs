{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator
  ( translateRoot,
    translateToken,
    translateRootWithPositions,
    translateParseResult,
  )
where

import Prelude hiding (gets)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Typeable (cast)
import Language.Fish.AST
import Language.Fish.Translator.Args (renderArgs)
import Language.Fish.Translator.Builtins
  ( translateDeclareCommand,
    translateExportCommand,
    translateLocalCommand,
    translateReadonlyCommand,
    translateShiftCommand,
    translateTrapCommand,
    translateUnsetCommand,
  )
import Language.Fish.Translator.Commands
  ( translateSimpleCommandM,
    translateTokenToStatusCmdM,
  )
import Language.Fish.Translator.Control qualified as Control
import Language.Fish.Translator.ForArithmetic (translateForArithmetic)
import Language.Fish.Translator.Hoist (Hoisted (..))
import Language.Fish.Translator.IO qualified as FIO
import Language.Fish.Translator.Monad
  ( TranslateConfig,
    TranslateError (..),
    TranslateM,
    TranslateState (..),
    TranslationContext (..),
    addWarning,
    isErrexitEnabled,
    noteUnsupported,
    runTranslateWithPositions,
    unsupportedStmt,
    withTokenRange,
  )
import Language.Fish.Translator.Pipeline
  ( pipelineOf,
    shouldWrapErrexit,
    wrapErrexitIfEnabled,
  )
import Language.Fish.Translator.Redirections
  ( parseRedirectTokens,
    parseRedirectTokensM,
    translateRedirectTokenM,
  )
import Language.Fish.Translator.Variables
import Polysemy.State (gets)
import ShellCheck.AST
import ShellCheck.Interface (ParseResult (..), Position)

--------------------------------------------------------------------------------
-- 1. Main translation functions
--------------------------------------------------------------------------------

translateRoot :: Root -> TranslateM FishStatement
translateRoot (Root topToken) = do
  stmt <- translateToken topToken
  pre <- gets preamble
  pure (wrapStmtList (pre <> [stmt]))

translateRootWithPositions ::
  TranslateConfig ->
  M.Map Id (Position, Position) ->
  Root ->
  Either TranslateError (FishStatement, TranslateState)
translateRootWithPositions cfg positions root =
  runTranslateWithPositions cfg positions (translateRoot root)

translateParseResult ::
  TranslateConfig ->
  ParseResult ->
  Either TranslateError (FishStatement, TranslateState)
translateParseResult cfg result = do
  rootTok <- maybe (Left (InternalError "Missing parse root")) Right (prRoot result)
  runTranslateWithPositions cfg (prTokenPositions result) (translateRoot (Root rootTok))

-- | Dispatch on a ShellCheck Token to produce a FishStatement.
translateToken :: Token -> TranslateM FishStatement
translateToken token =
  withTokenRange token $
    case token of
      T_Script _ _ stmts -> wrapStmtList <$> mapM translateToken stmts
      T_SimpleCommand _ assignments cmdToks -> do
        locals <- gets (localVars . context)
        inFunc <- gets (inFunction . context)
        let localFlag = if inFunc then SetFunction else SetLocal
        let scopeFlags name =
              if Set.member name locals
                then [localFlag]
                else [SetGlobal]
        case cmdToks of
          (T_BraceGroup _ inner : rest) -> do
            body <- mapM translateToken inner
            case Control.toNonEmptyStmtList body of
              Just neBody ->
                let (reds, _unparsed) = parseRedirectTokens rest
                 in pure (Stmt (Begin neBody (renderArgs reds)))
              Nothing -> pure (Comment "Skipped empty brace group in simple command")
          (cmdTok : args)
            | tokenToLiteralText cmdTok == "local" ->
                translateLocalCommand args
          (cmdTok : args)
            | tokenToLiteralText cmdTok == "export" ->
                translateExportCommand args
          (cmdTok : args)
            | tokenToLiteralText cmdTok == "declare" ->
                translateDeclareCommand args
          (cmdTok : args)
            | tokenToLiteralText cmdTok == "readonly" ->
                translateReadonlyCommand args
          (cmdTok : args)
            | tokenToLiteralText cmdTok == "shift" ->
                translateShiftCommand args
          (cmdTok : args)
            | tokenToLiteralText cmdTok == "unset" ->
                translateUnsetCommand args
          (cmdTok : args)
            | tokenToLiteralText cmdTok == "trap" ->
                translateTrapCommand args
          (cmdTok : args)
            | tokenToLiteralText cmdTok == "exec" ->
                let scopeFor name =
                      if Set.member name locals
                        then [localFlag]
                        else [SetGlobal]
                 in do
                      Hoisted preRedirs (redirs, plainArgs) <- parseRedirectTokensM args
                      if null plainArgs && not (null redirs)
                        then do
                          fishAssignments <-
                            fmap concat $
                              forM assignments $ \tok ->
                                case tok of
                                  T_Assignment _ _ var _ _ ->
                                    translateAssignmentWithFlagsM (scopeFor (T.pack var)) tok
                                  _ -> translateAssignmentWithFlagsM (scopeFor "") tok
                          let execStmt = Stmt (Command "exec" (renderArgs redirs))
                          addWarning "exec with file descriptor redirection may require manual adjustment in fish"
                          case Control.toNonEmptyStmtList (preRedirs <> fishAssignments <> [execStmt]) of
                            Just body -> pure (Stmt (Begin body []))
                            Nothing -> pure (Comment "Empty exec with assignments")
                        else simplifyStatement <$> translateSimpleCommandM scopeFlags assignments cmdToks
          _ -> simplifyStatement <$> translateSimpleCommandM scopeFlags assignments cmdToks
      T_Pipeline _ bang cmds ->
        case (bang, cmds) of
          ([], [single]) -> translateToken single
          _ -> FIO.translatePipelineM bang cmds
      T_IfExpression _ conditionBranches elseBranch ->
        Control.translateIfExpression translateToken conditionBranches elseBranch
      T_WhileExpression _ cond body -> do
        bodyStmts <- mapM translateToken body
        case Control.toNonEmptyStmtList bodyStmts of
          Just neBody -> do
            condJob <- Control.translateCondTokensM cond
            pure (Stmt (While condJob neBody []))
          Nothing -> pure (Comment "Skipped empty while loop body")
      T_UntilExpression _ cond body -> do
        bodyStmts <- mapM translateToken body
        case Control.toNonEmptyStmtList bodyStmts of
          Just neBody -> do
            condJob <- Control.translateCondTokensM cond
            pure (Stmt (While (Control.negateJobList condJob) neBody []))
          Nothing -> pure (Comment "Skipped empty until loop body")
      T_Arithmetic _ exprTok -> do
        cmd <- translateArithmeticStatusM exprTok
        cmd' <- wrapErrexitIfEnabled cmd
        pure (Stmt cmd')
      T_ForArithmetic _ initTok condTok incTok body ->
        translateForArithmetic translateToken initTok condTok incTok body
      T_Function _ _ _ funcName body -> Control.translateFunction translateToken funcName body
      T_BraceGroup _ tokens -> do
        bodyStmts <- mapM translateToken tokens
        case Control.toNonEmptyStmtList bodyStmts of
          Just neBody -> pure (Stmt (Begin neBody []))
          Nothing -> pure (Comment "Skipped empty brace group")
      T_Subshell _ tokens -> do
        note <- noteUnsupported "Subshell does not isolate environment in fish; best-effort translation emitted"
        bodyStmts <- mapM translateToken tokens
        case Control.toNonEmptyStmtList bodyStmts of
          Just neBody -> pure (StmtList [note, Stmt (Begin neBody [])])
          Nothing -> pure note
      T_AndIf _ l r -> do
        lp <- pipelineOf <$> translateTokenToStatusCmdM l
        rp <- pipelineOf <$> translateTokenToStatusCmdM r
        conj <- wrapErrexitOnConjunction (FishJobConjunction Nothing lp [JCAnd rp])
        pure (Stmt (JobConj conj))
      T_OrIf _ l r -> do
        lp <- pipelineOf <$> translateTokenToStatusCmdM l
        rp <- pipelineOf <$> translateTokenToStatusCmdM r
        conj <- wrapErrexitOnConjunction (FishJobConjunction Nothing lp [JCOr rp])
        pure (Stmt (JobConj conj))
      T_Backgrounded _ bgToken -> do
        cmd <- translateTokenToStatusCmdM bgToken
        pure (Stmt (Background cmd))
      T_Annotation _ _ inner -> translateToken inner
      T_ForIn _ var tokens body -> do
        argParts <- mapM translateTokenToListExprM tokens
        let Hoisted pre args = sequenceA argParts
            (pre', args') =
              if null tokens
                then ([], [ExprVariable (VarAll "argv")])
                else (pre, args)
        bodyStmts <- mapM translateToken body
        case (NE.nonEmpty args', Control.toNonEmptyStmtList bodyStmts) of
          (Just neArgs, Just neBody) ->
            let (x NE.:| xs) = neArgs
                listExpr = foldl' ExprListConcat x xs
                forStmt = Stmt (For (T.pack var) listExpr neBody [])
             in case Control.toNonEmptyStmtList (pre' <> [forStmt]) of
                  Just block -> pure (Stmt (Begin block []))
                  Nothing -> pure (Comment "Skipped empty for loop body or list")
          _ -> pure (Comment "Skipped empty for loop body or list")
      T_SelectIn _ var tokens body ->
        Control.translateSelectExpression translateToken var tokens body
      T_CaseExpression _ switchExpr cases -> Control.translateCaseExpression translateToken switchExpr cases
      T_CoProc {} -> unsupportedStmt "Coprocess (coproc)"
      T_CoProcBody {} -> unsupportedStmt "Coprocess body (coproc)"
      T_Redirecting _ redirs cmd -> do
        parts <- mapM translateRedirectTokenM redirs
        let Hoisted preRedirs redirArgs = sequenceA parts
            redirExprs' = renderArgs (catMaybes redirArgs)
        translated <- translateToken cmd
        let attached = attachRedirs redirExprs' translated
        if null preRedirs
          then pure attached
          else
            case Control.toNonEmptyStmtList (preRedirs <> [attached]) of
              Just body -> pure (Stmt (Begin body []))
              Nothing -> pure (Comment "Skipped empty redirection block")
      _ -> unsupportedStmt ("Skipped token at statement level: " <> T.pack (show token))

wrapStmtList :: [FishStatement] -> FishStatement
wrapStmtList [stmt] = stmt
wrapStmtList stmts = StmtList stmts

attachRedirs :: [ExprOrRedirect] -> FishStatement -> FishStatement
attachRedirs redirs stmt =
  case stmt of
    Stmt (Command name args) -> Stmt (Command name (args ++ redirs))
    Stmt (Exec cmd args) -> Stmt (Exec cmd (args ++ redirs))
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
            Just neBody -> Stmt (Begin neBody redirs)
            Nothing -> Comment "Skipped empty redirection block"
    other ->
      case redirs of
        [] -> other
        _ -> Stmt (Begin (other NE.:| []) redirs)

wrapErrexitOnConjunction :: FishJobConjunction -> TranslateM FishJobConjunction
wrapErrexitOnConjunction conj = do
  enabled <- isErrexitEnabled
  if not enabled
    then pure conj
    else do
      let conts = jcContinuations conj
      case reverse conts of
        [] -> do
          job' <- wrapPipelineErrexit (jcJob conj)
          pure conj {jcJob = job'}
        (lastCont : revInit) -> do
          lastCont' <- wrapConjCont lastCont
          pure conj {jcContinuations = reverse revInit ++ [lastCont']}
  where
    wrapPipelineErrexit pipe =
      case jpStatement pipe of
        Stmt cmd ->
          case cast cmd of
            Just statusCmd ->
              pure pipe {jpStatement = Stmt (wrapErrexitInBegin statusCmd)}
            Nothing -> pure pipe
        _ -> pure pipe

    wrapConjCont cont =
      case cont of
        JCAnd pipe -> JCAnd <$> wrapPipelineErrexit pipe
        JCOr pipe -> JCOr <$> wrapPipelineErrexit pipe

wrapErrexitInBegin :: FishCommand TStatus -> FishCommand TStatus
wrapErrexitInBegin cmd
  | not (shouldWrapErrexit cmd) = cmd
  | otherwise =
      let exitCmd = Exit (Just (ExprSpecialVar SVStatus))
          cmdPipe = pipelineOf cmd
          exitPipe = pipelineOf exitCmd
          body = Stmt (JobConj (FishJobConjunction Nothing cmdPipe [JCOr exitPipe])) NE.:| []
       in Begin body []

simplifyStatement :: FishStatement -> FishStatement
simplifyStatement stmt =
  case stmt of
    Stmt (Begin body suffix)
      | null suffix,
        all isSetStmt (NE.toList body) ->
          StmtList (NE.toList body)
    _ -> stmt
  where
    isSetStmt (Stmt (Set {})) = True
    isSetStmt _ = False

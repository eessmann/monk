{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator
  ( translateRoot,
    translateToken,
    translateRootWithPositions,
    translateParseResult,
  )
where

import Control.Monad.State.Strict (gets)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Typeable (cast)
import Language.Fish.DSL (Script)
import Language.Fish.Translator.Args (renderArgs)
import Language.Fish.Translator.Background
  ( instrumentBackgroundStatusCmd,
  )
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
  ( stmtToStatusCommand,
    translateProcessSubstitutionConsumerM,
    translateSimpleCommandM,
    translateTokenToStatusCmdM,
  )
import Language.Fish.Translator.Construction
  ( attachRedirectsToStatement,
    statementToScript,
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
    WarningCode (..),
    addWarning,
    isErrexitEnabled,
    isErrexitGuardSuppressed,
    preambleStatements,
    runTranslateWithPositions,
    unsupportedStmt,
    withErrexitGuardSuppressed,
    withTokenRange,
  )
import Language.Fish.Translator.Pipeline
  ( pipelineOf,
    shouldWrapErrexit,
    wrapErrexitIfEnabled,
    wrapErrexitStatusCommand,
  )
import Language.Fish.Translator.Redirections
  ( parseRedirectTokens,
    parseRedirectTokensM,
    translateRedirectTokenM,
  )
import Language.Fish.Translator.Rename (renameStatementVariable)
import Language.Fish.Translator.Simplify (simplifyFishStatement)
import Language.Fish.Translator.Statement
  ( jobConjunctionFromPipelines,
    translateSubshellStatement,
  )
import Language.Fish.Translator.Types
import Language.Fish.Translator.Variables
import Language.Fish.Translator.Variables.ProcessSubst (procSubOutRedirectCommand)
import ShellCheck.AST
import ShellCheck.Interface (ParseResult (..), Position)
import Prelude hiding (gets)

--------------------------------------------------------------------------------
-- 1. Main translation functions
--------------------------------------------------------------------------------

translateRoot :: Root -> TranslateM Script
translateRoot root = statementToScript <$> translateRootStatement root

translateRootStatement :: Root -> TranslateM FishStatement
translateRootStatement (Root topToken) = do
  stmt <- translateToken topToken
  pre <- preambleStatements
  pure (simplifyFishStatement (wrapStmtList (pre <> [stmt])))

translateRootWithPositions ::
  TranslateConfig ->
  M.Map Id (Position, Position) ->
  Root ->
  Either TranslateError (Script, TranslateState)
translateRootWithPositions cfg positions root =
  runTranslateWithPositions cfg positions (translateRoot root)

translateParseResult ::
  TranslateConfig ->
  ParseResult ->
  Either TranslateError (Script, TranslateState)
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
                      MkHoisted preRedirs (redirs, plainArgs) <- parseRedirectTokensM args
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
                          addWarning ExecFdRedirect Nothing
                          case Control.toNonEmptyStmtList (preRedirs <> fishAssignments <> [execStmt]) of
                            Just body -> pure (Stmt (Begin body []))
                            Nothing -> pure (Comment "Empty exec with assignments")
                        else translateSimpleCommandM scopeFlags assignments cmdToks
          _ -> translateSimpleCommandM scopeFlags assignments cmdToks
      T_Pipeline _ bang cmds ->
        case (bang, cmds) of
          ([], [single]) -> translateToken single
          _ -> FIO.translatePipelineM translateToken bang cmds
      T_Banged _ inner ->
        Stmt . Not . stmtToStatusCommand <$> withErrexitGuardSuppressed (translateToken inner)
      T_IfExpression _ conditionBranches elseBranch ->
        Control.translateIfExpression translateToken conditionBranches elseBranch
      T_WhileExpression _ cond body -> do
        bodyStmts <- mapM translateToken body
        case Control.toNonEmptyStmtList bodyStmts of
          Just neBody -> do
            condJob <- Control.translateCondTokensWith translateToken cond
            pure (Stmt (While condJob neBody []))
          Nothing -> pure (Comment "Skipped empty while loop body")
      T_UntilExpression _ cond body -> do
        bodyStmts <- mapM translateToken body
        case Control.toNonEmptyStmtList bodyStmts of
          Just neBody -> do
            condJob <- Control.translateCondTokensWith translateToken cond
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
        bodyStmts <- mapM translateToken tokens
        case Control.toNonEmptyStmtList bodyStmts of
          Just neBody -> translateSubshellStatement neBody
          Nothing -> unsupportedStmt BestEffortSubshell Nothing
      T_AndIf _ l r -> do
        lp <- pipelineOf . stmtToStatusCommand <$> withErrexitGuardSuppressed (translateToken l)
        rp <- pipelineOf . stmtToStatusCommand <$> withErrexitGuardSuppressed (translateToken r)
        conj <- wrapErrexitOnConjunction (jobConjunctionFromPipelines ConjAnd lp rp)
        pure (Stmt (JobConj conj))
      T_OrIf _ l r -> do
        lp <- pipelineOf . stmtToStatusCommand <$> withErrexitGuardSuppressed (translateToken l)
        rp <- pipelineOf . stmtToStatusCommand <$> withErrexitGuardSuppressed (translateToken r)
        conj <- wrapErrexitOnConjunction (jobConjunctionFromPipelines ConjOr lp rp)
        pure (Stmt (JobConj conj))
      T_Backgrounded _ bgToken -> do
        statement <- withErrexitGuardSuppressed (translateToken bgToken)
        instrumentBackgroundStatusCmd (stmtToStatusCommand statement)
      T_Annotation _ _ inner -> translateToken inner
      T_Include _ inner -> translateToken inner
      T_SourceCommand _ original _ -> translateToken original
      T_ForIn _ var tokens body -> do
        argParts <- mapM translateTokenToListExprM tokens
        let MkHoisted pre args = sequenceA argParts
            (pre', args') =
              if null tokens
                then ([], [ExprVariable (VarAll "argv")])
                else (pre, args)
        bodyStmts <- mapM translateToken body
        let loopVar = fishLoopVarName var
            bodyStmts' =
              if loopVar == T.pack var
                then bodyStmts
                else map (renameStatementVariable (T.pack var) loopVar) bodyStmts
        case (NE.nonEmpty args', Control.toNonEmptyStmtList bodyStmts) of
          (Just neArgs, Just _) ->
            let (x NE.:| xs) = neArgs
                listExpr = foldl' ExprListConcat x xs
             in case Control.toNonEmptyStmtList bodyStmts' of
                  Just neBody' ->
                    let forStmt = Stmt (For loopVar listExpr neBody' [])
                     in case Control.toNonEmptyStmtList (pre' <> [forStmt]) of
                          Just block -> pure (Stmt (Begin block []))
                          Nothing -> pure (Comment "Skipped empty for loop body or list")
                  Nothing -> pure (Comment "Skipped empty for loop body or list")
          _ -> pure (Comment "Skipped empty for loop body or list")
      T_SelectIn _ var tokens body ->
        Control.translateSelectExpression translateToken var tokens body
      T_CaseExpression _ switchExpr cases -> Control.translateCaseExpression translateToken switchExpr cases
      T_CoProc {} -> unsupportedStmt UnsupportedConstruct (Just "Coprocess (coproc)")
      T_CoProcBody {} -> unsupportedStmt UnsupportedConstruct (Just "Coprocess body (coproc)")
      T_Redirecting _ redirs cmd ->
        case exactOutputProcessSubstitution redirs of
          Just procSubBody -> do
            producer <- translateTokenToStatusCmdM cmd
            consumer <- translateProcessSubstitutionConsumerM procSubBody
            procSubCmd <- wrapErrexitIfEnabled (procSubOutRedirectCommand producer consumer)
            pure (Stmt procSubCmd)
          Nothing -> do
            parts <- mapM translateRedirectTokenM redirs
            let MkHoisted preRedirs redirArgs = sequenceA parts
                redirExprs' = renderArgs (catMaybes redirArgs)
            translated <- translateToken cmd
            let attached = attachRedirectsToStatement redirExprs' translated
            if null preRedirs
              then pure attached
              else case Control.toNonEmptyStmtList (preRedirs <> [attached]) of
                Just body -> pure (Stmt (Begin body []))
                Nothing -> pure (Comment "Skipped empty redirection block")
      _ -> unsupportedStmt UnsupportedConstruct (Just ("Unsupported statement: " <> statementTokenDescription token))

statementTokenDescription :: Token -> Text
statementTokenDescription = \case
  T_Condition {} -> "test condition"
  T_Extglob {} -> "extended glob"
  T_CaseExpression {} -> "case expression"
  T_IfExpression {} -> "if expression"
  T_WhileExpression {} -> "while expression"
  T_UntilExpression {} -> "until expression"
  T_ForIn {} -> "for loop"
  T_SelectIn {} -> "select loop"
  T_Function {} -> "function definition"
  T_CoProc {} -> "coprocess"
  T_CoProcBody {} -> "coprocess body"
  T_BatsTest {} -> "Bats test"
  _ -> "unknown ShellCheck token"

wrapStmtList :: [FishStatement] -> FishStatement
wrapStmtList [stmt] = stmt
wrapStmtList stmts = StmtList stmts

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

fishLoopVarName :: String -> Text
fishLoopVarName = \case
  "_" -> "__monk_underscore"
  name -> T.pack name

wrapErrexitOnConjunction :: FishJobConjunction -> TranslateM FishJobConjunction
wrapErrexitOnConjunction conj = do
  enabled <- isErrexitEnabled
  suppressed <- isErrexitGuardSuppressed
  inCmdSubst <- gets (inCommandSubst . context)
  if not enabled || suppressed || inCmdSubst
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
  | otherwise = Begin (Stmt (wrapErrexitStatusCommand cmd) NE.:| []) []

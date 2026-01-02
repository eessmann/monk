{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator
  ( translateRoot
  , translateToken
  , translateRootWithPositions
  , translateParseResult
  ) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Data.Text as T
import Language.Fish.AST
import ShellCheck.AST
import ShellCheck.Interface (ParseResult(..), Position)
import Language.Fish.Translator.Builtins
  ( translateDeclareCommand
  , translateExportCommand
  , translateLocalCommand
  , translateReadonlyCommand
  , translateScopedAssignment
  , translateShiftCommand
  , translateTrapCommand
  , translateUnsetCommand
  )
import Language.Fish.Translator.Variables
import Language.Fish.Translator.Commands
import Language.Fish.Translator.ForArithmetic (translateForArithmetic)
import Language.Fish.Translator.Redirections (parseRedirectTokens, translateRedirectToken)
import qualified Language.Fish.Translator.Control as Control
import qualified Language.Fish.Translator.IO as FIO
import Language.Fish.Translator.Monad
  ( TranslateConfig
  , TranslateError(..)
  , TranslateM
  , TranslateState(..)
  , TranslationContext(..)
  , runTranslateWithPositions
  , addWarning
  , unsupportedStmt
  , withTokenRange
  )

--------------------------------------------------------------------------------
-- 1. Main translation functions
--------------------------------------------------------------------------------

translateRoot :: Root -> TranslateM FishStatement
translateRoot (Root topToken) = translateToken topToken

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
  runTranslateWithPositions cfg (prTokenPositions result) (translateToken rootTok)

-- | Dispatch on a ShellCheck Token to produce a FishStatement.
translateToken :: Token -> TranslateM FishStatement
translateToken token =
  withTokenRange token $
    case token of
      T_Script _ _ stmts -> wrapStmtList <$> mapM translateToken stmts

      T_SimpleCommand _ assignments cmdToks -> do
        locals <- gets (localVars . context)
        let scopeFlags name =
              if Set.member name locals
                then [SetLocal]
                else [SetGlobal]
        case cmdToks of
          (T_BraceGroup _ inner : rest) -> do
            body <- mapM translateToken inner
            case Control.toNonEmptyStmtList body of
              Just neBody ->
                let (reds, _unparsed) = parseRedirectTokens rest
                in pure (Stmt (Begin neBody reds))
              Nothing -> pure (Comment "Skipped empty brace group in simple command")
          (cmdTok:args)
            | tokenToLiteralText cmdTok == "local" ->
                translateLocalCommand args
          (cmdTok:args)
            | tokenToLiteralText cmdTok == "export" ->
                translateExportCommand args
          (cmdTok:args)
            | tokenToLiteralText cmdTok == "declare" ->
                translateDeclareCommand args
          (cmdTok:args)
            | tokenToLiteralText cmdTok == "readonly" ->
                translateReadonlyCommand args
          (cmdTok:args)
            | tokenToLiteralText cmdTok == "shift" ->
                translateShiftCommand args
          (cmdTok:args)
            | tokenToLiteralText cmdTok == "unset" ->
                translateUnsetCommand args
          (cmdTok:args)
            | tokenToLiteralText cmdTok == "trap" ->
                translateTrapCommand args
          (cmdTok:args)
            | tokenToLiteralText cmdTok == "exec" ->
                let (redirs, plainArgs) = parseRedirectTokens args
                    fishAssignments = concatMap (translateScopedAssignment locals) assignments
                    execStmt = Stmt (Command "exec" redirs)
                in if null plainArgs && not (null redirs)
                     then do
                       addWarning "exec with file descriptor redirection may require manual adjustment in fish"
                       case NE.nonEmpty fishAssignments of
                         Just neAssigns ->
                           case Control.toNonEmptyStmtList (NE.toList neAssigns ++ [execStmt]) of
                             Just body -> pure (Stmt (Begin body []))
                             Nothing -> pure (Comment "Empty exec with assignments")
                         Nothing -> pure execStmt
                     else pure (simplifyStatement (translateSimpleCommand scopeFlags assignments cmdToks))
          _ -> pure (simplifyStatement (translateSimpleCommand scopeFlags assignments cmdToks))

      T_Pipeline _ bang cmds ->
        case (bang, cmds) of
          ([], [single]) -> translateToken single
          _ -> pure (FIO.translatePipeline bang cmds)

      T_IfExpression _ conditionBranches elseBranch ->
        Control.translateIfExpression translateToken conditionBranches elseBranch

      T_WhileExpression _ cond body -> do
        bodyStmts <- mapM translateToken body
        case Control.toNonEmptyStmtList bodyStmts of
          Just neBody -> pure (Stmt (While (Control.translateCondTokens cond) neBody []))
          Nothing     -> pure (Comment "Skipped empty while loop body")

      T_UntilExpression _ cond body -> do
        bodyStmts <- mapM translateToken body
        case Control.toNonEmptyStmtList bodyStmts of
          Just neBody -> pure (Stmt (While (Control.negateJobList (Control.translateCondTokens cond)) neBody []))
          Nothing     -> pure (Comment "Skipped empty until loop body")

      T_Arithmetic _ exprTok ->
        pure (Stmt (translateArithmetic exprTok))

      T_ForArithmetic _ initTok condTok incTok body ->
        translateForArithmetic translateToken initTok condTok incTok body

      T_Function _ _ _ funcName body -> Control.translateFunction translateToken funcName body

      T_BraceGroup _ tokens -> do
        bodyStmts <- mapM translateToken tokens
        case Control.toNonEmptyStmtList bodyStmts of
          Just neBody -> pure (Stmt (Begin neBody []))
          Nothing     -> pure (Comment "Skipped empty brace group")

      T_Subshell _ tokens -> do
        bodyStmts <- mapM translateToken tokens
        case Control.toNonEmptyStmtList bodyStmts of
          Just neBody -> pure (Stmt (Begin neBody []))
          Nothing     -> pure (Comment "Skipped empty subshell")

      T_AndIf _ l r ->
        let lp = FIO.pipelineOf (translateTokenToStatusCmd l)
            rp = FIO.pipelineOf (translateTokenToStatusCmd r)
         in pure (Stmt (JobConj (FishJobConjunction Nothing lp [JCAnd rp])))
      T_OrIf _ l r ->
        let lp = FIO.pipelineOf (translateTokenToStatusCmd l)
            rp = FIO.pipelineOf (translateTokenToStatusCmd r)
         in pure (Stmt (JobConj (FishJobConjunction Nothing lp [JCOr rp])))

      T_Backgrounded _ bgToken -> pure (Stmt (Background (translateTokenToStatusCmd bgToken)))

      T_Annotation _ _ inner -> translateToken inner

      T_ForIn _ var tokens body -> do
        let args = map translateTokenToListExpr tokens
        bodyStmts <- mapM translateToken body
        case (NE.nonEmpty args, Control.toNonEmptyStmtList bodyStmts) of
          (Just neArgs, Just neBody) ->
            let (x NE.:| xs) = neArgs
                listExpr = foldl' ExprListConcat x xs
            in pure (Stmt (For (T.pack var) listExpr neBody []))
          _ -> pure (Comment "Skipped empty for loop body or list")

      T_SelectIn _ var tokens body ->
        Control.translateSelectExpression translateToken var tokens body

      T_CaseExpression _ switchExpr cases -> Control.translateCaseExpression translateToken switchExpr cases

      T_CoProc {} -> unsupportedStmt "Coprocess (coproc)"
      T_CoProcBody {} -> unsupportedStmt "Coprocess body (coproc)"

      T_Redirecting _ redirs cmd -> do
        let redirExprs = mapMaybe translateRedirectToken redirs
        translated <- translateToken cmd
        pure (attachRedirs redirExprs translated)

      _ -> unsupportedStmt ("Skipped token at statement level: " <> T.pack (show token))

wrapStmtList :: [FishStatement] -> FishStatement
wrapStmtList [stmt] = stmt
wrapStmtList stmts  = StmtList stmts

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
        _  -> Stmt (Begin (other NE.:| []) redirs)

simplifyStatement :: FishStatement -> FishStatement
simplifyStatement stmt =
  case stmt of
    Stmt (Begin body suffix)
      | null suffix
      , all isSetStmt (NE.toList body) -> StmtList (NE.toList body)
    _ -> stmt
  where
    isSetStmt (Stmt (Set {})) = True
    isSetStmt _ = False

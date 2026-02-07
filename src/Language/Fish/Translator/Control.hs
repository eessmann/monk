{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Control
  ( translateIfExpression,
    translateFunction,
    translateCaseExpression,
    translateSelectExpression,
    translateCondTokens,
    translateCondTokensM,
    negateJobList,
    toNonEmptyStmtList,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Language.Fish.AST
import Language.Fish.Translator.Commands (translateTokensToStatusCmd, translateTokensToStatusCmdM)
import Language.Fish.Translator.Monad (TranslateM, withFunctionScope)
import Language.Fish.Translator.Variables
  ( patternExprFromToken,
    translateTokenToExprM,
    translateTokenToListExpr,
  )
import ShellCheck.AST

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

toNonEmptyStmtList :: [FishStatement] -> Maybe (NonEmpty FishStatement)
toNonEmptyStmtList stmts = NE.nonEmpty (filter (not . isEmptyStatement) stmts)

isEmptyStatement :: FishStatement -> Bool
isEmptyStatement = \case
  EmptyStmt -> True
  StmtList [] -> True
  _ -> False

tokenHasExpansion :: Token -> Bool
tokenHasExpansion = \case
  T_NormalWord _ parts -> wordHasExpansion parts
  T_DoubleQuoted _ parts -> wordHasExpansion parts
  T_DollarBraced {} -> True
  T_DollarArithmetic {} -> True
  T_Arithmetic {} -> True
  T_DollarExpansion {} -> True
  T_Backticked {} -> True
  T_DollarBraceCommandExpansion {} -> True
  T_ProcSub {} -> True
  _ -> False

wordHasExpansion :: [Token] -> Bool
wordHasExpansion = any isExpansionPart
  where
    isExpansionPart = \case
      T_DollarBraced {} -> True
      T_DollarArithmetic {} -> True
      T_Arithmetic {} -> True
      T_DollarExpansion {} -> True
      T_Backticked {} -> True
      T_DollarBraceCommandExpansion {} -> True
      T_ProcSub {} -> True
      _ -> False

--------------------------------------------------------------------------------
-- If / Function / Case
--------------------------------------------------------------------------------

translateIfExpression :: (Token -> TranslateM FishStatement) -> [([Token], [Token])] -> [Token] -> TranslateM FishStatement
translateIfExpression translateStmt conditionBranches elseBranch = do
  elseStmts <- mapM translateStmt elseBranch
  Stmt <$> translateIf' conditionBranches elseStmts
  where
    translateIf' [] elseStmts =
      pure $ case toNonEmptyStmtList elseStmts of
        Just neElse -> Begin neElse []
        Nothing -> Begin (Stmt (Command "true" []) NE.:| []) []
    translateIf' ((condTokens, thenTokens) : rest) elseStmts =
      do
        condition <- translateCondTokensM condTokens
        thenBlock <- mapM translateStmt thenTokens
        nestedElse <- translateIf' rest elseStmts
        let elseBlock = [Stmt nestedElse]
        pure $ case toNonEmptyStmtList thenBlock of
          Just neThen -> If condition neThen elseBlock []
          Nothing -> If condition (Comment "Empty 'then' block" NE.:| []) elseBlock []

translateFunction :: (Token -> TranslateM FishStatement) -> String -> Token -> TranslateM FishStatement
translateFunction translateStmt funcName bodyToken = withFunctionScope $ do
  bodyStmts <- case bodyToken of
    T_BraceGroup _ stmts -> mapM translateStmt stmts
    T_Subshell _ stmts -> mapM translateStmt stmts
    _ -> (: []) <$> translateStmt bodyToken
  let funcNameT = toText funcName
  pure $ case toNonEmptyStmtList bodyStmts of
    Just neBody ->
      Stmt (Function FishFunction {funcName = funcNameT, funcFlags = [], funcParams = [], funcBody = neBody})
    Nothing -> Comment ("Skipped function with empty body: " <> funcNameT)

translateCaseExpression :: (Token -> TranslateM FishStatement) -> Token -> [(CaseType, [Token], [Token])] -> TranslateM FishStatement
translateCaseExpression translateStmt switchExpr cases = do
  (preSwitch, switchArg) <- translateTokenToExprM switchExpr
  caseItems <- mapM (translateCaseItem translateStmt) cases
  let prePatterns = concatMap fst caseItems
      filtered = catMaybes (map snd caseItems)
      switchStmt =
        case NE.nonEmpty filtered of
          Just neCases -> Stmt (Switch switchArg neCases [])
          Nothing -> Comment "Skipped case expression with no valid cases"
      prelude = preSwitch <> prePatterns
  case prelude of
    [] -> pure switchStmt
    _ ->
      case toNonEmptyStmtList (prelude <> [switchStmt]) of
        Just body -> pure (Stmt (Begin body []))
        Nothing -> pure (Comment "Skipped case expression with no valid cases")

translateCaseItem :: (Token -> TranslateM FishStatement) -> (CaseType, [Token], [Token]) -> TranslateM ([FishStatement], Maybe CaseItem)
translateCaseItem translateStmt (_, patterns, body) = do
  patternPlans <- mapM translateCasePatternM patterns
  let prePatterns = concatMap fst patternPlans
      patternExprs = map snd patternPlans
  bodyStmts <- mapM translateStmt body
  pure $
    ( prePatterns,
      case (NE.nonEmpty patternExprs, toNonEmptyStmtList bodyStmts) of
        (Just nePatterns, Just neBody) -> Just CaseItem {casePatterns = nePatterns, caseBody = neBody}
        _ -> Nothing
    )

translateCasePatternM :: Token -> TranslateM ([FishStatement], FishExpr TStr)
translateCasePatternM tok =
  case tok of
    T_NormalWord _ parts
      | wordHasExpansion parts -> translateCasePatternPartsM parts
    _ | tokenHasExpansion tok -> translateTokenToExprM tok
    _ -> pure ([], patternExprFromToken tok)

translateCasePatternPartsM :: [Token] -> TranslateM ([FishStatement], FishExpr TStr)
translateCasePatternPartsM parts = do
  translated <- mapM translateCasePatternPartM parts
  let pre = concatMap fst translated
      exprs = map snd translated
  pure $
    case NE.nonEmpty exprs of
      Nothing -> (pre, ExprLiteral "")
      Just neExprs ->
        case NE.toList neExprs of
          [single] -> (pre, single)
          xs -> (pre, patternConcatExpr xs)

patternConcatExpr :: [FishExpr TStr] -> FishExpr TStr
patternConcatExpr parts =
  let fmt = T.replicate (length parts) "%s"
      args = ExprVal (ExprLiteral fmt) : map ExprVal parts
   in ExprJoinList (ExprCommandSubst (Stmt (Command "printf" args) NE.:| []))

translateCasePatternPartM :: Token -> TranslateM ([FishStatement], FishExpr TStr)
translateCasePatternPartM part
  | tokenHasExpansion part = translateTokenToExprM part
  | otherwise = pure ([], patternExprFromToken part)

--------------------------------------------------------------------------------
-- Select loops
--------------------------------------------------------------------------------

translateSelectExpression :: (Token -> TranslateM FishStatement) -> String -> [Token] -> [Token] -> TranslateM FishStatement
translateSelectExpression translateStmt var items body = do
  bodyStmts <- mapM translateStmt body
  let itemsExpr = selectItemsExpr items
      itemsVar = "__monk_select_items"
      idxVar = "__monk_select_idx"
      choiceVar = "__monk_select_choice"
      setItems = Stmt (Set [SetLocal] itemsVar itemsExpr)
      menuBody =
        Stmt
          ( Command
              "echo"
              [ ExprVal (ExprStringConcat (ExprVariable (VarScalar idxVar)) (ExprLiteral ")")),
                ExprVal (ExprVariable (VarIndex itemsVar (IndexSingle (selectIndexExpr idxVar))))
              ]
          )
      menuLoop =
        Stmt (For idxVar (selectIndexListExpr itemsVar) (menuBody NE.:| []) [])
      readChoice = Stmt (Read [ReadPrompt "> ", ReadLocal] [choiceVar])
      setReply = Stmt (Set [SetLocal] "REPLY" (ExprListLiteral [ExprVariable (VarScalar choiceVar)]))
      setVar =
        Stmt
          ( Set
              [SetLocal]
              (T.pack var)
              (ExprListLiteral [ExprVariable (VarIndex itemsVar (IndexSingle (selectIndexExpr choiceVar)))])
          )
      loopBody = menuLoop : readChoice : setReply : setVar : bodyStmts
  case toNonEmptyStmtList loopBody of
    Just neBody ->
      let cond = FishJobList (FishJobConjunction Nothing (FishJobPipeline False [] (Stmt (Command "true" [])) [] False) [] NE.:| [])
          whileStmt = Stmt (While cond neBody [])
          initBlock = [setItems, whileStmt]
       in pure $ case toNonEmptyStmtList initBlock of
            Just neInit -> Stmt (Begin neInit [])
            Nothing -> Comment "Skipped select loop"
    Nothing -> pure (Comment "Skipped empty select loop body")

selectItemsExpr :: [Token] -> FishExpr (TList TStr)
selectItemsExpr [] = ExprVariable (VarAll "argv")
selectItemsExpr tokens =
  case map translateTokenToListExpr tokens of
    [] -> ExprListLiteral []
    (x : xs) -> foldl' ExprListConcat x xs

selectIndexListExpr :: Text -> FishExpr (TList TStr)
selectIndexListExpr itemsVar =
  let countExpr =
        ExprCommandSubst (Stmt (Command "count" [ExprVal (ExprVariable (VarAll itemsVar))]) NE.:| [])
   in ExprCommandSubst (Stmt (Command "seq" [ExprVal (ExprLiteral "1"), ExprVal countExpr]) NE.:| [])

selectIndexExpr :: Text -> FishExpr TInt
selectIndexExpr varName =
  ExprMath (ExprVariable (VarScalar varName) NE.:| [])

--------------------------------------------------------------------------------
-- Conditions
--------------------------------------------------------------------------------

translateCondTokens :: [Token] -> FishJobList
translateCondTokens tokens = jobListFromStatus (translateTokensToStatusCmd tokens)

translateCondTokensM :: [Token] -> TranslateM FishJobList
translateCondTokensM tokens = do
  cmd <- translateTokensToStatusCmdM tokens
  pure (jobListFromStatus cmd)

negateJobList :: FishJobList -> FishJobList
negateJobList (FishJobList conjs) =
  let body = NE.map (Stmt . JobConj) conjs
      negCmd = Not (Begin body [])
   in FishJobList (FishJobConjunction Nothing (pipelineOf negCmd) [] NE.:| [])

jobListFromStatus :: FishCommand TStatus -> FishJobList
jobListFromStatus cmd =
  case cmd of
    JobConj jc -> FishJobList (jc NE.:| [])
    Pipeline jp -> FishJobList (FishJobConjunction Nothing jp [] NE.:| [])
    _ -> FishJobList (FishJobConjunction Nothing (pipelineOf cmd) [] NE.:| [])

pipelineOf :: FishCommand TStatus -> FishJobPipeline
pipelineOf cmd =
  FishJobPipeline {jpTime = False, jpVariables = [], jpStatement = Stmt cmd, jpCont = [], jpBackgrounded = False}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Language.Fish.Translator.Control
  ( translateIfExpression
  , translateFunction
  , translateCaseExpression
  , translateSelectExpression
  , translateCondTokens
  , negateJobList
  , toNonEmptyStmtList
  ) where

import qualified Data.List.NonEmpty as NE
import Data.List (foldl')
import qualified Data.Text as T
import Language.Fish.AST
import Language.Fish.Translator.Variables
import Language.Fish.Translator.Commands (translateTokensToStatusCmd)
import Language.Fish.Translator.Monad (TranslateM, withFunctionScope)
import ShellCheck.AST
import Data.Type.Equality (testEquality, (:~:) (Refl))
import Type.Reflection (typeRep)

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
        Nothing     -> Begin (Stmt (Command "true" []) NE.:| []) []
    translateIf' ((condTokens, thenTokens) : rest) elseStmts =
      let condition = translateCondTokens condTokens
       in do
            thenBlock <- mapM translateStmt thenTokens
            nestedElse <- translateIf' rest elseStmts
            let elseBlock = [Stmt nestedElse]
            pure $ case toNonEmptyStmtList thenBlock of
              Just neThen -> If condition neThen elseBlock []
              Nothing     -> If condition (Comment "Empty 'then' block" NE.:| []) elseBlock []

translateFunction :: (Token -> TranslateM FishStatement) -> String -> Token -> TranslateM FishStatement
translateFunction translateStmt funcName bodyToken = withFunctionScope $ do
  bodyStmts <- case bodyToken of
    T_BraceGroup _ stmts -> mapM translateStmt stmts
    T_Subshell _ stmts -> mapM translateStmt stmts
    _ -> (:[]) <$> translateStmt bodyToken
  let funcNameT = toText funcName
  pure $ case toNonEmptyStmtList bodyStmts of
    Just neBody ->
      Stmt (Function FishFunction { funcName = funcNameT, funcFlags = [], funcParams = [], funcBody = neBody })
    Nothing -> Comment ("Skipped function with empty body: " <> funcNameT)

translateCaseExpression :: (Token -> TranslateM FishStatement) -> Token -> [(CaseType, [Token], [Token])] -> TranslateM FishStatement
translateCaseExpression translateStmt switchExpr cases = do
  caseItems <- mapM (translateCaseItem translateStmt) cases
  let switchArg = translateTokenToExpr switchExpr
      filtered = catMaybes caseItems
  pure $ case NE.nonEmpty filtered of
    Just neCases -> Stmt (Switch switchArg neCases [])
    Nothing      -> Comment "Skipped case expression with no valid cases"

translateCaseItem :: (Token -> TranslateM FishStatement) -> (CaseType, [Token], [Token]) -> TranslateM (Maybe CaseItem)
translateCaseItem translateStmt (_, patterns, body) = do
  let patternExprs = map translateTokenToExpr patterns
  bodyStmts <- mapM translateStmt body
  pure $ case (NE.nonEmpty patternExprs, toNonEmptyStmtList bodyStmts) of
    (Just nePatterns, Just neBody) -> Just CaseItem {casePatterns = nePatterns, caseBody = neBody}
    _ -> Nothing

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
          ( Command "echo"
              [ ExprVal (ExprStringConcat (ExprVariable (VarScalar idxVar)) (ExprLiteral ")"))
              , ExprVal (ExprVariable (VarIndex itemsVar (IndexSingle (selectIndexExpr idxVar))))
              ]
          )
      menuLoop =
        Stmt (For idxVar (selectIndexListExpr itemsVar) (menuBody NE.:| []) [])
      readChoice = Stmt (Read [ReadPrompt "> ", ReadLocal] [choiceVar])
      setReply = Stmt (Set [SetLocal] "REPLY" (ExprListLiteral [ExprVariable (VarScalar choiceVar)]))
      setVar =
        Stmt
          ( Set [SetLocal] (T.pack var)
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
    (x:xs) -> foldl' ExprListConcat x xs

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

negateJobList :: FishJobList -> FishJobList
negateJobList (FishJobList (jc NE.:| rest)) =
  FishJobList (negateConj jc NE.:| rest)
  where
    negateConj conj = conj { jcJob = negatePipeline (jcJob conj) }
    negatePipeline jp = jp { jpStatement = negateStmt (jpStatement jp) }
    negateStmt (Stmt (cmd :: FishCommand a)) =
      case testEquality (typeRep @a) (typeRep @TStatus) of
        Just Refl -> Stmt (Not cmd)
        Nothing -> Stmt (Not (Command "true" []))
    negateStmt _ = Stmt (Not (Command "true" []))

jobListFromStatus :: FishCommand TStatus -> FishJobList
jobListFromStatus cmd =
  case cmd of
    JobConj jc -> FishJobList (jc NE.:| [])
    Pipeline jp -> FishJobList (FishJobConjunction Nothing jp [] NE.:| [])
    _ -> FishJobList (FishJobConjunction Nothing (pipelineOf cmd) [] NE.:| [])

pipelineOf :: FishCommand TStatus -> FishJobPipeline
pipelineOf cmd =
  FishJobPipeline { jpTime = False, jpVariables = [], jpStatement = Stmt cmd, jpCont = [], jpBackgrounded = False }

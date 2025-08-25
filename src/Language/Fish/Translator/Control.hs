{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Control
  ( translateIfExpression
  , translateFunction
  , translateCaseExpression
  , translateBoolTokens
  , negateBoolExpr
  , toNonEmptyStmtList
  ) where

import Relude
import qualified Data.List.NonEmpty as NE
import Language.Fish.AST
import Language.Fish.Translator.Variables
import Language.Fish.Translator.Commands (translateTokensToStatusCmd)
import ShellCheck.AST

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

toNonEmptyStmtList :: [FishStatement] -> Maybe (NonEmpty FishStatement)
toNonEmptyStmtList stmts = NE.nonEmpty (filter (/= SemiNl) stmts)

--------------------------------------------------------------------------------
-- If / Function / Case
--------------------------------------------------------------------------------

translateIfExpression :: [([Token], [Token])] -> [Token] -> FishStatement
translateIfExpression conditionBranches elseBranch =
  Stmt (translateIf' conditionBranches (map translateToken elseBranch))
  where
    translateIf' [] elseStmts =
      case toNonEmptyStmtList elseStmts of
        Just neElse -> Begin neElse
        Nothing     -> Begin (Stmt (Command "true" []) NE.:| [])
    translateIf' ((condTokens, thenTokens) : rest) elseStmts =
      let condition = translateBoolTokens condTokens
          thenBlock = map translateToken thenTokens
          elseBlock = [Stmt (translateIf' rest elseStmts)]
       in case toNonEmptyStmtList thenBlock of
            Just neThen -> If condition neThen elseBlock
            Nothing     -> If condition (Comment "Empty 'then' block" NE.:| []) elseBlock

translateFunction :: String -> Token -> FishStatement
translateFunction funcName bodyToken =
  let bodyStmts = case bodyToken of
                    T_BraceGroup _ stmts -> map translateToken stmts
                    T_Subshell _ stmts -> map translateToken stmts
                    _ -> [translateToken bodyToken]
      funcNameT = toText funcName
   in case toNonEmptyStmtList bodyStmts of
        Just neBody ->
          Stmt (Function FishFunction { funcName = funcNameT, funcFlags = [], funcParams = [], funcBody = neBody })
        Nothing -> Comment ("Skipped function with empty body: " <> funcNameT)

translateCaseExpression :: Token -> [(CaseType, [Token], [Token])] -> FishStatement
translateCaseExpression switchExpr cases =
  let switchArg = translateTokenToExpr switchExpr
      caseItems = mapMaybe translateCaseItem cases
   in case NE.nonEmpty caseItems of
        Just neCases -> Stmt (Switch switchArg neCases)
        Nothing      -> Comment "Skipped case expression with no valid cases"

translateCaseItem :: (CaseType, [Token], [Token]) -> Maybe CaseItem
translateCaseItem (_, patterns, body) =
  let patternExprs = map translateTokenToExpr patterns
      bodyStmts = map translateToken body
   in case (NE.nonEmpty patternExprs, toNonEmptyStmtList bodyStmts) of
        (Just nePatterns, Just neBody) -> Just CaseItem {casePatterns = nePatterns, caseBody = neBody}
        _ -> Nothing

--------------------------------------------------------------------------------
-- Booleans
--------------------------------------------------------------------------------

translateBoolTokens :: [Token] -> FishExpr TBool
translateBoolTokens tokens = ExprBoolExpr (BoolCommand (translateTokensToStatusCmd tokens))

negateBoolExpr :: FishExpr TBool -> FishExpr TBool
negateBoolExpr (ExprBoolExpr (BoolNot b)) = b
negateBoolExpr other = ExprBoolExpr (BoolNot other)


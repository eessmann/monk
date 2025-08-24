{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator
  ( translateRoot,
    translateToken,
  )
where

import Prelude hiding (show)
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import Language.Fish.AST
import ShellCheck.AST
import ShellCheck.ASTLib (getLiteralStringDef)
import GHC.Show (show)

--------------------------------------------------------------------------------
-- 1. Main translation functions
--------------------------------------------------------------------------------

translateRoot :: Root -> FishStatement
translateRoot (Root topToken) = translateToken topToken

-- | Dispatch on a ShellCheck Token to produce a FishStatement.
translateToken :: Token -> FishStatement
translateToken token =
  case token of
    T_Script _ _ stmts -> wrapStmtList (map translateToken stmts)

    T_SimpleCommand _ assignments cmdToks -> translateSimpleCommand assignments cmdToks

    T_Pipeline _ bang cmds -> translatePipeline bang cmds

    T_IfExpression _ conditionBranches elseBranch ->
      translateIfExpression conditionBranches elseBranch

    T_WhileExpression _ cond body ->
      case toNonEmptyStmtList (map translateToken body) of
        Just neBody -> Stmt (While (translateBoolTokens cond) neBody)
        Nothing     -> Comment "Skipped empty while loop body"

    T_UntilExpression _ cond body ->
      case toNonEmptyStmtList (map translateToken body) of
        Just neBody -> Stmt (While (negateBoolExpr (translateBoolTokens cond)) neBody)
        Nothing     -> Comment "Skipped empty until loop body"

    T_Function _ _ _ funcName body -> translateFunction funcName body

    T_BraceGroup _ tokens ->
      case toNonEmptyStmtList (map translateToken tokens) of
         Just neBody -> Stmt (Begin neBody)
         Nothing     -> Comment "Skipped empty brace group"

    T_Subshell _ tokens ->
      case toNonEmptyStmtList (map translateToken tokens) of
         Just neBody -> Stmt (Begin neBody)
         Nothing     -> Comment "Skipped empty subshell"

    T_AndIf _ l r ->
      let lp = pipelineOf (translateTokenToStatusCmd l)
          rp = pipelineOf (translateTokenToStatusCmd r)
       in Stmt (JobConj (FishJobConjunction Nothing lp [JCAnd rp] False))
    T_OrIf _ l r ->
      let lp = pipelineOf (translateTokenToStatusCmd l)
          rp = pipelineOf (translateTokenToStatusCmd r)
       in Stmt (JobConj (FishJobConjunction Nothing lp [JCOr rp] False))

    T_Backgrounded _ bgToken -> Stmt (Background (translateTokenToStatusCmd bgToken))

    T_Annotation _ _ inner -> translateToken inner

    T_ForIn _ var tokens body ->
      case (NE.nonEmpty (map translateTokenToExpr tokens), toNonEmptyStmtList (map translateToken body)) of
        (Just neArgs, Just neBody) -> Stmt (For (T.pack var) neArgs neBody)
        _ -> Comment "Skipped empty for loop body or list"

    T_CaseExpression _ switchExpr cases -> translateCaseExpression switchExpr cases

    T_CoProc {} -> Comment "Unsupported: Coprocess (coproc)"

    _ -> Comment ("Skipped token at statement level: " <> T.pack (show token))

--------------------------------------------------------------------------------
-- 2. Helpers
--------------------------------------------------------------------------------

toNonEmptyStmtList :: [FishStatement] -> Maybe (NonEmpty FishStatement)
toNonEmptyStmtList stmts = NE.nonEmpty (filter (/= SemiNl) stmts)

wrapStmtList :: [FishStatement] -> FishStatement
wrapStmtList [stmt] = stmt
wrapStmtList stmts  = StmtList stmts

tokenToLiteralText :: Token -> Text
tokenToLiteralText = T.pack . getLiteralStringDef ""

--------------------------------------------------------------------------------
-- 3. Expressions
--------------------------------------------------------------------------------

translateTokenToExpr :: Token -> FishExpr TStr
translateTokenToExpr = \case
  T_Literal _ s -> ExprLiteral (T.pack s)
  T_SingleQuoted _ s -> ExprLiteral (T.pack s)
  T_DoubleQuoted _ parts -> ExprLiteral (T.concat (map tokenToLiteralText parts))
  T_Arithmetic _ exprToken -> ExprCommandSubstStr (Stmt (translateArithmetic exprToken) :| [])
  T_Backticked _ stmts ->
    case toNonEmptyStmtList (map translateToken stmts) of
      Just neBody -> ExprCommandSubstStr neBody
      Nothing     -> ExprLiteral ""
  T_DollarExpansion _ stmts ->
    case toNonEmptyStmtList (map translateToken stmts) of
      Just neBody -> ExprCommandSubstStr neBody
      Nothing     -> ExprLiteral ""
  T_DollarBraceCommandExpansion _ stmts ->
    case toNonEmptyStmtList (map translateToken stmts) of
      Just neBody -> ExprCommandSubstStr neBody
      Nothing     -> ExprLiteral ""
  other -> ExprLiteral (tokenToLiteralText other)

translateTokenToExprOrRedirect :: Token -> ExprOrRedirect
translateTokenToExprOrRedirect tok = ExprVal (translateTokenToExpr tok)

--------------------------------------------------------------------------------
-- 4. Commands and assignments
--------------------------------------------------------------------------------

translateSimpleCommand :: [Token] -> [Token] -> FishStatement
translateSimpleCommand assignments cmdTokens =
  let fishAssignments = mapMaybe translateAssignment assignments
      fishCmd = translateCommandTokens cmdTokens
   in case (NE.nonEmpty fishAssignments, fishCmd) of
        (Just neAssigns, Just fCmd) ->
          case toNonEmptyStmtList (NE.toList neAssigns ++ [Stmt fCmd]) of
            Just body -> Stmt $ Begin body
            Nothing   -> Comment "Empty command with assignments"
        (Just neAssigns, Nothing) -> StmtList (NE.toList neAssigns)
        (Nothing, Just fCmd) -> Stmt fCmd
        (Nothing, Nothing) -> SemiNl

translateAssignment :: Token -> Maybe FishStatement
translateAssignment tok =
  case tok of
    T_Assignment _ _ var _ val ->
      let fishVar = T.pack var
          fishScope = ScopeLocal
          fishValExpr = translateTokenToExpr val
       in Just $ Stmt (Set fishScope fishVar fishValExpr)
    _ -> Nothing

translateCommandTokens :: [Token] -> Maybe (FishCommand TStatus)
translateCommandTokens cmdTokens =
  case cmdTokens of
    [] -> Nothing
    (c : args) ->
      Just $ case isSingleBracketTest c args of
        Just testCmd -> testCmd
        Nothing -> Command (tokenToLiteralText c) (map translateTokenToExprOrRedirect args)

isSingleBracketTest :: Token -> [Token] -> Maybe (FishCommand TStatus)
isSingleBracketTest bracketToken args =
  let bracketTxt = tokenToLiteralText bracketToken
   in if bracketTxt == "[" then
        case NE.nonEmpty args of
          Just neArgs ->
            let lastToken = NE.last neArgs
                lastText = tokenToLiteralText lastToken
                middle = NE.init neArgs
             in if lastText == "]"
                  then Just (Command "test" (map translateTokenToExprOrRedirect middle))
                  else Nothing
          Nothing -> Nothing
      else Nothing

--------------------------------------------------------------------------------
-- 5. Pipelines and status commands
--------------------------------------------------------------------------------

pipelineOf :: FishCommand TStatus -> FishJobPipeline
pipelineOf cmd =
  FishJobPipeline { jpTime = False, jpVariables = [], jpStatement = Stmt cmd, jpCont = [], jpBackgrounded = False }

jobPipelineFromList :: [FishCommand TStatus] -> FishJobPipeline
jobPipelineFromList [] = pipelineOf (Command "true" [])
jobPipelineFromList (c:cs) =
  FishJobPipeline
    { jpTime = False
    , jpVariables = []
    , jpStatement = Stmt c
    , jpCont = map (\cmd' -> PipeTo { jpcVariables = [], jpcStatement = Stmt cmd' }) cs
    , jpBackgrounded = False
    }

translatePipeline :: [Token] -> [Token] -> FishStatement
translatePipeline bang cmds =
  case mapMaybe translateTokenToMaybeStatusCmd cmds of
    [] -> Stmt (Command "true" [])
    (c:cs) ->
      let pipe = Pipeline (jobPipelineFromList (c:cs))
       in if null bang then Stmt pipe else Stmt (Not pipe)

translateTokenToMaybeStatusCmd :: Token -> Maybe (FishCommand TStatus)
translateTokenToMaybeStatusCmd token =
  case token of
    T_SimpleCommand _ assignments rest -> Just (translateCommandTokensToStatus assignments rest)
    T_Pipeline _ bang cmds -> Just (translatePipelineToStatus bang cmds)
    T_AndIf _ l r ->
      let lp = pipelineOf (translateTokenToStatusCmd l)
          rp = pipelineOf (translateTokenToStatusCmd r)
       in Just (JobConj (FishJobConjunction Nothing lp [JCAnd rp] False))
    T_OrIf _ l r ->
      let lp = pipelineOf (translateTokenToStatusCmd l)
          rp = pipelineOf (translateTokenToStatusCmd r)
       in Just (JobConj (FishJobConjunction Nothing lp [JCOr rp] False))
    _ -> Nothing

translateTokensToStatusCmd :: [Token] -> FishCommand TStatus
translateTokensToStatusCmd tokens =
  case tokens of
    [] -> Command "true" []
    [T_SimpleCommand _ a r] -> translateCommandTokensToStatus a r
    [T_Pipeline _ b c] -> translatePipelineToStatus b c
    -- Other structured forms not mapped directly to status here
    [T_AndIf _ l r] ->
      let lp = pipelineOf (translateTokenToStatusCmd l)
          rp = pipelineOf (translateTokenToStatusCmd r)
       in JobConj (FishJobConjunction Nothing lp [JCAnd rp] False)
    [T_OrIf _ l r] ->
      let lp = pipelineOf (translateTokenToStatusCmd l)
          rp = pipelineOf (translateTokenToStatusCmd r)
       in JobConj (FishJobConjunction Nothing lp [JCOr rp] False)
    (c:args) -> Command (tokenToLiteralText c) (map translateTokenToExprOrRedirect args)

translateTokenToStatusCmd :: Token -> FishCommand TStatus
translateTokenToStatusCmd = translateTokensToStatusCmd . pure

translateCommandTokensToStatus :: [Token] -> [Token] -> FishCommand TStatus
translateCommandTokensToStatus assignments cmdTokens = fromMaybe (Command "true" []) $ do
  fishCmd <- translateCommandTokens cmdTokens
  if null assignments
    then pure fishCmd
    else pure fishCmd -- TODO: handle env-var scoping; simplified for now

translatePipelineToStatus :: [Token] -> [Token] -> FishCommand TStatus
translatePipelineToStatus bang cmds =
  case mapMaybe translateTokenToMaybeStatusCmd cmds of
    [] -> Command "true" []
    (c:cs) -> let pipe = Pipeline (jobPipelineFromList (c:cs))
              in if null bang then pipe else Not pipe

--------------------------------------------------------------------------------
-- 6. If/Function/Case
--------------------------------------------------------------------------------

translateIfExpression :: [([Token], [Token])] -> [Token] -> FishStatement
translateIfExpression conditionBranches elseBranch =
  Stmt (translateIf' conditionBranches (map translateToken elseBranch))
  where
    translateIf' [] elseStmts =
      case toNonEmptyStmtList elseStmts of
        Just neElse -> Begin neElse
        Nothing     -> Begin (Stmt (Command "true" []) :| [])
    translateIf' ((condTokens, thenTokens) : rest) elseStmts =
      let condition = translateBoolTokens condTokens
          thenBlock = map translateToken thenTokens
          elseBlock = [Stmt (translateIf' rest elseStmts)]
       in case toNonEmptyStmtList thenBlock of
            Just neThen -> If condition neThen elseBlock
            Nothing     -> If condition (Comment "Empty 'then' block" :| []) elseBlock

translateFunction :: String -> Token -> FishStatement
translateFunction funcName bodyToken =
  let bodyStmts = case bodyToken of
                    T_BraceGroup _ stmts -> map translateToken stmts
                    T_Subshell _ stmts -> map translateToken stmts
                    _ -> [translateToken bodyToken]
      funcNameT = T.pack funcName
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
-- 7. Booleans and arithmetic
--------------------------------------------------------------------------------

translateBoolTokens :: [Token] -> FishExpr TBool
translateBoolTokens tokens = ExprBoolExpr (BoolCommand (translateTokensToStatusCmd tokens))

negateBoolExpr :: FishExpr TBool -> FishExpr TBool
negateBoolExpr (ExprBoolExpr (BoolNot b)) = b
negateBoolExpr other = ExprBoolExpr (BoolNot other)

translateArithmetic :: Token -> FishCommand TStatus
translateArithmetic exprToken =
  let mathArg = ExprVal (translateTokenToExpr exprToken)
   in Command "math" [mathArg, RedirectVal RedirectOut (ExprLiteral "/dev/null")]

-- (No extra status helpers; complex forms are not mapped to status here.)

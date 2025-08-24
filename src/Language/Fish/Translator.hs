{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator
  ( translateRoot,
    translateToken,
  )
where

import Prelude hiding (show)
import Data.Char (isDigit)
import Text.Read (readMaybe)
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
         Just neBody -> BraceStmt neBody []
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
  T_DollarBraceCommandExpansion _ _ stmts ->
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
   in case (cmdTokens, NE.nonEmpty fishAssignments) of
        -- Handle a brace group with possible trailing redirections: { ...; } > file
        (T_BraceGroup _ inner : rest, _) ->
          case toNonEmptyStmtList (map translateToken inner) of
            Just neBody ->
              let (reds, _unparsed) = parseRedirectTokens rest
              in BraceStmt neBody reds
            Nothing -> Comment "Skipped empty brace group in simple command"
        _ ->
          let fishCmd = translateCommandTokens cmdTokens in
          case (fishCmd, NE.nonEmpty fishAssignments) of
            (Just fCmd, Just neAssigns) ->
              case toNonEmptyStmtList (NE.toList neAssigns ++ [Stmt fCmd]) of
                Just body -> Stmt $ Begin body
                Nothing   -> Comment "Empty command with assignments"
            (Nothing, Just neAssigns) -> StmtList (NE.toList neAssigns)
            (Just fCmd, Nothing) -> Stmt fCmd
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
      let name = tokenToLiteralText c in
      Just $ case isSingleBracketTest c args of
        Just testCmd -> testCmd
        Nothing -> case T.unpack name of
          "exit" -> translateExit args
          "source" -> translateSource args
          "." -> translateSource args
          "eval" -> translateEval args
          "exec" -> translateExec args
          "read" -> translateRead args
          _ -> Command name (map translateTokenToExprOrRedirect args)

-- exit [n]
translateExit :: [Token] -> FishCommand TStatus
translateExit [] = Exit Nothing
translateExit [t] =
  case tokenToLiteralText t of
    txt | T.all isDigit txt && not (T.null txt)
        , Just n <- readMaybe (T.unpack txt) -> Exit (Just (ExprNumLiteral n))
        | otherwise -> Exit Nothing
translateExit ts = Command "exit" (map translateTokenToExprOrRedirect ts)

-- source FILE
translateSource :: [Token] -> FishCommand TStatus
translateSource [] = Command "source" []
translateSource (t:_) = Source (translateTokenToExpr t)

-- eval STRING (join args by space)
translateEval :: [Token] -> FishCommand TStatus
translateEval ts = Eval (concatWithSpaces (map translateTokenToExpr ts))

-- exec CMD [ARGS|REDIRS]
translateExec :: [Token] -> FishCommand TStatus
translateExec [] = Command "exec" []
translateExec (t:rest) = Exec (translateTokenToExpr t) (map translateTokenToExprOrRedirect rest)

-- read [flags] VARS...
translateRead :: [Token] -> FishCommand TStatus
translateRead ts =
  let (flags, vars) = parseReadArgs ts [] []
   in Read flags vars

parseReadArgs :: [Token] -> [ReadFlag] -> [Text] -> ([ReadFlag], [Text])
parseReadArgs [] fs vs = (fs, vs)
parseReadArgs (x:xs) fs vs =
  case tokenToLiteralText x of
    "-p" -> case xs of
              (p:rest) -> parseReadArgs rest (fs ++ [ReadPrompt (tokenToLiteralText p)]) vs
              []       -> parseReadArgs xs fs vs
    "--prompt" -> case xs of
              (p:rest) -> parseReadArgs rest (fs ++ [ReadPrompt (tokenToLiteralText p)]) vs
              []       -> parseReadArgs xs fs vs
    "--local" -> parseReadArgs xs (fs ++ [ReadLocal]) vs
    "--global" -> parseReadArgs xs (fs ++ [ReadGlobal]) vs
    "--universal" -> parseReadArgs xs (fs ++ [ReadUniversal]) vs
    "--export" -> parseReadArgs xs (fs ++ [ReadExport]) vs
    "--array" -> parseReadArgs xs (fs ++ [ReadArray]) vs
    tok ->
      if T.isPrefixOf "-" tok
        then parseReadArgs xs fs vs -- unknown flag: ignore
        else parseReadArgs xs fs (vs ++ [tok])

concatWithSpaces :: [FishExpr TStr] -> FishExpr TStr
concatWithSpaces [] = ExprLiteral ""
concatWithSpaces [e] = e
concatWithSpaces (e:es) = foldl' (\acc x -> ExprStringConcat (ExprStringConcat acc (ExprLiteral " ")) x) e es

-- Very simple redirection token parser based on literal tokens following a brace-group
parseRedirectTokens :: [Token] -> ([ExprOrRedirect], [Token])
parseRedirectTokens [] = ([], [])
parseRedirectTokens (opTok:rest) =
  let opTxt = tokenToLiteralText opTok
   in case opTxt of
        ">"  -> case rest of
                  (t:ts) -> let (more, rem) = parseRedirectTokens ts
                                e = translateTokenToExpr t
                             in (RedirectVal RedirectOut e : more, rem)
                  []     -> ([], opTok:rest)
        ">>" -> case rest of
                  (t:ts) -> let (more, rem) = parseRedirectTokens ts
                                e = translateTokenToExpr t
                             in (RedirectVal RedirectOutAppend e : more, rem)
                  []     -> ([], opTok:rest)
        "<"  -> case rest of
                  (t:ts) -> let (more, rem) = parseRedirectTokens ts
                                e = translateTokenToExpr t
                             in (RedirectVal RedirectIn e : more, rem)
                  []     -> ([], opTok:rest)
        "2>" -> case rest of
                  (t:ts) -> let (more, rem) = parseRedirectTokens ts
                                e = translateTokenToExpr t
                             in (RedirectVal RedirectErr e : more, rem)
                  []     -> ([], opTok:rest)
        "2>>"-> case rest of
                  (t:ts) -> let (more, rem) = parseRedirectTokens ts
                                e = translateTokenToExpr t
                             in (RedirectVal RedirectErrAppend e : more, rem)
                  []     -> ([], opTok:rest)
        "&>" -> case rest of
                  (t:ts) -> let (more, rem) = parseRedirectTokens ts
                                e = translateTokenToExpr t
                             in (RedirectVal RedirectBoth e : more, rem)
                  []     -> ([], opTok:rest)
        "&>>"-> case rest of
                  (t:ts) -> let (more, rem) = parseRedirectTokens ts
                                e = translateTokenToExpr t
                             in (RedirectVal RedirectBothAppend e : more, rem)
                  []     -> ([], opTok:rest)
        _     -> ([], opTok:rest)

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

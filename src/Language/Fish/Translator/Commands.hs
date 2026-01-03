{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Commands
  ( translateSimpleCommand,
    translateCommandTokens,
    translateExit,
    translateSource,
    translateEval,
    translateExec,
    translateRead,
    translateTokensToStatusCmd,
    translateTokenToStatusCmd,
    translateCommandTokensToStatus,
    isSingleBracketTest,
    toNonEmptyStmtList,
    concatWithSpaces,
    stripTimePrefix,
  )
where

import Data.Char (isDigit)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Language.Fish.AST
import Language.Fish.Translator.Names (isValidVarName)
import Language.Fish.Translator.Redirections (parseRedirectTokens)
import Language.Fish.Translator.Variables
import ShellCheck.AST

--------------------------------------------------------------------------------
-- Small helpers
--------------------------------------------------------------------------------

toNonEmptyStmtList :: [FishStatement] -> Maybe (NonEmpty FishStatement)
toNonEmptyStmtList stmts = NE.nonEmpty (filter (not . isEmptyStatement) stmts)

isEmptyStatement :: FishStatement -> Bool
isEmptyStatement = \case
  EmptyStmt -> True
  StmtList [] -> True
  _ -> False

--------------------------------------------------------------------------------
-- Simple commands & Assignments
--------------------------------------------------------------------------------

translateSimpleCommand :: (Text -> [SetFlag]) -> [Token] -> [Token] -> FishStatement
translateSimpleCommand scopeFlags assignments cmdTokens =
  let fishAssignments = concatMap (translateScopedAssignment scopeFlags) assignments
      cmdIsAssignment = not (null cmdTokens) && all isAssignmentWord cmdTokens
      hasCmd = not (null cmdTokens) && not cmdIsAssignment
   in case cmdTokens of
        [] -> assignOnly fishAssignments
        (c : _)
          | T.null (tokenToLiteralText c) || cmdIsAssignment -> assignOnly fishAssignments
        _
          | hasCmd && not (null assignments) ->
              translateEnvPrefix assignments cmdTokens
          | otherwise ->
              let fishCmd = translateCommandTokens cmdTokens
               in case (fishCmd, NE.nonEmpty fishAssignments) of
                    (Just fCmd, Just neAssigns) ->
                      case toNonEmptyStmtList (NE.toList neAssigns ++ [Stmt fCmd]) of
                        Just body -> Stmt (Begin body [])
                        Nothing -> Comment "Empty command with assignments"
                    (Nothing, Just neAssigns) -> StmtList (NE.toList neAssigns)
                    (Just fCmd, Nothing) -> Stmt fCmd
                    (Nothing, Nothing) -> EmptyStmt
  where
    assignOnly assigns =
      case NE.nonEmpty assigns of
        Just neAssigns -> StmtList (NE.toList neAssigns)
        Nothing -> EmptyStmt

    translateScopedAssignment :: (Text -> [SetFlag]) -> Token -> [FishStatement]
    translateScopedAssignment scope tok =
      case tok of
        T_Assignment _ _ var _ _ ->
          translateAssignmentWithFlags (scope (toText var)) tok
        _ -> translateAssignmentWithFlags (scope "") tok

    translateEnvPrefix :: [Token] -> [Token] -> FishStatement
    translateEnvPrefix assigns tokens =
      let envFlags = [SetLocal, SetExport]
          envAssigns = concatMap (translateAssignmentWithFlags envFlags) assigns
          fishCmd = translateCommandTokens tokens
       in case (fishCmd, NE.nonEmpty envAssigns) of
            (Just fCmd, Just neAssigns) ->
              case toNonEmptyStmtList (NE.toList neAssigns ++ [Stmt fCmd]) of
                Just body -> Stmt (Begin body [])
                Nothing -> Comment "Empty command with env assignments"
            (Just fCmd, Nothing) -> Stmt fCmd
            (Nothing, Just neAssigns) -> StmtList (NE.toList neAssigns)
            (Nothing, Nothing) -> EmptyStmt

isAssignmentWord :: Token -> Bool
isAssignmentWord tok =
  case tok of
    T_Assignment {} -> True
    _ -> looksLikeAssignment (tokenToLiteralText tok)

looksLikeAssignment :: Text -> Bool
looksLikeAssignment txt =
  case T.breakOn "=" txt of
    (_, rhs) | T.null rhs -> False
    (lhs, _) ->
      let base = T.takeWhile (/= '[') lhs
       in isValidVarName base

--------------------------------------------------------------------------------
-- Command translation & builtins
--------------------------------------------------------------------------------

translateCommandTokens :: [Token] -> Maybe (FishCommand TStatus)
translateCommandTokens cmdTokens =
  case cmdTokens of
    [] -> Nothing
    (c : args) ->
      let name = tokenToLiteralText c
          (redirs, plainArgs) = parseRedirectTokens args
          argExprs = map translateTokenToExprOrRedirect plainArgs
          fallback = Command name (argExprs ++ redirs)
       in if T.null name
            then Nothing
            else case translateTimeReserved name plainArgs of
              Just timedCmd -> Just timedCmd
              Nothing ->
                Just $
                  if null redirs
                    then case isSingleBracketTest c plainArgs of
                      Just testCmd -> testCmd
                      Nothing -> case isDoubleBracketTest c plainArgs of
                        Just testCmd -> testCmd
                        Nothing -> case T.unpack name of
                          "exit" -> translateExit plainArgs
                          "source" -> translateSource plainArgs
                          "." -> translateSource plainArgs
                          "eval" -> translateEval plainArgs
                          "exec" -> translateExec plainArgs
                          "read" -> translateRead plainArgs
                          _ -> Command name argExprs
                    else fallback

translateTimeReserved :: Text -> [Token] -> Maybe (FishCommand TStatus)
translateTimeReserved name args
  | name /= "time" = Nothing
  | otherwise = case args of
      [tok] -> translateTimedToken tok
      _ -> Nothing
  where
    translateTimedToken tok =
      case tok of
        T_Pipeline _ bang cmds -> Just (timedPipeline bang cmds)
        T_Redirecting _ _ inner -> translateTimedToken inner
        _ -> Nothing

    timedPipeline bang cmds =
      case mapMaybe translateTokenToMaybeStatusCmd cmds of
        [] -> Command "true" []
        (c : cs) ->
          let pipe = Pipeline (jobPipelineFromListWithTime True (c : cs))
           in if null bang then pipe else Not pipe

-- exit [n]
translateExit :: [Token] -> FishCommand TStatus
translateExit [] = Exit Nothing
translateExit [t] =
  case tokenToLiteralText t of
    txt
      | T.all isDigit txt && not (T.null txt),
        Just n <- readMaybe (T.unpack txt) ->
          Exit (Just (ExprNumLiteral n))
      | otherwise -> Command "exit" [translateTokenToExprOrRedirect t]
translateExit ts = Command "exit" (map translateTokenToExprOrRedirect ts)

-- source FILE
translateSource :: [Token] -> FishCommand TStatus
translateSource [] = Command "source" []
translateSource (t : _) = Source (translateTokenToExpr t)

-- eval STRING (join args by space)
translateEval :: [Token] -> FishCommand TStatus
translateEval ts = Eval (concatWithSpaces (map translateTokenToExpr ts))

-- exec CMD [ARGS|REDIRS]
translateExec :: [Token] -> FishCommand TStatus
translateExec [] = Command "exec" []
translateExec (t : rest) = Exec (translateTokenToExpr t) (map translateTokenToExprOrRedirect rest)

-- read [flags] VARS...
translateRead :: [Token] -> FishCommand TStatus
translateRead ts =
  let (flags, vars) = parseReadArgs ts [] []
   in Read flags vars

parseReadArgs :: [Token] -> [ReadFlag] -> [Text] -> ([ReadFlag], [Text])
parseReadArgs [] fs vs = (fs, vs)
parseReadArgs (x : xs) fs vs =
  case tokenToLiteralText x of
    "-p" -> case xs of
      (p : rest) -> parseReadArgs rest (fs ++ [ReadPrompt (tokenToLiteralText p)]) vs
      [] -> parseReadArgs xs fs vs
    "--prompt" -> case xs of
      (p : rest) -> parseReadArgs rest (fs ++ [ReadPrompt (tokenToLiteralText p)]) vs
      [] -> parseReadArgs xs fs vs
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
concatWithSpaces (e : es) = foldl' (\acc x -> ExprStringConcat (ExprStringConcat acc (ExprLiteral " ")) x) e es

isSingleBracketTest :: Token -> [Token] -> Maybe (FishCommand TStatus)
isSingleBracketTest bracketToken args =
  let bracketTxt = tokenToLiteralText bracketToken
   in if bracketTxt == "["
        then case NE.nonEmpty args of
          Just neArgs ->
            let lastToken = NE.last neArgs
                lastText = tokenToLiteralText lastToken
                middle = NE.init neArgs
             in if lastText == "]"
                  then Just (Command "test" (map translateTokenToExprOrRedirect middle))
                  else Nothing
          Nothing -> Nothing
        else Nothing

isDoubleBracketTest :: Token -> [Token] -> Maybe (FishCommand TStatus)
isDoubleBracketTest bracketToken args =
  let bracketTxt = tokenToLiteralText bracketToken
   in if bracketTxt == "[["
        then case NE.nonEmpty args of
          Just neArgs ->
            let lastToken = NE.last neArgs
                lastText = tokenToLiteralText lastToken
                middle = NE.init neArgs
             in if lastText == "]]"
                  then translateDoubleBracketArgs middle
                  else Nothing
          Nothing -> Nothing
        else Nothing

translateDoubleBracketArgs :: [Token] -> Maybe (FishCommand TStatus)
translateDoubleBracketArgs toks =
  case toks of
    [lhs, opTok, rhs] ->
      Just (translateBinaryCondition (tokenToLiteralText opTok) lhs rhs)
    _ -> Nothing

--------------------------------------------------------------------------------
-- Status command helpers (used by control/IO)
--------------------------------------------------------------------------------

translateTokensToStatusCmd :: [Token] -> FishCommand TStatus
translateTokensToStatusCmd tokens =
  case tokens of
    [] -> Command "true" []
    [T_Condition _ _ condTok] -> translateConditionToken condTok
    [T_Arithmetic _ exprTok] -> translateArithmetic exprTok
    [T_SimpleCommand _ a r] -> translateCommandTokensToStatus a r
    [T_Pipeline _ b c] -> translatePipelineToStatus b c
    [T_AndIf _ l r] ->
      let lp = pipelineOf (translateTokenToStatusCmd l)
          rp = pipelineOf (translateTokenToStatusCmd r)
       in JobConj (FishJobConjunction Nothing lp [JCAnd rp])
    [T_OrIf _ l r] ->
      let lp = pipelineOf (translateTokenToStatusCmd l)
          rp = pipelineOf (translateTokenToStatusCmd r)
       in JobConj (FishJobConjunction Nothing lp [JCOr rp])
    (c : args) -> Command (tokenToLiteralText c) (map translateTokenToExprOrRedirect args)

translateTokenToStatusCmd :: Token -> FishCommand TStatus
translateTokenToStatusCmd = translateTokensToStatusCmd . pure

translateConditionToken :: Token -> FishCommand TStatus
translateConditionToken = \case
  TC_Group _ _ inner -> translateConditionToken inner
  TC_And _ _ _ l r ->
    let lp = pipelineOf (translateConditionToken l)
        rp = pipelineOf (translateConditionToken r)
     in JobConj (FishJobConjunction Nothing lp [JCAnd rp])
  TC_Or _ _ _ l r ->
    let lp = pipelineOf (translateConditionToken l)
        rp = pipelineOf (translateConditionToken r)
     in JobConj (FishJobConjunction Nothing lp [JCOr rp])
  TC_Unary _ _ "!" inner ->
    Not (translateConditionToken inner)
  TC_Unary _ _ op inner ->
    Command "test" [ExprVal (ExprLiteral (toText op)), ExprVal (translateTokenToExpr inner)]
  TC_Binary _ _ op lhs rhs ->
    translateBinaryCondition (toText op) lhs rhs
  TC_Nullary _ _ tok ->
    Command "test" [ExprVal (translateTokenToExpr tok)]
  TC_Empty {} ->
    Command "true" []
  other ->
    Command "test" [ExprVal (ExprLiteral (tokenToLiteralText other))]

translateBinaryCondition :: Text -> Token -> Token -> FishCommand TStatus
translateBinaryCondition op lhs rhs
  | op == "=~" = stringMatch "-qr"
  | op == "==" || op == "=" = stringMatch "-q"
  | op == "!=" = Not (stringMatch "-q")
  | otherwise =
      let testArgs =
            [ ExprVal (translateTokenToExpr lhs),
              ExprVal (ExprLiteral op),
              ExprVal (translateTokenToExpr rhs)
            ]
       in Command "test" testArgs
  where
    stringMatch flag =
      Command
        "string"
        [ ExprVal (ExprLiteral "match"),
          ExprVal (ExprLiteral flag),
          ExprVal (ExprLiteral "--"),
          ExprVal (translateTokenToExpr rhs),
          ExprVal (translateTokenToExpr lhs)
        ]

translateCommandTokensToStatus :: [Token] -> [Token] -> FishCommand TStatus
translateCommandTokensToStatus assignments cmdTokens = fromMaybe (Command "true" []) $ do
  fishCmd <- translateCommandTokens cmdTokens
  if null assignments
    then pure fishCmd
    else
      let envFlags = [SetLocal, SetExport]
          envAssigns = concatMap (translateAssignmentWithFlags envFlags) assignments
       in case NE.nonEmpty (envAssigns ++ [Stmt fishCmd]) of
            Just body -> pure (Begin body [])
            Nothing -> pure fishCmd

-- Placeholder to break the import cycle; IO will override via qualified import
translatePipelineToStatus :: [Token] -> [Token] -> FishCommand TStatus
translatePipelineToStatus bang cmds =
  let (timed, cmds') = stripTimePrefix cmds
   in case mapMaybe translateTokenToMaybeStatusCmd cmds' of
        [] -> Command "true" []
        (c : cs) ->
          let pipe = Pipeline (jobPipelineFromListWithTime timed (c : cs))
           in if null bang then pipe else Not pipe

translateTokenToMaybeStatusCmd :: Token -> Maybe (FishCommand TStatus)
translateTokenToMaybeStatusCmd token =
  case token of
    T_SimpleCommand _ assignments rest -> Just (translateCommandTokensToStatus assignments rest)
    T_Condition {} -> Just (translateTokenToStatusCmd token)
    T_Redirecting _ _ inner -> translateTokenToMaybeStatusCmd inner
    T_Pipeline _ bang cmds -> Just (translatePipelineToStatus bang cmds)
    T_AndIf _ l r ->
      let lp = pipelineOf (translateTokenToStatusCmd l)
          rp = pipelineOf (translateTokenToStatusCmd r)
       in Just (JobConj (FishJobConjunction Nothing lp [JCAnd rp]))
    T_OrIf _ l r ->
      let lp = pipelineOf (translateTokenToStatusCmd l)
          rp = pipelineOf (translateTokenToStatusCmd r)
       in Just (JobConj (FishJobConjunction Nothing lp [JCOr rp]))
    _ -> Nothing

jobPipelineFromListWithTime :: Bool -> [FishCommand TStatus] -> FishJobPipeline
jobPipelineFromListWithTime _ [] = pipelineOf (Command "true" [])
jobPipelineFromListWithTime timed (c : cs) =
  FishJobPipeline
    { jpTime = timed,
      jpVariables = [],
      jpStatement = Stmt c,
      jpCont = map (\cmd -> PipeTo {jpcVariables = [], jpcStatement = Stmt cmd}) cs,
      jpBackgrounded = False
    }

pipelineOf :: FishCommand TStatus -> FishJobPipeline
pipelineOf cmd =
  FishJobPipeline {jpTime = False, jpVariables = [], jpStatement = Stmt cmd, jpCont = [], jpBackgrounded = False}

stripTimePrefix :: [Token] -> (Bool, [Token])
stripTimePrefix cmds =
  case cmds of
    (T_SimpleCommand tokId assignments (cmdTok : rest) : xs)
      | tokenToLiteralText cmdTok == "time",
        not (null rest) ->
          case rest of
            (arg1 : _)
              | isTimeOption arg1 -> (False, cmds)
            _ -> (True, T_SimpleCommand tokId assignments rest : xs)
    _ -> (False, cmds)
  where
    isTimeOption tok =
      let txt = tokenToLiteralText tok
       in T.isPrefixOf "-" txt

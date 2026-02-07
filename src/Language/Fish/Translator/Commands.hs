{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Commands
  ( translateSimpleCommand,
    translateSimpleCommandM,
    translateCommandTokens,
    translateCommandTokensM,
    translateExit,
    translateSource,
    translateEval,
    translateExec,
    translateRead,
    translateReadM,
    translateTokensToStatusCmd,
    translateTokenToStatusCmd,
    translateTokensToStatusCmdM,
    translateTokenToStatusCmdM,
    translateCommandTokensToStatus,
    isSingleBracketTest,
    toNonEmptyStmtList,
    concatWithSpaces,
    stripTimePrefix,
  )
where

import Prelude hiding (gets)
import Data.Char (isDigit)
import Data.List (nub)
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Text qualified as T
import Language.Fish.AST
import Language.Fish.Translator.Names (isValidVarName)
import Language.Fish.Translator.Redirections (parseRedirectTokens, parseRedirectTokensM)
import Language.Fish.Translator.Variables
import Language.Fish.Translator.Monad (TranslateM, TranslationContext (..), context, noteUnsupported)
import Polysemy.State (gets)
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

translateSimpleCommandM :: (Text -> [SetFlag]) -> [Token] -> [Token] -> TranslateM FishStatement
translateSimpleCommandM scopeFlags assignments cmdTokens = do
  fishAssignments <- fmap concat (mapM (translateScopedAssignmentM scopeFlags) assignments)
  let cmdIsAssignment = not (null cmdTokens) && all isAssignmentWord cmdTokens
      hasCmd = not (null cmdTokens) && not cmdIsAssignment
  case cmdTokens of
    [] -> pure (assignOnly fishAssignments)
    (c : _)
      | T.null (tokenToLiteralText c) || cmdIsAssignment -> pure (assignOnly fishAssignments)
    _
      | hasCmd && not (null assignments) ->
          translateEnvPrefixM assignments cmdTokens
      | otherwise -> do
          (preArgs, fishCmd) <- translateCommandTokensM cmdTokens
          let allAssigns = fishAssignments
              bodyStmts = preArgs <> allAssigns <> maybe [] (\c -> [Stmt c]) fishCmd
          pure $
            case (fishCmd, NE.nonEmpty allAssigns, null preArgs) of
              (Just fCmd, Just neAssigns, True) ->
                case toNonEmptyStmtList (NE.toList neAssigns ++ [Stmt fCmd]) of
                  Just body -> Stmt (Begin body [])
                  Nothing -> Comment "Empty command with assignments"
              (Just fCmd, Nothing, True) -> Stmt fCmd
              (_, _, False) ->
                case toNonEmptyStmtList bodyStmts of
                  Just body -> Stmt (Begin body [])
                  Nothing -> Comment "Empty command with prelude"
              (Nothing, Just neAssigns, True) -> StmtList (NE.toList neAssigns)
              (Nothing, Nothing, True) -> EmptyStmt
  where
    assignOnly assigns =
      case NE.nonEmpty assigns of
        Just neAssigns -> StmtList (NE.toList neAssigns)
        Nothing -> EmptyStmt

    translateScopedAssignmentM :: (Text -> [SetFlag]) -> Token -> TranslateM [FishStatement]
    translateScopedAssignmentM scope tok =
      case tok of
        T_Assignment _ _ var _ _ ->
          translateAssignmentWithFlagsM (scope (toText var)) tok
        _ -> translateAssignmentWithFlagsM (scope "") tok

    translateEnvPrefixM :: [Token] -> [Token] -> TranslateM FishStatement
    translateEnvPrefixM assigns tokens = do
      let envFlags = [SetLocal, SetExport]
      envAssigns <- fmap concat (mapM (translateAssignmentWithFlagsM envFlags) assigns)
      (preArgs, fishCmd) <- translateCommandTokensM tokens
      let bodyStmts = preArgs <> envAssigns <> maybe [] (\c -> [Stmt c]) fishCmd
      pure $
        case toNonEmptyStmtList bodyStmts of
          Just body -> Stmt (Begin body [])
          Nothing -> Comment "Empty command with env assignments"

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

translateCommandTokensM :: [Token] -> TranslateM ([FishStatement], Maybe (FishCommand TStatus))
translateCommandTokensM cmdTokens =
  case cmdTokens of
    [] -> pure ([], Nothing)
    (c : args) -> do
      let name = tokenToLiteralText c
      (preRedirs, redirs, plainArgs) <- parseRedirectTokensM args
      (preArgs, argExprs) <- translateArgsM plainArgs
      let fallback = Command name (argExprs ++ redirs)
      if T.null name
        then pure (preRedirs <> preArgs, Nothing)
        else case translateTimeReserved name plainArgs of
          Just timedCmd -> pure (preRedirs <> preArgs, Just timedCmd)
          Nothing ->
            if null redirs
              then case isSingleBracketTokens c plainArgs of
                Just middle -> do
                  (pre, testArgs) <- translateArgsM middle
                  pure (preRedirs <> pre, Just (Command "test" testArgs))
                Nothing ->
                  case isDoubleBracketTokens c plainArgs of
                    Just middle -> do
                      (pre, cmd) <- translateDoubleBracketArgsM middle
                      pure (preRedirs <> pre, Just cmd)
                    Nothing ->
                      case T.unpack name of
                        "exit" ->
                          pure (preRedirs <> preArgs, Just (translateExitFromExprs plainArgs argExprs))
                        "source" ->
                          pure (preRedirs <> preArgs, Just (Command "source" argExprs))
                        "." ->
                          pure (preRedirs <> preArgs, Just (Command "source" argExprs))
                        "eval" -> do
                          (pre, expr) <- translateEvalM plainArgs
                          let cmd = Eval expr
                          pure (preRedirs <> pre, Just cmd)
                        "exec" ->
                          pure (preRedirs <> preArgs, Just (Command "exec" argExprs))
                        "set" -> do
                          let issues = detectSetOptionIssues plainArgs
                          notes <- mapM noteUnsupported issues
                          if null issues
                            then pure (preRedirs <> preArgs, Just (Command "set" argExprs))
                            else pure (preRedirs <> preArgs <> notes, Nothing)
                        "read" -> do
                          let ReadParseResult {readFlags, readVars, readIssues, readUnsupported} =
                                parseReadArgsDetailed plainArgs [] [] [] False
                          notes <- mapM noteUnsupported (nub readIssues)
                          if readUnsupported
                            then pure (preRedirs <> preArgs <> notes, Just (Command "read" argExprs))
                            else pure (preRedirs <> preArgs <> notes, Just (Read readFlags readVars))
                        _ ->
                          pure (preRedirs <> preArgs, Just (Command name argExprs))
              else pure (preRedirs <> preArgs, Just fallback)

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
           in if hasBang bang then Not pipe else pipe

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
translateSource (t : rest) =
  if null rest
    then Source (translateTokenToExpr t)
    else Command "source" (map translateTokenToExprOrRedirect (t : rest))

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
  let (flags, vars, unsupported) = parseReadArgs ts [] [] False
   in if unsupported
        then Command "read" (map translateTokenToExprOrRedirect ts)
        else Read flags vars

translateReadM :: [Token] -> TranslateM (FishCommand TStatus)
translateReadM = pure . translateRead

data ReadParseResult = ReadParseResult
  { readFlags :: [ReadFlag],
    readVars :: [Text],
    readIssues :: [Text],
    readUnsupported :: Bool
  }
  deriving stock (Show, Eq)

parseReadArgs :: [Token] -> [ReadFlag] -> [Text] -> Bool -> ([ReadFlag], [Text], Bool)
parseReadArgs ts fs vs unsupported =
  let ReadParseResult {readFlags, readVars, readUnsupported} = parseReadArgsDetailed ts fs vs [] unsupported
   in (readFlags, readVars, readUnsupported)

parseReadArgsDetailed :: [Token] -> [ReadFlag] -> [Text] -> [Text] -> Bool -> ReadParseResult
parseReadArgsDetailed [] fs vs issues unsupported =
  let needsSplitNote = ReadArray `elem` fs || length vs > 1
      issues' =
        if needsSplitNote
          then issues <> ["read IFS splitting semantics may differ between bash and fish"]
          else issues
   in ReadParseResult fs vs issues' unsupported
parseReadArgsDetailed (x : xs) fs vs issues unsupported =
  case tokenToLiteralText x of
    "-p" -> case xs of
      (p : rest) ->
        parseReadArgsDetailed rest (fs ++ [ReadPrompt (tokenToLiteralText p)]) vs issues unsupported
      [] -> parseReadArgsDetailed xs fs vs issues True
    "--prompt" -> case xs of
      (p : rest) ->
        parseReadArgsDetailed rest (fs ++ [ReadPrompt (tokenToLiteralText p)]) vs issues unsupported
      [] -> parseReadArgsDetailed xs fs vs issues True
    "-n" -> parseReadArgValue xs fs vs issues unsupported ReadNChars "read -n requires a value"
    "--nchars" -> parseReadArgValue xs fs vs issues unsupported ReadNChars "read --nchars requires a value"
    "-t" -> parseReadArgValue xs fs vs issues unsupported ReadTimeout "read -t requires a value"
    "--timeout" -> parseReadArgValue xs fs vs issues unsupported ReadTimeout "read --timeout requires a value"
    "-u" -> parseReadArgValue xs fs vs issues unsupported ReadFD "read -u requires a value"
    "--fd" -> parseReadArgValue xs fs vs issues unsupported ReadFD "read --fd requires a value"
    "-a" -> parseReadArgsDetailed xs (fs ++ [ReadArray]) vs issues unsupported
    "--array" -> parseReadArgsDetailed xs (fs ++ [ReadArray]) vs issues unsupported
    "-r" ->
      parseReadArgsDetailed xs fs vs (issues <> ["read -r has no fish equivalent; backslash escapes may differ"]) unsupported
    tok
      | Just val <- readShortArg "n" tok -> parseReadArgsDetailed xs (fs ++ [ReadNChars val]) vs issues unsupported
      | Just val <- readShortArg "t" tok -> parseReadArgsDetailed xs (fs ++ [ReadTimeout val]) vs issues unsupported
      | Just val <- readShortArg "u" tok -> parseReadArgsDetailed xs (fs ++ [ReadFD val]) vs issues unsupported
      | T.isPrefixOf "-" tok ->
          parseReadArgsDetailed xs fs vs (issues <> ["Unsupported read flag: " <> tok]) True
      | otherwise ->
          parseReadArgsDetailed xs fs (vs ++ [tok]) issues unsupported

parseReadArgValue ::
  [Token] ->
  [ReadFlag] ->
  [Text] ->
  [Text] ->
  Bool ->
  (Text -> ReadFlag) ->
  Text ->
  ReadParseResult
parseReadArgValue xs fs vs issues unsupported mkFlag errMsg =
  case xs of
    (p : rest) ->
      let val = tokenToLiteralText p
       in if T.null val
            then parseReadArgsDetailed rest fs vs (issues <> [errMsg]) True
            else parseReadArgsDetailed rest (fs ++ [mkFlag val]) vs issues unsupported
    [] -> parseReadArgsDetailed xs fs vs (issues <> [errMsg]) True

readShortArg :: Text -> Text -> Maybe Text
readShortArg flag tok =
  if T.isPrefixOf ("-" <> flag) tok && T.length tok > 2
    then Just (T.drop 2 tok)
    else Nothing

detectSetOptionIssues :: [Token] -> [Text]
detectSetOptionIssues toks =
  nub (go (map tokenToLiteralText toks) [])
  where
    go [] acc = acc
    go (t : ts) acc
      | t == "--" = acc
      | t == "-o" =
          case ts of
            (opt : rest) -> go rest (acc <> noteForOpt opt)
            [] -> acc <> ["Bash set -o requires an option; manual review required"]
      | T.isPrefixOf "-" t =
          let flags = T.drop 1 t
              acc' = acc <> noteForFlags flags
              hasO = T.any (== 'o') flags
           in if hasO
                then case ts of
                  (opt : rest) -> go rest (acc' <> noteForOpt opt)
                  [] -> acc' <> ["Bash set -o requires an option; manual review required"]
                else go ts acc'
      | otherwise = go ts acc

    noteForFlags flags =
      concat
        [ if T.any (== 'e') flags
            then ["Bash set -e/errexit has no fish equivalent; manual review required"]
            else [],
          if T.any (== 'u') flags
            then ["Bash set -u/nounset has no fish equivalent; manual review required"]
            else []
        ]

    noteForOpt opt =
      case opt of
        "pipefail" -> ["Bash set -o pipefail has no fish equivalent; manual review required"]
        "errexit" -> ["Bash set -o errexit has no fish equivalent; manual review required"]
        "nounset" -> ["Bash set -o nounset has no fish equivalent; manual review required"]
        _ -> []

translateArgsM :: [Token] -> TranslateM ([FishStatement], [ExprOrRedirect])
translateArgsM toks = do
  parts <- mapM translateTokenToExprOrRedirectM toks
  let pre = concatMap fst parts
      exprs = map snd parts
  pure (pre, exprs)

translateEvalM :: [Token] -> TranslateM ([FishStatement], FishExpr TStr)
translateEvalM toks = do
  parts <- mapM translateTokenToExprM toks
  let pre = concatMap fst parts
      exprs = map snd parts
  pure (pre, concatWithSpaces exprs)

translateExitFromExprs :: [Token] -> [ExprOrRedirect] -> FishCommand TStatus
translateExitFromExprs [] _ = Exit Nothing
translateExitFromExprs [t] [expr] =
  case tokenToLiteralText t of
    txt
      | T.all isDigit txt && not (T.null txt),
        Just n <- readMaybe (T.unpack txt) ->
          Exit (Just (ExprNumLiteral n))
      | otherwise -> Command "exit" [expr]
translateExitFromExprs _ exprs = Command "exit" exprs


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

isSingleBracketTokens :: Token -> [Token] -> Maybe [Token]
isSingleBracketTokens bracketToken args =
  let bracketTxt = tokenToLiteralText bracketToken
   in if bracketTxt == "["
        then case NE.nonEmpty args of
          Just neArgs ->
            let lastToken = NE.last neArgs
                lastText = tokenToLiteralText lastToken
                middle = NE.init neArgs
             in if lastText == "]" then Just middle else Nothing
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

isDoubleBracketTokens :: Token -> [Token] -> Maybe [Token]
isDoubleBracketTokens bracketToken args =
  let bracketTxt = tokenToLiteralText bracketToken
   in if bracketTxt == "[["
        then case NE.nonEmpty args of
          Just neArgs ->
            let lastToken = NE.last neArgs
                lastText = tokenToLiteralText lastToken
                middle = NE.init neArgs
             in if lastText == "]]" then Just middle else Nothing
          Nothing -> Nothing
        else Nothing

translateDoubleBracketArgs :: [Token] -> Maybe (FishCommand TStatus)
translateDoubleBracketArgs toks =
  case toks of
    [lhs, opTok, rhs] ->
      Just (translateBinaryCondition (tokenToLiteralText opTok) lhs rhs)
    _ -> Nothing

translateDoubleBracketArgsM :: [Token] -> TranslateM ([FishStatement], FishCommand TStatus)
translateDoubleBracketArgsM toks =
  case toks of
    [lhs, opTok, rhs] -> do
      (preL, lhsExpr) <- translateTokenToExprM lhs
      (preR, rhsExpr) <- translateTokenToExprM rhs
      let cmd = translateBinaryConditionExprs (tokenToLiteralText opTok) lhsExpr rhsExpr
      pure (preL <> preR, cmd)
    _ -> pure ([], Command "true" [])

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

translateTokensToStatusCmdM :: [Token] -> TranslateM (FishCommand TStatus)
translateTokensToStatusCmdM tokens =
  case tokens of
    [] -> pure (Command "true" [])
    [tok] -> translateTokenToStatusCmdM tok
    _ -> do
      let toks = filter (not . isSeparatorToken) tokens
      cmds <- mapM translateTokenToStatusCmdM toks
      case NE.nonEmpty (map Stmt cmds) of
        Just body -> pure (Begin body [])
        Nothing -> pure (Command "true" [])

translateTokenToStatusCmdM :: Token -> TranslateM (FishCommand TStatus)
translateTokenToStatusCmdM tok =
  case tok of
    T_SimpleCommand _ assignments cmdToks -> do
      locals <- gets (localVars . context)
      let scopeFlags name =
            if Set.member name locals
              then [SetLocal]
              else [SetGlobal]
      stmt <- translateSimpleCommandM scopeFlags assignments cmdToks
      pure (stmtToStatusCommand stmt)
    T_Pipeline _ bang cmds ->
      translatePipelineToStatusM bang cmds
    T_Condition _ _ condTok ->
      translateConditionTokenM condTok
    T_Redirecting _ _ inner ->
      translateTokenToStatusCmdM inner
    T_Arithmetic _ exprTok ->
      translateArithmeticStatusM exprTok
    T_AndIf _ l r -> do
      lp <- pipelineOf <$> translateTokenToStatusCmdM l
      rp <- pipelineOf <$> translateTokenToStatusCmdM r
      pure (JobConj (FishJobConjunction Nothing lp [JCAnd rp]))
    T_OrIf _ l r -> do
      lp <- pipelineOf <$> translateTokenToStatusCmdM l
      rp <- pipelineOf <$> translateTokenToStatusCmdM r
      pure (JobConj (FishJobConjunction Nothing lp [JCOr rp]))
    _ ->
      pure (Command "true" [])

translateConditionTokenM :: Token -> TranslateM (FishCommand TStatus)
translateConditionTokenM = \case
  TC_Group _ _ inner -> translateConditionTokenM inner
  TC_And _ _ _ l r -> do
    lp <- pipelineOf <$> translateConditionTokenM l
    rp <- pipelineOf <$> translateConditionTokenM r
    pure (JobConj (FishJobConjunction Nothing lp [JCAnd rp]))
  TC_Or _ _ _ l r -> do
    lp <- pipelineOf <$> translateConditionTokenM l
    rp <- pipelineOf <$> translateConditionTokenM r
    pure (JobConj (FishJobConjunction Nothing lp [JCOr rp]))
  TC_Unary _ _ "!" inner ->
    Not <$> translateConditionTokenM inner
  TC_Unary _ _ op inner -> do
    (pre, expr) <- translateTokenToExprM inner
    let cmd = Command "test" [ExprVal (ExprLiteral (toText op)), ExprVal expr]
    pure (wrapPrelude pre cmd)
  TC_Binary _ _ op lhs rhs -> do
    (preL, lhsExpr) <- translateTokenToExprM lhs
    (preR, rhsExpr) <- translateTokenToExprM rhs
    let cmd = translateBinaryConditionExprs (toText op) lhsExpr rhsExpr
    pure (wrapPrelude (preL <> preR) cmd)
  TC_Nullary _ _ tok -> do
    (pre, expr) <- translateTokenToExprM tok
    pure (wrapPrelude pre (Command "test" [ExprVal expr]))
  TC_Empty {} ->
    pure (Command "true" [])
  other ->
    pure (Command "test" [ExprVal (ExprLiteral (tokenToLiteralText other))])

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

translateBinaryConditionExprs :: Text -> FishExpr TStr -> FishExpr TStr -> FishCommand TStatus
translateBinaryConditionExprs op lhs rhs
  | op == "=~" = stringMatch "-qr"
  | op == "==" || op == "=" = stringMatch "-q"
  | op == "!=" = Not (stringMatch "-q")
  | otherwise =
      let testArgs =
            [ ExprVal lhs,
              ExprVal (ExprLiteral op),
              ExprVal rhs
            ]
       in Command "test" testArgs
  where
    stringMatch flag =
      Command
        "string"
        [ ExprVal (ExprLiteral "match"),
          ExprVal (ExprLiteral flag),
          ExprVal (ExprLiteral "--"),
          ExprVal rhs,
          ExprVal lhs
        ]

wrapPrelude :: [FishStatement] -> FishCommand TStatus -> FishCommand TStatus
wrapPrelude [] cmd = cmd
wrapPrelude pre cmd =
  case NE.nonEmpty (pre <> [Stmt cmd]) of
    Just body -> Begin body []
    Nothing -> cmd

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

isSeparatorToken :: Token -> Bool
isSeparatorToken tok =
  let txt = tokenToLiteralText tok
   in txt == ";" || txt == "\n"

translatePipelineToStatusM :: [Token] -> [Token] -> TranslateM (FishCommand TStatus)
translatePipelineToStatusM bang cmds = do
  let (timed, cmds') = stripTimePrefix cmds
  mCmds <- mapM translateTokenToMaybeStatusCmdM cmds'
  case catMaybes mCmds of
    [] -> pure (Command "true" [])
    (c : cs) ->
      let pipe = Pipeline (jobPipelineFromListWithTime timed (c : cs))
       in pure (if hasBang bang then Not pipe else pipe)

translateTokenToMaybeStatusCmdM :: Token -> TranslateM (Maybe (FishCommand TStatus))
translateTokenToMaybeStatusCmdM token =
  case token of
    T_SimpleCommand {} -> do
      cmd <- translateTokenToStatusCmdM token
      pure (Just cmd)
    T_Condition {} -> Just <$> translateTokenToStatusCmdM token
    T_Redirecting _ _ inner -> translateTokenToMaybeStatusCmdM inner
    T_Pipeline _ bang cmds -> Just <$> translatePipelineToStatusM bang cmds
    T_AndIf _ l r -> do
      lp <- pipelineOf <$> translateTokenToStatusCmdM l
      rp <- pipelineOf <$> translateTokenToStatusCmdM r
      pure (Just (JobConj (FishJobConjunction Nothing lp [JCAnd rp])))
    T_OrIf _ l r -> do
      lp <- pipelineOf <$> translateTokenToStatusCmdM l
      rp <- pipelineOf <$> translateTokenToStatusCmdM r
      pure (Just (JobConj (FishJobConjunction Nothing lp [JCOr rp])))
    _ -> pure Nothing
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
           in if hasBang bang then Not pipe else pipe

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

hasBang :: [Token] -> Bool
hasBang = any (\tok -> tokenToLiteralText tok == "!")

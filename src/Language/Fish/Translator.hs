{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator
  ( translateRoot
  , translateToken
  , translateRootWithPositions
  , translateParseResult
  ) where

import Prelude hiding (show)
import qualified Data.List.NonEmpty as NE
import Data.List (lookup)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Char (isAlpha, isAlphaNum)
import Control.Monad (foldM)
import Language.Fish.AST
import ShellCheck.AST
import ShellCheck.Interface (ParseResult(..), Position)
import GHC.Show (show)
import Language.Fish.Translator.Variables
import Language.Fish.Translator.Commands
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
  , addLocalVars
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
        translateForArithmetic initTok condTok incTok body

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

      T_CoProc {} -> pure (Comment "Unsupported: Coprocess (coproc)")

      T_Redirecting _ redirs cmd -> do
        let redirExprs = mapMaybe translateRedirectToken redirs
        translated <- translateToken cmd
        pure (attachRedirs redirExprs translated)

      _ -> pure (Comment ("Skipped token at statement level: " <> T.pack (show token)))

wrapStmtList :: [FishStatement] -> FishStatement
wrapStmtList [stmt] = stmt
wrapStmtList stmts  = StmtList stmts

scopeFlagsFor :: Set.Set Text -> Text -> [SetFlag]
scopeFlagsFor locals name =
  if Set.member name locals
    then [SetLocal]
    else [SetGlobal]

translateScopedAssignment :: Set.Set Text -> Token -> [FishStatement]
translateScopedAssignment locals tok =
  case tok of
    T_Assignment _ _ var _ _ ->
      translateAssignmentWithFlags (scopeFlagsFor locals (toText var)) tok
    _ ->
      translateAssignmentWithFlags (scopeFlagsFor locals "") tok

translateLocalCommand :: [Token] -> TranslateM FishStatement
translateLocalCommand args = do
  inFunc <- gets (inFunction . context)
  when (not inFunc) $
    addWarning "local used outside a function; fish will treat it as local to the current scope"
  parsed <- mapM parseLocalArg args
  let names = map fst (catMaybes parsed)
      stmts = concatMap snd (catMaybes parsed)
  addLocalVars names
  pure $ if null stmts
    then Comment "Skipped local with no assignments"
    else wrapStmtList stmts

translateExportCommand :: [Token] -> TranslateM FishStatement
translateExportCommand args = do
  locals <- gets (localVars . context)
  parsed <- mapM (parseExportArg locals) args
  let stmts = concat parsed
  pure $ if null stmts
    then Comment "Skipped export with no assignments"
    else wrapStmtList stmts

data DeclareFlags = DeclareFlags
  { declareGlobal :: Bool
  , declareExport :: Bool
  , declareReadonly :: Bool
  } deriving stock (Show, Eq)

defaultDeclareFlags :: DeclareFlags
defaultDeclareFlags = DeclareFlags False False False

translateDeclareCommand :: [Token] -> TranslateM FishStatement
translateDeclareCommand =
  translateDeclareCommandWith defaultDeclareFlags

translateReadonlyCommand :: [Token] -> TranslateM FishStatement
translateReadonlyCommand =
  translateDeclareCommandWith (defaultDeclareFlags { declareReadonly = True })

translateDeclareCommandWith :: DeclareFlags -> [Token] -> TranslateM FishStatement
translateDeclareCommandWith baseFlags args = do
  inFunc <- gets (inFunction . context)
  (flags, rest) <- parseDeclareFlags baseFlags args
  when (declareReadonly flags) $
    addWarning "readonly/declare -r has no direct fish equivalent; emitted set without enforcing readonly"
  let scopeFlags = declareScopeFlags inFunc flags
  if null rest
    then pure (Stmt (Command "set" []))
    else do
      parsed <- mapM (parseDeclareArg scopeFlags) rest
      let names = mapMaybe fst parsed
          stmts = concatMap snd parsed
      when (SetLocal `elem` scopeFlags) (addLocalVars names)
      pure $ if null stmts
        then Comment "Skipped declare with no supported arguments"
        else wrapStmtList stmts

declareScopeFlags :: Bool -> DeclareFlags -> [SetFlag]
declareScopeFlags inFunc flags =
  let scope =
        if declareGlobal flags || not inFunc
          then SetGlobal
          else SetLocal
      base = [scope]
  in if declareExport flags
      then base <> [SetExport]
      else base

parseDeclareFlags :: DeclareFlags -> [Token] -> TranslateM (DeclareFlags, [Token])
parseDeclareFlags base toks = go base toks
  where
    go acc [] = pure (acc, [])
    go acc (t:ts) =
      let txt = tokenToLiteralText t
      in if txt == "--"
           then pure (acc, ts)
           else if T.isPrefixOf "-" txt && T.length txt > 1
             then do
               acc' <- foldM applyDeclareFlag acc (T.unpack (T.drop 1 txt))
               go acc' ts
             else pure (acc, t:ts)

    applyDeclareFlag flags c =
      case c of
        'g' -> pure flags { declareGlobal = True }
        'x' -> pure flags { declareExport = True }
        'r' -> pure flags { declareReadonly = True }
        'a' -> pure flags
        'A' -> addWarnFlag flags "declare -A (associative arrays)"
        'i' -> addWarnFlag flags "declare -i (integer attributes)"
        _ -> addWarnFlag flags ("declare flag -" <> T.singleton c)

    addWarnFlag flags msg = do
      addWarning ("Unsupported " <> msg <> "; ignoring flag")
      pure flags

parseDeclareArg :: [SetFlag] -> Token -> TranslateM (Maybe Text, [FishStatement])
parseDeclareArg flags tok =
  case tok of
    T_Assignment _ _ var _ _ ->
      let name = toText var
          stmts = translateAssignmentWithFlags flags tok
      in pure (Just name, stmts)
    _ -> do
      let txt = tokenToLiteralText tok
      case parseAssignmentLiteral txt of
        Just (name, valueTxt) ->
          let expr = ExprListLiteral [ExprLiteral valueTxt]
          in pure (Just name, [Stmt (Set flags name expr)])
        Nothing ->
          if isValidVarName txt
            then pure (Just txt, [Stmt (Set flags txt (ExprListLiteral []))])
            else do
              addWarning ("Unsupported declare argument: " <> txt)
              pure (Nothing, [])

translateShiftCommand :: [Token] -> TranslateM FishStatement
translateShiftCommand args =
  case args of
    [] -> pure (shiftByCount 1)
    [tok] ->
      case parseShiftCount tok of
        Just n | n >= 0 -> pure (shiftByCount n)
        Just _ -> do
          addWarning "shift count must be non-negative; emitting comment"
          pure (Comment "Unsupported shift count")
        Nothing -> do
          addWarning "Unsupported shift argument; emitting comment"
          pure (Comment "Unsupported shift argument")
    _ -> do
      addWarning "shift with multiple arguments is not supported; emitting comment"
      pure (Comment "Unsupported shift arguments")

shiftByCount :: Int -> FishStatement
shiftByCount n =
  let start = ExprNumLiteral (n + 1)
      range = IndexRange (Just start) (Just (ExprNumLiteral (-1)))
      expr = ExprVariable (VarIndex "argv" range)
  in Stmt (Set [] "argv" expr)

parseShiftCount :: Token -> Maybe Int
parseShiftCount tok =
  case tok of
    T_Literal _ s -> readMaybe (toString (toText s))
    _ -> readMaybe (toString (tokenToLiteralText tok))

translateTrapCommand :: [Token] -> TranslateM FishStatement
translateTrapCommand args =
  case args of
    [] -> do
      addWarning "trap with no arguments is not supported"
      pure (Stmt (Command "trap" []))
    (cmdTok:signalToks) -> do
      let cmdExpr = translateTokenToExpr cmdTok
          rawSignals = map tokenToLiteralText signalToks
      if any isTrapOption rawSignals
        then do
          addWarning "trap options are not supported; emitting raw trap command"
          pure (Stmt (Command "trap" (map translateTokenToExprOrRedirect args)))
        else do
          let signals = if null rawSignals then ["EXIT"] else rawSignals
              trapStmts = map (trapForSignal cmdExpr) signals
          pure (wrapStmtList (map Stmt trapStmts))
  where
    isTrapOption sig = sig `elem` ["-p", "-l", "--"]

    trapForSignal :: FishExpr TStr -> Text -> FishCommand TStatus
    trapForSignal cmd sig
      | isExitSignal sig =
          Command "trap" [ExprVal (ExprLiteral "--on-exit"), ExprVal cmd]
      | otherwise =
          Command "trap"
            [ ExprVal (ExprLiteral "--on-signal")
            , ExprVal (ExprLiteral (normalizeSignal sig))
            , ExprVal cmd
            ]

    isExitSignal sig =
      let upper = T.toUpper sig
      in upper == "EXIT" || upper == "0"

    normalizeSignal sig =
      let upper = T.toUpper sig
      in fromMaybe upper (T.stripPrefix "SIG" upper)

parseLocalArg :: Token -> TranslateM (Maybe (Text, [FishStatement]))
parseLocalArg tok =
  case tok of
    T_Assignment _ _ var _ _ ->
      let name = toText var
          stmts = translateAssignmentWithFlags [SetLocal] tok
      in pure (Just (name, stmts))
    _ -> do
      let txt = tokenToLiteralText tok
      case parseAssignmentLiteral txt of
        Just (name, valueTxt) ->
          let expr = ExprListLiteral [ExprLiteral valueTxt]
          in pure (Just (name, [Stmt (Set [SetLocal] name expr)]))
        Nothing ->
          if isValidVarName txt
            then pure (Just (txt, [Stmt (Set [SetLocal] txt (ExprListLiteral []))]))
            else do
              addWarning ("Unsupported local argument: " <> txt)
              pure Nothing

parseExportArg :: Set.Set Text -> Token -> TranslateM [FishStatement]
parseExportArg locals tok =
  case tok of
    T_Assignment _ _ var _ _ ->
      let name = toText var
          flags = exportFlags locals name
      in pure (translateAssignmentWithFlags flags tok)
    _ -> do
      let txt = tokenToLiteralText tok
      case parseAssignmentLiteral txt of
        Just (name, valueTxt) ->
          let expr = ExprListLiteral [ExprLiteral valueTxt]
          in pure [Stmt (Set (exportFlags locals name) name expr)]
        Nothing ->
          if isValidVarName txt
            then pure [Stmt (Set (exportFlags locals txt) txt (ExprVariable (VarAll txt)))]
            else do
              addWarning ("Unsupported export argument: " <> txt)
              pure []

exportFlags :: Set.Set Text -> Text -> [SetFlag]
exportFlags locals name =
  if Set.member name locals
    then [SetLocal, SetExport]
    else [SetGlobal, SetExport]

parseAssignmentLiteral :: Text -> Maybe (Text, Text)
parseAssignmentLiteral txt = do
  let trimmed = T.strip txt
  guard (not (T.null trimmed))
  let (lhs, rest) = T.breakOn "=" trimmed
  guard (T.isPrefixOf "=" rest)
  guard (isValidVarName lhs)
  pure (lhs, T.drop 1 rest)

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

--------------------------------------------------------------------------------
-- Arithmetic for-loop translation helpers
--------------------------------------------------------------------------------

translateForArithmetic :: Token -> Token -> Token -> [Token] -> TranslateM FishStatement
translateForArithmetic initTok condTok incTok body = do
  bodyStmts <- mapM translateToken body
  initStmt <- case parseForInit initTok of
    Just stmt -> pure stmt
    Nothing -> do
      addWarning "Unsupported arithmetic init; emitting comment"
      pure (Comment "Unsupported arithmetic init")
  let condCmd = fromMaybe (Command "false" []) (parseForCond condTok)
  incStmt <- case parseForInc incTok of
    Just stmt -> pure stmt
    Nothing -> do
      addWarning "Unsupported arithmetic increment; emitting comment"
      pure (Comment "Unsupported arithmetic increment")
  let loopBody = fromMaybe (Comment "Empty arithmetic loop body" NE.:| [])
        (Control.toNonEmptyStmtList (bodyStmts <> [incStmt]))
      condJob = FishJobList (FishJobConjunction Nothing (FIO.pipelineOf condCmd) [] NE.:| [])
      whileStmt = Stmt (While condJob loopBody [])
      initBlock = [initStmt, whileStmt]
  pure $ case Control.toNonEmptyStmtList initBlock of
    Just neBody -> Stmt (Begin neBody [])
    Nothing -> Comment "Skipped arithmetic for loop"

parseForInit :: Token -> Maybe FishStatement
parseForInit tok =
  parseForInitToken tok <|> parseForInitText tok

parseForInitToken :: Token -> Maybe FishStatement
parseForInitToken = \case
  TA_Assignment _ "=" lhs rhs -> do
    var <- arithVarName lhs
    let expr = mathSubstFromArgs (arithArgsFromToken rhs)
    pure (Stmt (Set [SetGlobal] var expr))
  TA_Sequence _ (t:_) -> parseForInitToken t
  TA_Parenthesis _ t -> parseForInitToken t
  _ -> Nothing

parseForInitText :: Token -> Maybe FishStatement
parseForInitText tok = do
  let txt = T.strip (tokenToLiteralText tok)
  (var, exprTxt) <- parseAssignmentText txt
  let expr = mathSubstFromText exprTxt
  pure (Stmt (Set [SetGlobal] var expr))

parseForCond :: Token -> Maybe (FishCommand TStatus)
parseForCond tok =
  parseForCondToken tok <|> parseForCondText tok

parseForCondToken :: Token -> Maybe (FishCommand TStatus)
parseForCondToken tok =
  case tok of
    TA_Binary _ op lhs rhs ->
      case lookup (T.pack op) testOpMap of
        Just testOp ->
          let lhsExpr = arithValueExpr lhs
              rhsExpr = arithValueExpr rhs
          in Just (Command "test" [ExprVal lhsExpr, ExprVal (ExprLiteral testOp), ExprVal rhsExpr])
        Nothing -> Just (arithNonZeroCond tok)
    TA_Sequence _ (t:_) -> parseForCondToken t
    TA_Parenthesis _ t -> parseForCondToken t
    _ -> Just (arithNonZeroCond tok)

parseForCondText :: Token -> Maybe (FishCommand TStatus)
parseForCondText tok =
  let txt = T.strip (tokenToLiteralText tok)
  in parseBinaryCond txt <|> parseMathCond txt

parseBinaryCond :: Text -> Maybe (FishCommand TStatus)
parseBinaryCond txt = do
  (lhs, op, rhs) <- parseComparison txt
  testOp <- lookup op testOpMap
  let lhsExpr = valueExpr lhs
      rhsExpr = valueExpr rhs
  pure (Command "test" [ExprVal lhsExpr, ExprVal (ExprLiteral testOp), ExprVal rhsExpr])

testOpMap :: [(Text, Text)]
testOpMap =
  [ ("<=", "-le")
  , (">=", "-ge")
  , ("==", "-eq")
  , ("!=", "-ne")
  , ("<", "-lt")
  , (">", "-gt")
  ]

parseMathCond :: Text -> Maybe (FishCommand TStatus)
parseMathCond txt = do
  args <- arithArgsFromText txt
  let expr = ExprMath args
  pure (Command "test" [ExprVal expr, ExprVal (ExprLiteral "-ne"), ExprVal (ExprLiteral "0")])

arithNonZeroCond :: Token -> FishCommand TStatus
arithNonZeroCond tok =
  let expr = ExprMath (arithArgsFromToken tok)
  in Command "test" [ExprVal expr, ExprVal (ExprLiteral "-ne"), ExprVal (ExprLiteral "0")]

arithVarName :: Token -> Maybe Text
arithVarName = \case
  TA_Variable _ name _ -> Just (T.pack name)
  TA_Expansion _ inner -> listToMaybe inner >>= arithVarName
  _ -> Nothing

arithValueExpr :: Token -> FishExpr TInt
arithValueExpr tok = ExprMath (arithArgsFromToken tok)

parseForInc :: Token -> Maybe FishStatement
parseForInc tok =
  parseIncToken tok <|> parseIncText (T.strip (tokenToLiteralText tok))

parseIncToken :: Token -> Maybe FishStatement
parseIncToken = \case
  TA_Unary _ "++" inner -> incByVar inner "+" (ExprLiteral "1" NE.:| [])
  TA_Unary _ "--" inner -> incByVar inner "-" (ExprLiteral "1" NE.:| [])
  TA_Assignment _ "+=" lhs rhs -> incByVar lhs "+" (arithArgsFromToken rhs)
  TA_Assignment _ "-=" lhs rhs -> incByVar lhs "-" (arithArgsFromToken rhs)
  TA_Sequence _ (t:_) -> parseIncToken t
  TA_Parenthesis _ t -> parseIncToken t
  _ -> Nothing

parseIncText :: Text -> Maybe FishStatement
parseIncText txt
  | Just var <- stripPrefixOp "++" txt = Just (incByArgs var "+" (ExprLiteral "1" NE.:| []))
  | Just var <- stripPrefixOp "--" txt = Just (incByArgs var "-" (ExprLiteral "1" NE.:| []))
  | Just var <- stripSuffixOp "++" txt = Just (incByArgs var "+" (ExprLiteral "1" NE.:| []))
  | Just var <- stripSuffixOp "--" txt = Just (incByArgs var "-" (ExprLiteral "1" NE.:| []))
  | Just (var, rhs) <- stripInfixOp "+=" txt = Just (incByArgs var "+" (rhsExpr rhs NE.:| []))
  | Just (var, rhs) <- stripInfixOp "-=" txt = Just (incByArgs var "-" (rhsExpr rhs NE.:| []))
  | otherwise = Nothing
  where
    rhsExpr rhs =
      if isValidVarName (T.strip rhs)
        then ExprVariable (VarScalar (T.strip rhs))
        else ExprLiteral (T.strip rhs)

incByVar :: Token -> Text -> NonEmpty (FishExpr TStr) -> Maybe FishStatement
incByVar lhs op rhsArgs = do
  var <- arithVarName lhs
  pure (incByArgs var op rhsArgs)

incByArgs :: Text -> Text -> NonEmpty (FishExpr TStr) -> FishStatement
incByArgs var op rhsArgs =
  Stmt (Set [SetGlobal] var (mathSubstFromArgs (ExprVariable (VarScalar var) NE.:| (ExprLiteral op : NE.toList rhsArgs))))

parseAssignmentText :: Text -> Maybe (Text, Text)
parseAssignmentText txt =
  let (lhs, rest) = T.breakOn "=" txt
      rhs = T.drop 1 rest
  in if T.isPrefixOf "=" rest && isValidVarName lhs && not (T.null rhs)
      then Just (lhs, rhs)
      else Nothing

parseComparison :: Text -> Maybe (Text, Text, Text)
parseComparison txt =
  let ops = ["<=", ">=", "==", "!=", "<", ">"]
      trimmed = T.strip txt
  in firstJust (map (splitOnOp trimmed) ops)
  where
    splitOnOp t op =
      case T.breakOn op t of
        (_, rhs) | T.null rhs -> Nothing
        (lhs, rhs) ->
          let rhs' = T.drop (T.length op) rhs
          in if T.null (T.strip lhs) || T.null (T.strip rhs')
              then Nothing
              else Just (T.strip lhs, op, T.strip rhs')

valueExpr :: Text -> FishExpr TStr
valueExpr txt =
  let trimmed = T.strip txt
  in if isValidVarName trimmed
       then ExprVariable (VarScalar trimmed)
       else ExprLiteral trimmed

mathSubstFromText :: Text -> FishExpr (TList TStr)
mathSubstFromText txt =
  fromMaybe (ExprListLiteral [ExprLiteral (T.strip txt)]) (mathSubstFromTextMaybe txt)

mathSubstFromTextMaybe :: Text -> Maybe (FishExpr (TList TStr))
mathSubstFromTextMaybe txt = do
  args <- arithArgsFromText (T.strip txt)
  pure (mathSubstFromArgs args)

mathSubstFromArgs :: NonEmpty (FishExpr TStr) -> FishExpr (TList TStr)
mathSubstFromArgs args =
  ExprCommandSubst (Stmt (Command "math" (map ExprVal (NE.toList args))) NE.:| [])

stripPrefixOp :: Text -> Text -> Maybe Text
stripPrefixOp op txt = do
  rest <- T.stripPrefix op (T.strip txt)
  let var = T.strip rest
  guard (isValidVarName var)
  pure var

stripSuffixOp :: Text -> Text -> Maybe Text
stripSuffixOp op txt = do
  let trimmed = T.strip txt
  guard (T.isSuffixOf op trimmed)
  let var = T.dropEnd (T.length op) trimmed
  guard (isValidVarName var)
  pure var

stripInfixOp :: Text -> Text -> Maybe (Text, Text)
stripInfixOp op txt = do
  let trimmed = T.strip txt
  (lhs, rhs) <- case T.breakOn op trimmed of
    (_, "") -> Nothing
    (l, r) -> Just (l, T.drop (T.length op) r)
  guard (isValidVarName (T.strip lhs))
  guard (not (T.null (T.strip rhs)))
  pure (T.strip lhs, T.strip rhs)

isValidVarName :: Text -> Bool
isValidVarName name =
  case T.uncons name of
    Just (c, rest) | isAlpha c || c == '_' -> T.all isIdentChar rest
    _ -> False
  where
    isIdentChar ch = isAlphaNum ch || ch == '_'

firstJust :: [Maybe a] -> Maybe a
firstJust = listToMaybe . catMaybes

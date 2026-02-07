{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.ForArithmetic
  ( translateForArithmetic,
  )
where

import Data.List (lookup)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Language.Fish.AST
import Language.Fish.Translator.Control qualified as Control
import Language.Fish.Translator.IO qualified as FIO
import Language.Fish.Translator.Monad (TranslateM, addWarning)
import Language.Fish.Translator.Names (isValidVarName)
import Language.Fish.Translator.Variables
import ShellCheck.AST

translateForArithmetic ::
  (Token -> TranslateM FishStatement) ->
  Token ->
  Token ->
  Token ->
  [Token] ->
  TranslateM FishStatement
translateForArithmetic translateStmt initTok condTok incTok body = do
  bodyStmts <- mapM translateStmt body
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
  let loopBody =
        fromMaybe
          (Comment "Empty arithmetic loop body" NE.:| [])
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
  TA_Sequence _ (t : _) -> parseForInitToken t
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
    TA_Sequence _ (t : _) -> parseForCondToken t
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
  [ ("<=", "-le"),
    (">=", "-ge"),
    ("==", "-eq"),
    ("!=", "-ne"),
    ("<", "-lt"),
    (">", "-gt")
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
  TA_Unary _ op inner
    | stripUnaryMarkers op == "++" -> incByVar inner "+" (ExprLiteral "1" NE.:| [])
  TA_Unary _ op inner
    | stripUnaryMarkers op == "--" -> incByVar inner "-" (ExprLiteral "1" NE.:| [])
  TA_Assignment _ "+=" lhs rhs -> incByVar lhs "+" (arithArgsFromToken rhs)
  TA_Assignment _ "-=" lhs rhs -> incByVar lhs "-" (arithArgsFromToken rhs)
  TA_Sequence _ (t : _) -> parseIncToken t
  TA_Parenthesis _ t -> parseIncToken t
  _ -> Nothing

stripUnaryMarkers :: String -> Text
stripUnaryMarkers op =
  T.dropAround (== '|') (toText op)

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

firstJust :: [Maybe a] -> Maybe a
firstJust = listToMaybe . catMaybes

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Variables
  ( translateTokenToExpr
  , translateTokenToListExpr
  , translateTokenToExprOrRedirect
  , translateAssignment
  , translateAssignmentWithFlags
  , translateArithmetic
  , arithArgsFromToken
  , arithArgsFromText
  , tokenToLiteralText
  ) where

import qualified Data.List.NonEmpty as NE
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import qualified Data.Text as T
import Language.Fish.AST
import ShellCheck.AST
import ShellCheck.ASTLib (getBracedModifier, getBracedReference, getLiteralStringDef, oversimplify)

-- | Utility: get literal text from a token if available
tokenToLiteralText :: Token -> Text
tokenToLiteralText = T.pack . getLiteralStringDef ""

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

translateTokenToExpr :: Token -> FishExpr TStr
translateTokenToExpr = \case
  T_Literal _ s -> ExprLiteral (T.pack s)
  T_Glob _ s -> ExprJoinList (ExprGlob (parseGlobPattern (toText s)))
  T_Extglob _ op parts ->
    case renderExtglobForFish op parts of
      Just pat -> ExprJoinList (ExprGlob (parseGlobPattern pat))
      Nothing -> ExprJoinList (extglobShimListExpr (renderExtglobRaw op parts))
  T_SingleQuoted _ s -> ExprLiteral (T.pack s)
  T_DoubleQuoted _ parts -> translateDoubleQuotedExpr parts
  T_NormalWord _ parts ->
    if wordIsGlob parts
      then
        if wordNeedsExtglobShim parts
          then ExprJoinList (extglobShimListExpr (renderGlobWordRaw parts))
          else ExprJoinList (ExprGlob (parseGlobPattern (renderGlobWord parts)))
      else translateWordPartsToExpr parts
  T_DollarBraced _ _ word -> ExprJoinList (translateDollarBraced word)
  T_DollarArithmetic _ exprTok ->
    ExprJoinList (ExprCommandSubst (Stmt (mathCommandFromToken False exprTok) NE.:| []))
  -- Arithmetic tokens: convert to a command substitution calling `math`
  T_Arithmetic _ exprTok ->
    ExprJoinList (ExprCommandSubst (Stmt (mathCommandFromToken True exprTok) NE.:| []))
  -- Backticks and $(...) command substitutions
  T_Backticked _ stmts ->
    commandSubstExprStr stmts
  T_DollarExpansion _ stmts ->
    commandSubstExprStr stmts
  -- ${ ... $(cmd) ... } style
  T_DollarBraceCommandExpansion _ _ stmts ->
    commandSubstExprStr stmts
  T_ProcSub _ dir stmts ->
    case NE.nonEmpty (map translateStmt stmts) of
      Just neBody -> procSubExpr dir neBody
      Nothing     -> ExprLiteral ""
  -- Fallback: take literal interpretation where possible
  other -> ExprLiteral (tokenToLiteralText other)
  where
    translateStmt = \t -> case t of
      T_Script _ _ ts -> StmtList (map translateStmt ts)
      _               -> Stmt (Command (tokenToLiteralText t) [])

translateTokenToListExpr :: Token -> FishExpr (TList TStr)
translateTokenToListExpr = \case
  T_Literal _ s -> ExprListLiteral [ExprLiteral (T.pack s)]
  T_Glob _ s -> ExprGlob (parseGlobPattern (toText s))
  T_Extglob _ op parts ->
    case renderExtglobForFish op parts of
      Just pat -> ExprGlob (parseGlobPattern pat)
      Nothing -> extglobShimListExpr (renderExtglobRaw op parts)
  T_SingleQuoted _ s -> ExprListLiteral [ExprLiteral (T.pack s)]
  T_DoubleQuoted _ parts -> ExprListLiteral [translateDoubleQuotedExpr parts]
  T_NormalWord _ parts ->
    case parts of
      [T_DollarBraced _ _ word] -> translateDollarBraced word
      _ | wordIsGlob parts ->
          if wordNeedsExtglobShim parts
            then extglobShimListExpr (renderGlobWordRaw parts)
            else ExprGlob (parseGlobPattern (renderGlobWord parts))
        | otherwise -> ExprListLiteral [translateWordPartsToExpr parts]
  T_DollarBraced _ _ word -> translateDollarBraced word
  T_DollarArithmetic _ exprTok ->
    ExprCommandSubst (Stmt (mathCommandFromToken False exprTok) NE.:| [])
  T_Arithmetic _ exprTok ->
    ExprCommandSubst (Stmt (mathCommandFromToken True exprTok) NE.:| [])
  T_Backticked _ stmts ->
    commandSubstExprList stmts
  T_DollarExpansion _ stmts ->
    commandSubstExprList stmts
  T_DollarBraceCommandExpansion _ _ stmts ->
    commandSubstExprList stmts
  T_ProcSub _ dir stmts ->
    case NE.nonEmpty (map translateStmt stmts) of
      Just neBody -> procSubListExpr dir neBody
      Nothing     -> ExprListLiteral []
  T_Array _ elems -> translateArrayElements elems
  other -> ExprListLiteral [ExprLiteral (tokenToLiteralText other)]
  where
    translateStmt = translateSubstToken

translateTokenToExprOrRedirect :: Token -> ExprOrRedirect
translateTokenToExprOrRedirect tok = ExprVal (translateTokenToListExpr tok)

wordIsGlob :: [Token] -> Bool
wordIsGlob parts =
  all isGlobToken parts && any hasGlobMeta parts
  where
    isGlobToken = \case
      T_Literal {} -> True
      T_Glob {} -> True
      T_Extglob {} -> True
      _ -> False

    hasGlobMeta = \case
      T_Literal _ s -> hasGlobChars (toText s)
      T_Glob {} -> True
      T_Extglob {} -> True
      _ -> False

    hasGlobChars = T.any (`elem` ("*?[]{}" :: String))

wordNeedsExtglobShim :: [Token] -> Bool
wordNeedsExtglobShim =
  any (\case
    T_Extglob _ op _ -> extglobNeedsShim op
    _ -> False)

extglobNeedsShim :: String -> Bool
extglobNeedsShim op = op /= "@"

renderGlobWord :: [Token] -> Text
renderGlobWord =
  foldMap $ \case
    T_Literal _ s -> toText s
    T_Glob _ s -> toText s
    T_Extglob _ op parts -> fromMaybe (renderExtglobRaw op parts) (renderExtglobForFish op parts)
    other -> tokenToLiteralText other

renderGlobWordRaw :: [Token] -> Text
renderGlobWordRaw =
  foldMap $ \case
    T_Literal _ s -> toText s
    T_Glob _ s -> toText s
    T_Extglob _ op parts -> renderExtglobRaw op parts
    other -> tokenToLiteralText other

--------------------------------------------------------------------------------
-- Assignments
--------------------------------------------------------------------------------

translateAssignment :: Token -> [FishStatement]
translateAssignment = translateAssignmentWithFlags [SetGlobal]

translateAssignmentWithFlags :: [SetFlag] -> Token -> [FishStatement]
translateAssignmentWithFlags baseFlags tok =
  case tok of
    T_Assignment _ mode var indices val ->
      let fishVar = T.pack var
          flags = case mode of
            Assign -> baseFlags
            Append -> baseFlags <> [SetAppend]
          indexedVar = indexedVarText fishVar indices
      in case indexedVar of
           Just varTxt ->
             [Stmt (Set flags varTxt (translateTokenToListExpr val))]
           Nothing ->
             case val of
               T_Array _ elems ->
                 translateArrayAssignment fishVar flags elems
               _ ->
                 [Stmt (Set flags fishVar (translateTokenToListExpr val))]
    _ -> []

--------------------------------------------------------------------------------
-- Arithmetic
--------------------------------------------------------------------------------

translateArithmetic :: Token -> FishCommand TStatus
translateArithmetic = mathCommandFromToken True

mathCommandFromToken :: Bool -> Token -> FishCommand TStatus
mathCommandFromToken suppressOutput exprToken =
  let args = map ExprVal (NE.toList (arithArgsFromToken exprToken))
      mathRedir = RedirectVal (Redirect RedirectStdout RedirectOut (RedirectFile (ExprLiteral "/dev/null")))
      allArgs = if suppressOutput then args <> [mathRedir] else args
  in Command "math" allArgs

arithArgsFromToken :: Token -> NonEmpty (FishExpr TStr)
arithArgsFromToken exprToken =
  fromMaybe (ExprLiteral "0" NE.:| []) (NE.nonEmpty (arithArgsFromTokenList exprToken))

arithArgsFromTokenList :: Token -> [FishExpr TStr]
arithArgsFromTokenList = \case
  TA_Sequence _ ts -> concatMap arithArgsFromTokenList ts
  TA_Expansion _ ts -> concatMap arithArgsFromTokenList ts
  TA_Parenthesis _ t -> wrapParens (arithArgsFromTokenList t)
  TA_Unary _ op t -> ExprLiteral (toText op) : arithArgsFromTokenList t
  TA_Binary _ op l r ->
    wrapParens (arithArgsFromTokenList l <> [ExprLiteral (toText op)] <> arithArgsFromTokenList r)
  TA_Trinary _ a b c ->
    wrapParens (arithArgsFromTokenList a <> [ExprLiteral "?"] <> arithArgsFromTokenList b <> [ExprLiteral ":"] <> arithArgsFromTokenList c)
  TA_Assignment _ op l r ->
    wrapParens (arithArgsFromTokenList l <> [ExprLiteral (toText op)] <> arithArgsFromTokenList r)
  TA_Variable _ name _ -> [ExprVariable (VarScalar (toText name))]
  T_Literal _ s -> [ExprLiteral (toText s)]
  T_SingleQuoted _ s -> [ExprLiteral (toText s)]
  tok ->
    let txt = tokenToLiteralText tok
    in [ExprLiteral txt]

wrapParens :: [FishExpr TStr] -> [FishExpr TStr]
wrapParens exprs = [ExprLiteral "("] <> exprs <> [ExprLiteral ")"]

arithArgsFromText :: Text -> Maybe (NonEmpty (FishExpr TStr))
arithArgsFromText txt =
  NE.nonEmpty (map arithTokenToExpr (tokenizeArithText txt))

arithTokenToExpr :: Text -> FishExpr TStr
arithTokenToExpr tok
  | Just name <- stripDollarVar tok = ExprVariable (VarScalar name)
  | isVarToken tok = ExprVariable (VarScalar tok)
  | otherwise = ExprLiteral tok

stripDollarVar :: Text -> Maybe Text
stripDollarVar tok =
  case T.uncons tok of
    Just ('$', rest) | isVarToken rest -> Just rest
    _ -> Nothing

isVarToken :: Text -> Bool
isVarToken tok =
  case T.uncons tok of
    Just (c, _) | isAlpha c || c == '_' -> T.all isIdentChar tok
    _ -> False
  where
    isIdentChar ch = isAlphaNum ch || ch == '_'

tokenizeArithText :: Text -> [Text]
tokenizeArithText txt = go (T.unpack txt) []
  where
    go [] acc = reverse acc
    go (c:cs) acc
      | isSpace c = go cs acc
      | c == '$' =
          let (name, rest) = span isIdentChar cs
              tok = if null name then "$" else '$' : name
          in go rest (toText tok : acc)
      | isAlpha c || c == '_' =
          let (ident, rest) = span isIdentChar cs
          in go rest (toText (c : ident) : acc)
      | isDigit c =
          let (digits, rest) = span isDigit cs
          in go rest (toText (c : digits) : acc)
      | otherwise =
          case cs of
            (d:ds)
              | isTwoCharOp c d ->
                  go ds (toText [c, d] : acc)
              | otherwise ->
                  go cs (toText [c] : acc)
            [] -> go cs (toText [c] : acc)

    isIdentChar ch = isAlphaNum ch || ch == '_'
    isTwoCharOp a b = [a, b] `elem`
      [ "++"
      , "--"
      , "+="
      , "-="
      , "*="
      , "/="
      , "%="
      , "<<"
      , ">>"
      , "<="
      , ">="
      , "=="
      , "!="
      , "&&"
      , "||"
      ]

--------------------------------------------------------------------------------
-- Parameter expansion helpers
--------------------------------------------------------------------------------

translateDollarBraced :: Token -> FishExpr (TList TStr)
translateDollarBraced word =
  case splitParamOperator word of
    Nothing ->
      case translateModifierExpansion word of
        Just expr -> expr
        Nothing -> translateSimpleVar word
    Just (op, rest) ->
      case op of
        ":-" -> translateDefaultExpansion word rest
        "-"  -> translateDefaultExpansionUnset word rest
        ":=" -> translateAssignDefaultExpansion word rest
        "="  -> translateAssignDefaultExpansionUnset word rest
        ":?" -> translateErrorExpansion word rest
        "?"  -> translateErrorExpansionUnset word rest
        ":+" -> translateAltExpansion word rest
        "+"  -> translateAltExpansionUnset word rest
        _ -> translateSimpleVar word

translateSimpleVar :: Token -> FishExpr (TList TStr)
translateSimpleVar word =
  case (paramNameFrom word, paramIndexFrom word) of
    (Just name, _) | name == "#" ->
      commandSubst (Stmt (Command "count" [ExprVal (ExprVariable (VarAll "argv"))]) NE.:| [])
    (Just name, Just idx) -> ExprVariable (VarIndex (specialVarName name) idx)
    (Just name, Nothing)
      | T.all isDigit name
      , Just n <- readMaybe (toString name) ->
          ExprVariable (VarIndex "argv" (IndexList (ExprNumLiteral n NE.:| [])))
      | otherwise -> ExprVariable (VarAll (specialVarName name))
    _ -> ExprListLiteral []

translateDefaultExpansion :: Token -> [Token] -> FishExpr (TList TStr)
translateDefaultExpansion word rest =
  let name = fromMaybe "" (paramNameFrom word)
      defaultExpr = translateTokensToListExpr rest
  in translateDefaultExpansionWith varNonEmptyCond name defaultExpr

translateDefaultExpansionUnset :: Token -> [Token] -> FishExpr (TList TStr)
translateDefaultExpansionUnset word rest =
  let name = fromMaybe "" (paramNameFrom word)
      defaultExpr = translateTokensToListExpr rest
  in translateDefaultExpansionWith varSetCond name defaultExpr

translateAssignDefaultExpansion :: Token -> [Token] -> FishExpr (TList TStr)
translateAssignDefaultExpansion word rest =
  let name = fromMaybe "" (paramNameFrom word)
      defaultExpr = translateTokensToListExpr rest
  in translateAssignDefaultExpansionWith varNonEmptyCond name defaultExpr

translateAssignDefaultExpansionUnset :: Token -> [Token] -> FishExpr (TList TStr)
translateAssignDefaultExpansionUnset word rest =
  let name = fromMaybe "" (paramNameFrom word)
      defaultExpr = translateTokensToListExpr rest
  in translateAssignDefaultExpansionWith varSetCond name defaultExpr

translateErrorExpansion :: Token -> [Token] -> FishExpr (TList TStr)
translateErrorExpansion word rest =
  let name = fromMaybe "" (paramNameFrom word)
      errExpr = translateTokensToListExpr rest
  in translateErrorExpansionWith varNonEmptyCond name errExpr

translateErrorExpansionUnset :: Token -> [Token] -> FishExpr (TList TStr)
translateErrorExpansionUnset word rest =
  let name = fromMaybe "" (paramNameFrom word)
      errExpr = translateTokensToListExpr rest
  in translateErrorExpansionWith varSetCond name errExpr

translateAltExpansion :: Token -> [Token] -> FishExpr (TList TStr)
translateAltExpansion word rest =
  let name = fromMaybe "" (paramNameFrom word)
      altExpr = translateTokensToListExpr rest
  in translateAltExpansionWith varNonEmptyCond name altExpr

translateAltExpansionUnset :: Token -> [Token] -> FishExpr (TList TStr)
translateAltExpansionUnset word rest =
  let name = fromMaybe "" (paramNameFrom word)
      altExpr = translateTokensToListExpr rest
  in translateAltExpansionWith varSetCond name altExpr

splitParamOperator :: Token -> Maybe (Text, [Token])
splitParamOperator word@(T_NormalWord _ parts) =
  case break isParamOp parts of
    (_, []) -> splitLiteralOperator word parts
    (_, T_ParamSubSpecialChar _ op : rest) ->
      let opTxt = toText op
      in if opTxt `elem` paramOps then Just (opTxt, rest) else Nothing
    _ -> splitLiteralOperator word parts
  where
    isParamOp (T_ParamSubSpecialChar _ _) = True
    isParamOp _ = False
    paramOps =
      [ ":-"
      , "-"
      , ":="
      , "="
      , ":?"
      , "?"
      , ":+"
      , "+"
      ]
    splitLiteralOperator word' parts' = do
      name <- paramNameFrom word'
      (literal, rest) <- case parts' of
        (T_Literal _ s : xs) -> Just (toText s, xs)
        _ -> Nothing
      op <- findOp name literal
      let prefix = name <> op
          suffix = T.drop (T.length prefix) literal
          suffixTokens =
            ([T_Literal (Id 0) (toString suffix) | not (T.null suffix)])
      pure (op, suffixTokens <> rest)
    findOp name literal =
      let matches op = T.isPrefixOf (name <> op) literal
      in find matches paramOps
splitParamOperator _ = Nothing

paramNameFrom :: Token -> Maybe Text
paramNameFrom word =
  let rawTxt = toText (concat (oversimplify word))
      nameTxt = toText (getBracedReference (toString rawTxt))
      fallback = T.takeWhile (/= '[') rawTxt
      finalName = if T.null nameTxt then fallback else nameTxt
  in if T.null finalName then Nothing else Just finalName

paramIndexFrom :: Token -> Maybe (FishIndex TStr (TList TStr))
paramIndexFrom word =
  let rawFull = toText (concat (oversimplify word))
      rawMod = toText (getBracedModifier (toString rawFull))
      idxRaw = if T.null rawMod
        then extractBracket rawFull
        else rawMod
  in case T.stripPrefix "[" idxRaw >>= T.stripSuffix "]" of
       Just idxTxt -> parseIndexSpec idxTxt
       _ -> Nothing
  where
    extractBracket t =
      case T.breakOn "[" t of
        (_, rest) | T.null rest -> ""
        _ ->
          let after = T.drop 1 (T.dropWhile (/= '[') t)
              inner = T.takeWhile (/= ']') after
          in "[" <> inner <> "]"

specialVarName :: Text -> Text
specialVarName = \case
  "?" -> "status"
  "$" -> "fish_pid"
  "!" -> "last_pid"
  "@" -> "argv"
  "*" -> "argv"
  n -> n

translateTokensToListExpr :: [Token] -> FishExpr (TList TStr)
translateTokensToListExpr [] = ExprListLiteral []
translateTokensToListExpr [t] = translateTokenToListExpr t
translateTokensToListExpr (t:ts) =
  foldl' ExprListConcat (translateTokenToListExpr t) (map translateTokenToListExpr ts)

varNonEmptyCond :: Text -> FishJobList
varNonEmptyCond name =
  let varExpr = ExprJoinList (ExprVariable (VarAll (specialVarName name)))
      setq = FishJobPipeline False [] (Stmt (Command "set"
        [ ExprVal (ExprLiteral "-q")
        , ExprVal (ExprLiteral (specialVarName name))
        ])) [] False
      test = FishJobPipeline False [] (Stmt (Command "test"
        [ ExprVal (ExprLiteral "-n")
        , ExprVal varExpr
        ])) [] False
  in FishJobList (FishJobConjunction Nothing setq [JCAnd test] NE.:| [])

varSetCond :: Text -> FishJobList
varSetCond name =
  let setq = FishJobPipeline False [] (Stmt (Command "set"
        [ ExprVal (ExprLiteral "-q")
        , ExprVal (ExprLiteral (specialVarName name))
        ])) [] False
  in FishJobList (FishJobConjunction Nothing setq [] NE.:| [])

emitList :: FishExpr (TList TStr) -> FishStatement
emitList expr =
  Stmt (Command "printf"
    [ ExprVal (ExprLiteral "%s\n")
    , ExprVal expr
    ])

commandSubst :: NonEmpty FishStatement -> FishExpr (TList TStr)
commandSubst = ExprCommandSubst

translateArrayElements :: [Token] -> FishExpr (TList TStr)
translateArrayElements elems =
  case elems of
    [] -> ExprListLiteral []
    _  ->
      case map elementToListExpr elems of
        (x:xs) -> foldl' ExprListConcat x xs
        [] -> ExprListLiteral []
  where
    elementToListExpr t = case t of
      T_IndexedElement _ _ v -> translateTokenToListExpr v
      _ -> translateTokenToListExpr t

translateArrayAssignment :: Text -> [SetFlag] -> [Token] -> [FishStatement]
translateArrayAssignment name flags elems =
  case indexedElements elems of
    [] ->
      [Stmt (Set flags name (translateArrayElements elems))]
    indexed ->
      map (indexedSet name) indexed
  where
    indexedElements = mapMaybe asIndexed
    asIndexed = \case
      T_IndexedElement _ indices value -> Just (indices, value)
      _ -> Nothing

    indexedSet var (idxTokens, value) =
      case indexedVarText var idxTokens of
        Just varTxt ->
          Stmt (Set flags varTxt (translateTokenToListExpr value))
        Nothing ->
          Stmt (Set flags var (translateTokenToListExpr value))

--------------------------------------------------------------------------------
-- Process substitution helpers
--------------------------------------------------------------------------------

procSubExpr :: String -> NonEmpty FishStatement -> FishExpr TStr
procSubExpr dir body =
  case dir of
    "<" -> ExprProcessSubst body
    ">" -> ExprJoinList (procSubOutList body)
    _ -> ExprProcessSubst body

procSubListExpr :: String -> NonEmpty FishStatement -> FishExpr (TList TStr)
procSubListExpr dir body =
  case dir of
    "<" -> ExprListLiteral [ExprProcessSubst body]
    ">" -> procSubOutList body
    _ -> ExprListLiteral [ExprProcessSubst body]

procSubOutList :: NonEmpty FishStatement -> FishExpr (TList TStr)
procSubOutList body =
  let fifoVar = "__monk_psub_fifo"
      mktempStmt = Stmt (Set [SetLocal] fifoVar
        (ExprCommandSubst (Stmt (Command "mktemp"
          [ ExprVal (ExprLiteral "-t")
          , ExprVal (ExprLiteral "monk_psub")
          ]) NE.:| [])))
      rmStmt = Stmt (Command "rm" [ExprVal (ExprVariable (VarAll fifoVar))])
      mkfifoStmt = Stmt (Command "mkfifo" [ExprVal (ExprVariable (VarAll fifoVar))])
      catStmt = Stmt (Command "cat" [ExprVal (ExprVariable (VarAll fifoVar))])
      rhsStmt = case NE.toList body of
        [s] -> s
        xs  -> Stmt (Begin (NE.fromList xs) [])
      pipe = FishJobPipeline False [] catStmt [PipeTo [] rhsStmt] False
      pipeStmt = Stmt (Pipeline pipe)
      bgBody = pipeStmt NE.:| [rmStmt]
      bgStmt = Stmt (Background (Begin bgBody []))
      echoStmt = Stmt (Command "echo" [ExprVal (ExprVariable (VarAll fifoVar))])
  in ExprCommandSubst (mktempStmt NE.:| [rmStmt, mkfifoStmt, bgStmt, echoStmt])

--------------------------------------------------------------------------------
-- Parameter expansion helpers
--------------------------------------------------------------------------------

translateDoubleQuotedExpr :: [Token] -> FishExpr TStr
translateDoubleQuotedExpr parts =
  case map translateTokenToExpr parts of
    [] -> ExprLiteral ""
    (x:xs) -> foldl' ExprStringConcat x xs

translateWordPartsToExpr :: [Token] -> FishExpr TStr
translateWordPartsToExpr = translateDoubleQuotedExpr

--------------------------------------------------------------------------------
-- Command substitution helpers (nested substitutions)
--------------------------------------------------------------------------------

commandSubstExprList :: [Token] -> FishExpr (TList TStr)
commandSubstExprList stmts =
  case NE.nonEmpty (map translateSubstToken stmts) of
    Just neBody -> ExprCommandSubst neBody
    Nothing -> ExprListLiteral []

commandSubstExprStr :: [Token] -> FishExpr TStr
commandSubstExprStr stmts =
  case NE.nonEmpty (map translateSubstToken stmts) of
    Just neBody -> ExprJoinList (ExprCommandSubst neBody)
    Nothing -> ExprLiteral ""

translateSubstToken :: Token -> FishStatement
translateSubstToken = \case
  T_Script _ _ ts -> StmtList (map translateSubstToken ts)
  T_SimpleCommand _ assignments cmdToks ->
    translateSubstSimpleCommand assignments cmdToks
  T_Pipeline _ bang cmds ->
    Stmt (translateSubstPipeline bang cmds)
  T_AndIf _ l r ->
    let lp = substPipelineOf (translateSubstStatusCmd l)
        rp = substPipelineOf (translateSubstStatusCmd r)
    in Stmt (JobConj (FishJobConjunction Nothing lp [JCAnd rp]))
  T_OrIf _ l r ->
    let lp = substPipelineOf (translateSubstStatusCmd l)
        rp = substPipelineOf (translateSubstStatusCmd r)
    in Stmt (JobConj (FishJobConjunction Nothing lp [JCOr rp]))
  T_Backgrounded _ tok ->
    Stmt (Background (translateSubstStatusCmd tok))
  T_BraceGroup _ tokens ->
    substBegin tokens
  T_Subshell _ tokens ->
    substBegin tokens
  T_Redirecting _ _ inner ->
    translateSubstToken inner
  T_Arithmetic _ exprTok ->
    Stmt (translateArithmetic exprTok)
  T_Condition _ _ condTok ->
    Stmt (translateSubstConditionToken condTok)
  other ->
    Stmt (Command (tokenToLiteralText other) [])

substBegin :: [Token] -> FishStatement
substBegin tokens =
  case NE.nonEmpty (map translateSubstToken tokens) of
    Just body -> Stmt (Begin body [])
    Nothing -> Comment "Skipped empty substitution block"

translateSubstSimpleCommand :: [Token] -> [Token] -> FishStatement
translateSubstSimpleCommand assignments cmdToks =
  let envFlags = [SetLocal, SetExport]
      envAssigns = concatMap (translateAssignmentWithFlags envFlags) assignments
      cmd = translateSubstCommandTokens cmdToks
  in case (envAssigns, cmd) of
      ([], Just fishCmd) -> Stmt fishCmd
      ([], Nothing) -> Comment "Skipped empty command in substitution"
      (_, Just fishCmd) ->
        case NE.nonEmpty (envAssigns ++ [Stmt fishCmd]) of
          Just body -> Stmt (Begin body [])
          Nothing -> Comment "Skipped empty command in substitution"
      (_, Nothing) ->
        case NE.nonEmpty envAssigns of
          Just body -> Stmt (Begin body [])
          Nothing -> Comment "Skipped empty command in substitution"

translateSubstCommandTokens :: [Token] -> Maybe (FishCommand TStatus)
translateSubstCommandTokens cmdTokens =
  case cmdTokens of
    [] -> Nothing
    (c:args) ->
      let name = tokenToLiteralText c
          argExprs = map translateTokenToExprOrRedirect args
      in if T.null name
           then Nothing
           else Just (Command name argExprs)

translateSubstStatusCmd :: Token -> FishCommand TStatus
translateSubstStatusCmd tok =
  case tok of
    T_SimpleCommand _ assignments cmdToks ->
      translateSubstCommandTokensToStatus assignments cmdToks
    T_Pipeline _ bang cmds ->
      translateSubstPipeline bang cmds
    T_Condition _ _ condTok ->
      translateSubstConditionToken condTok
    T_Redirecting _ _ inner ->
      translateSubstStatusCmd inner
    T_AndIf _ l r ->
      let lp = substPipelineOf (translateSubstStatusCmd l)
          rp = substPipelineOf (translateSubstStatusCmd r)
      in JobConj (FishJobConjunction Nothing lp [JCAnd rp])
    T_OrIf _ l r ->
      let lp = substPipelineOf (translateSubstStatusCmd l)
          rp = substPipelineOf (translateSubstStatusCmd r)
      in JobConj (FishJobConjunction Nothing lp [JCOr rp])
    _ -> Command "true" []

translateSubstCommandTokensToStatus :: [Token] -> [Token] -> FishCommand TStatus
translateSubstCommandTokensToStatus assignments cmdTokens =
  let baseCmd = fromMaybe (Command "true" []) (translateSubstCommandTokens cmdTokens)
  in if null assignments
       then baseCmd
       else
         let envFlags = [SetLocal, SetExport]
             envAssigns = concatMap (translateAssignmentWithFlags envFlags) assignments
         in case NE.nonEmpty (envAssigns ++ [Stmt baseCmd]) of
              Just body -> Begin body []
              Nothing -> baseCmd

translateSubstPipeline :: [Token] -> [Token] -> FishCommand TStatus
translateSubstPipeline bang cmds =
  case mapMaybe translateSubstTokenToMaybeStatusCmd cmds of
    [] -> Command "true" []
    (c:cs) ->
      let pipe = Pipeline (substJobPipelineFromList (c:cs))
      in if null bang then pipe else Not pipe

translateSubstTokenToMaybeStatusCmd :: Token -> Maybe (FishCommand TStatus)
translateSubstTokenToMaybeStatusCmd token =
  case token of
    T_SimpleCommand _ assignments rest ->
      Just (translateSubstCommandTokensToStatus assignments rest)
    T_Condition {} -> Just (translateSubstStatusCmd token)
    T_Redirecting _ _ inner -> translateSubstTokenToMaybeStatusCmd inner
    T_Pipeline _ bang cmds -> Just (translateSubstPipeline bang cmds)
    T_AndIf _ l r ->
      let lp = substPipelineOf (translateSubstStatusCmd l)
          rp = substPipelineOf (translateSubstStatusCmd r)
      in Just (JobConj (FishJobConjunction Nothing lp [JCAnd rp]))
    T_OrIf _ l r ->
      let lp = substPipelineOf (translateSubstStatusCmd l)
          rp = substPipelineOf (translateSubstStatusCmd r)
      in Just (JobConj (FishJobConjunction Nothing lp [JCOr rp]))
    _ -> Nothing

substJobPipelineFromList :: [FishCommand TStatus] -> FishJobPipeline
substJobPipelineFromList [] = substPipelineOf (Command "true" [])
substJobPipelineFromList (c:cs) =
  FishJobPipeline
    { jpTime = False
    , jpVariables = []
    , jpStatement = Stmt c
    , jpCont = map (\cmd -> PipeTo { jpcVariables = [], jpcStatement = Stmt cmd }) cs
    , jpBackgrounded = False
    }

substPipelineOf :: FishCommand TStatus -> FishJobPipeline
substPipelineOf cmd =
  FishJobPipeline { jpTime = False, jpVariables = [], jpStatement = Stmt cmd, jpCont = [], jpBackgrounded = False }

translateSubstConditionToken :: Token -> FishCommand TStatus
translateSubstConditionToken = \case
  TC_Group _ _ inner -> translateSubstConditionToken inner
  TC_And _ _ _ l r ->
    let lp = substPipelineOf (translateSubstConditionToken l)
        rp = substPipelineOf (translateSubstConditionToken r)
    in JobConj (FishJobConjunction Nothing lp [JCAnd rp])
  TC_Or _ _ _ l r ->
    let lp = substPipelineOf (translateSubstConditionToken l)
        rp = substPipelineOf (translateSubstConditionToken r)
    in JobConj (FishJobConjunction Nothing lp [JCOr rp])
  TC_Unary _ _ "!" inner ->
    Not (translateSubstConditionToken inner)
  TC_Unary _ _ op inner ->
    Command "test" [ExprVal (ExprLiteral (toText op)), ExprVal (translateTokenToExpr inner)]
  TC_Binary _ _ op lhs rhs ->
    translateSubstBinaryCondition (toText op) lhs rhs
  TC_Nullary _ _ tok ->
    Command "test" [ExprVal (translateTokenToExpr tok)]
  TC_Empty {} ->
    Command "true" []
  other ->
    Command "test" [ExprVal (ExprLiteral (tokenToLiteralText other))]

translateSubstBinaryCondition :: Text -> Token -> Token -> FishCommand TStatus
translateSubstBinaryCondition op lhs rhs
  | op == "=~" = stringMatch "-qr"
  | op == "==" || op == "=" = stringMatch "-q"
  | op == "!=" = Not (stringMatch "-q")
  | otherwise =
      let testArgs =
            [ ExprVal (translateTokenToExpr lhs)
            , ExprVal (ExprLiteral op)
            , ExprVal (translateTokenToExpr rhs)
            ]
      in Command "test" testArgs
  where
    stringMatch flag =
      Command "string"
        [ ExprVal (ExprLiteral "match")
        , ExprVal (ExprLiteral flag)
        , ExprVal (ExprLiteral "--")
        , ExprVal (translateTokenToExpr rhs)
        , ExprVal (translateTokenToExpr lhs)
        ]

translateDefaultExpansionWith :: (Text -> FishJobList) -> Text -> FishExpr (TList TStr) -> FishExpr (TList TStr)
translateDefaultExpansionWith condFn name defaultExpr =
  let cond = condFn name
      thenStmt = emitList (ExprVariable (VarAll (specialVarName name)))
      elseStmt = emitList defaultExpr
  in commandSubst (Stmt (If cond (thenStmt NE.:| []) [elseStmt] []) :| [])

translateAssignDefaultExpansionWith :: (Text -> FishJobList) -> Text -> FishExpr (TList TStr) -> FishExpr (TList TStr)
translateAssignDefaultExpansionWith condFn name defaultExpr =
  let cond = condFn name
      setStmt = Stmt (Set [SetLocal] (specialVarName name) defaultExpr)
      thenStmt = emitList (ExprVariable (VarAll (specialVarName name)))
      elseStmts = [setStmt, emitList (ExprVariable (VarAll (specialVarName name)))]
  in commandSubst (Stmt (If cond (thenStmt NE.:| []) elseStmts []) :| [])

translateErrorExpansionWith :: (Text -> FishJobList) -> Text -> FishExpr (TList TStr) -> FishExpr (TList TStr)
translateErrorExpansionWith condFn name errExpr =
  let cond = condFn name
      thenStmt = emitList (ExprVariable (VarAll (specialVarName name)))
      errStmt = Stmt (Command "printf"
        [ ExprVal (ExprLiteral "%s\n")
        , ExprVal (ExprJoinList errExpr)
        , RedirectVal (Redirect RedirectStdout RedirectOut (RedirectTargetFD 2))
        ])
      elseStmt = StmtList [errStmt, Stmt (Exit (Just (ExprNumLiteral 1)))]
  in commandSubst (Stmt (If cond (thenStmt NE.:| []) [elseStmt] []) :| [])

translateAltExpansionWith :: (Text -> FishJobList) -> Text -> FishExpr (TList TStr) -> FishExpr (TList TStr)
translateAltExpansionWith condFn name altExpr =
  let cond = condFn name
      thenStmt = emitList altExpr
  in commandSubst (Stmt (If cond (thenStmt NE.:| []) [] []) :| [])

--------------------------------------------------------------------------------
-- Extended parameter expansion (substring, patterns, case)
--------------------------------------------------------------------------------

translateModifierExpansion :: Token -> Maybe (FishExpr (TList TStr))
translateModifierExpansion word = do
  name <- paramNameFrom word
  let rawTxt = toText (concat (oversimplify word))
      modifier = toText (getBracedModifier (toString rawTxt))
      varName = specialVarName name
  if isLengthExpansion rawTxt modifier
    then Just (translateLengthExpansion name varName modifier)
    else case parseSubstringModifier modifier of
      Just (offsetTxt, lenTxt) ->
        Just (translateSubstringExpansion varName offsetTxt lenTxt)
      Nothing ->
        case parsePatternRemoval modifier of
          Just (isPrefix, greedy, pat) ->
            Just (translatePatternRemoval varName isPrefix greedy pat)
          Nothing ->
            case parsePatternReplacement modifier of
              Just (allMatches, pat, repl, anchor) ->
                Just (translatePatternReplacement varName allMatches pat repl anchor)
              Nothing ->
                case parseCaseModifier modifier of
                  Just CaseUpper -> Just (translateCaseModification varName True)
                  Just CaseLower -> Just (translateCaseModification varName False)
                  Nothing -> Nothing

data CaseMod = CaseUpper | CaseLower
data PatternAnchor = AnchorStart | AnchorEnd | AnchorNone

isLengthExpansion :: Text -> Text -> Bool
isLengthExpansion rawTxt modifier =
  T.isPrefixOf "#" rawTxt && (T.null modifier || isArrayLengthModifier modifier)

isArrayLengthModifier :: Text -> Bool
isArrayLengthModifier modifier =
  modifier `elem` ["[@]", "[*]", "@", "*"]

translateLengthExpansion :: Text -> Text -> Text -> FishExpr (TList TStr)
translateLengthExpansion rawName varName modifier =
  if isCountLengthExpansion rawName modifier
    then countVarExpr varName
    else ExprListLiteral [ExprStringOp StrLength (varAsString varName)]
  where
    isCountLengthExpansion name modTxt =
      name `elem` ["@", "*"] || isArrayLengthModifier modTxt
    countVarExpr name =
      commandSubst (Stmt (Command "count" [ExprVal (ExprVariable (VarAll name))]) NE.:| [])

parseSubstringModifier :: Text -> Maybe (Text, Maybe Text)
parseSubstringModifier modifier = do
  let trimmed = stripIndexPrefix modifier
  guard (T.isPrefixOf ":" trimmed)
  let rest = T.drop 1 trimmed
      (offsetTxt, remainder) = T.breakOn ":" rest
  if T.null offsetTxt
    then Nothing
    else if T.null remainder
      then Just (offsetTxt, Nothing)
      else Just (offsetTxt, Just (T.drop 1 remainder))

translateSubstringExpansion :: Text -> Text -> Maybe Text -> FishExpr (TList TStr)
translateSubstringExpansion name offsetTxt lenTxt =
  let offsetExpr = parseArithExprAdjusted offsetTxt
      lenExpr = lenTxt >>= parseArithExpr
      args =
        [ ExprVal (ExprLiteral "sub")
        , ExprVal (ExprLiteral "--start")
        , ExprVal offsetExpr
        ]
          <> maybe [] (\lenVal -> [ExprVal (ExprLiteral "--length"), ExprVal lenVal]) lenExpr
          <> [ExprVal (ExprLiteral "--"), ExprVal (ExprVariable (VarAll name))]
  in commandSubst (Stmt (Command "string" args) NE.:| [])

parsePatternRemoval :: Text -> Maybe (Bool, Bool, Text)
parsePatternRemoval modifier
  | Just pat <- T.stripPrefix "##" modifier = Just (True, True, pat)
  | Just pat <- T.stripPrefix "#" modifier = Just (True, False, pat)
  | Just pat <- T.stripPrefix "%%" modifier = Just (False, True, pat)
  | Just pat <- T.stripPrefix "%" modifier = Just (False, False, pat)
  | otherwise = Nothing

translatePatternRemoval :: Text -> Bool -> Bool -> Text -> FishExpr (TList TStr)
translatePatternRemoval name isPrefix greedy pat =
  let regex = globToRegex greedy pat
      anchored = if isPrefix then "^" <> regex else regex <> "$"
      args =
        [ ExprVal (ExprLiteral "replace")
        , ExprVal (ExprLiteral "-r")
        , ExprVal (ExprLiteral "--")
        , ExprVal (ExprLiteral anchored)
        , ExprVal (ExprLiteral "")
        , ExprVal (ExprVariable (VarAll name))
        ]
  in commandSubst (Stmt (Command "string" args) NE.:| [])

parsePatternReplacement :: Text -> Maybe (Bool, Text, Text, PatternAnchor)
parsePatternReplacement modifier = do
  rest <- T.stripPrefix "//" modifier <|> T.stripPrefix "/" modifier
  let allMatches = T.isPrefixOf "//" modifier
      (patRaw, remainder) = T.breakOn "/" rest
      repl = if T.null remainder then "" else T.drop 1 remainder
      (pat, anchor) =
        case T.uncons patRaw of
          Just ('#', tailPat) -> (tailPat, AnchorStart)
          Just ('%', tailPat) -> (tailPat, AnchorEnd)
          _ -> (patRaw, AnchorNone)
  pure (allMatches, pat, repl, anchor)

translatePatternReplacement :: Text -> Bool -> Text -> Text -> PatternAnchor -> FishExpr (TList TStr)
translatePatternReplacement name allMatches pat repl anchor =
  let regexBase = globToRegex True pat
      anchored = case anchor of
        AnchorStart -> "^" <> regexBase
        AnchorEnd -> regexBase <> "$"
        AnchorNone -> regexBase
      flags = ExprVal (ExprLiteral "replace")
        : ExprVal (ExprLiteral "-r")
        : ([ExprVal (ExprLiteral "-a") | allMatches])
      args =
        flags
          <> [ ExprVal (ExprLiteral "--")
             , ExprVal (ExprLiteral anchored)
             , ExprVal (ExprLiteral repl)
             , ExprVal (ExprVariable (VarAll name))
             ]
  in commandSubst (Stmt (Command "string" args) NE.:| [])

parseCaseModifier :: Text -> Maybe CaseMod
parseCaseModifier = \case
  "^^" -> Just CaseUpper
  "^" -> Just CaseUpper
  ",," -> Just CaseLower
  "," -> Just CaseLower
  _ -> Nothing

translateCaseModification :: Text -> Bool -> FishExpr (TList TStr)
translateCaseModification name upper =
  let op = if upper then StrUpper else StrLower
  in ExprListLiteral [ExprStringOp op (varAsString name)]

stripIndexPrefix :: Text -> Text
stripIndexPrefix modifier =
  case T.uncons modifier of
    Just ('[', rest) ->
      case T.breakOn "]" rest of
        (_, remainder) | T.null remainder -> modifier
        (_, remainder) -> T.drop 1 remainder
    _ -> modifier

varAsString :: Text -> FishExpr TStr
varAsString name = ExprJoinList (ExprVariable (VarAll name))

globToRegex :: Bool -> Text -> Text
globToRegex greedy = go False
  where
    star = if greedy then ".*" else ".*?"
    go _ "" = ""
    go inClass txt =
      case T.uncons txt of
        Nothing -> ""
        Just (c, rest)
          | inClass ->
              let next = if c == ']' then go False rest else go True rest
              in T.singleton c <> next
          | c == '[' -> T.singleton c <> go True rest
          | c == '*' -> star <> go False rest
          | c == '?' -> "." <> go False rest
          | isRegexMeta c -> "\\" <> T.singleton c <> go False rest
          | otherwise -> T.singleton c <> go False rest

    isRegexMeta ch = ch `elem` ("\\.+*?[^]$(){}=!<>|:-" :: String)

parseArithExprAdjusted :: Text -> FishExpr TInt
parseArithExprAdjusted txt =
  fromMaybe (ExprNumLiteral 1) (parseArithExprWith True txt)

parseArithExpr :: Text -> Maybe (FishExpr TInt)
parseArithExpr = parseArithExprWith False

parseArithExprWith :: Bool -> Text -> Maybe (FishExpr TInt)
parseArithExprWith adjust txt =
  let trimmed = T.strip txt
  in if T.null trimmed
      then Nothing
      else case parseInt trimmed of
        Just n ->
          let adjusted = if adjust then adjustIndex n else n
          in Just (ExprNumLiteral adjusted)
        Nothing -> do
          args <- arithArgsFromText trimmed
          let base = ExprMath args
          pure (if adjust then adjustIndexExpr base else base)

adjustIndexExpr :: FishExpr TInt -> FishExpr TInt
adjustIndexExpr expr =
  case expr of
    ExprNumLiteral n -> ExprNumLiteral (adjustIndex n)
    ExprMath args -> ExprMath (appendMathArgs args)
    _ -> expr

appendMathArgs :: NonEmpty (FishExpr TStr) -> NonEmpty (FishExpr TStr)
appendMathArgs args = args <> (ExprLiteral "+" NE.:| [ExprLiteral "1"])

parseIndexSpec :: Text -> Maybe (FishIndex TStr (TList TStr))
parseIndexSpec rawTxt =
  let txt = T.strip rawTxt
  in if T.null txt || txt == "@" || txt == "*"
       then Nothing
       else if T.isInfixOf ".." txt
         then parseIndexRange txt
         else if T.isInfixOf "," txt
           then parseIndexList txt
           else parseIndexSingle txt
  where
    parseIndexSingle t = do
      n <- parseIndexExpr t
      pure (IndexList (n NE.:| []))

    parseIndexList t = do
      parts <- NE.nonEmpty (map T.strip (T.splitOn "," t))
      exprs <- traverse parseIndexExpr parts
      pure (IndexList exprs)

    parseIndexRange t =
      case T.splitOn ".." t of
        [startTxt, endTxt] ->
          let startIx = parseIndexPart startTxt
              endIx = parseIndexPart endTxt
          in if isNothing startIx && isNothing endIx
              then Nothing
              else Just (IndexRange startIx endIx)
        _ -> Nothing

    parseIndexPart t =
      if T.null (T.strip t)
        then Nothing
        else parseIndexExpr t

parseIndexExpr :: Text -> Maybe (FishExpr TInt)
parseIndexExpr = parseArithExprWith True

--------------------------------------------------------------------------------
-- Array index helpers
--------------------------------------------------------------------------------

indexedVarText :: Text -> [Token] -> Maybe Text
indexedVarText name indices =
  case mapMaybe indexTokenText indices of
    [] -> Nothing
    parts -> Just (name <> foldMap (\t -> "[" <> t <> "]") parts)

indexTokenText :: Token -> Maybe Text
indexTokenText tok =
  case tok of
    T_UnparsedIndex _ _ raw ->
      Just (indexTextFromRaw (toText raw))
    TA_Sequence {} -> Just (indexTextFromArithToken tok)
    TA_Expansion {} -> Just (indexTextFromArithToken tok)
    TA_Parenthesis {} -> Just (indexTextFromArithToken tok)
    TA_Unary {} -> Just (indexTextFromArithToken tok)
    TA_Binary {} -> Just (indexTextFromArithToken tok)
    TA_Trinary {} -> Just (indexTextFromArithToken tok)
    TA_Assignment {} -> Just (indexTextFromArithToken tok)
    TA_Variable {} -> Just (indexTextFromArithToken tok)
    T_Literal _ s ->
      Just (adjustIndexText (toText s))
    _ ->
      let txt = tokenToLiteralText tok
      in if T.null txt then Nothing else Just (adjustIndexText txt)

indexTextFromArithToken :: Token -> Text
indexTextFromArithToken tok =
  let args = arithArgsFromToken tok
  in case NE.toList args of
    [ExprLiteral txt]
      | Just n <- parseInt txt -> show (adjustIndex n)
    _ ->
      mathExprText (appendMathArgs args)

indexTextFromRaw :: Text -> Text
indexTextFromRaw raw =
  case parseInt raw of
    Just n -> show (adjustIndex n)
    Nothing ->
      case arithArgsFromText raw of
        Just args -> mathExprText (appendMathArgs args)
        Nothing -> raw

--------------------------------------------------------------------------------
-- Glob helpers
--------------------------------------------------------------------------------

extglobShimListExpr :: Text -> FishExpr (TList TStr)
extglobShimListExpr pat =
  ExprCommandSubst (Stmt (Command "bash" [ExprVal (ExprLiteral "-O"), ExprVal (ExprLiteral "extglob"), ExprVal (ExprLiteral "-c"), ExprVal (ExprLiteral (extglobShimScript pat))]) NE.:| [])

extglobShimScript :: Text -> Text
extglobShimScript pat =
  "shopt -s extglob; shopt -u nullglob dotglob failglob; printf '%s\\n' " <> pat

parseGlobPattern :: Text -> GlobPattern
parseGlobPattern txt = GlobPattern (finishLiteral (go txt) [])
  where
    go t =
      case T.uncons t of
        Nothing -> []
        Just ('*', rest) ->
          case T.uncons rest of
            Just ('*', r) -> GlobStarStar : go r
            _ -> GlobStar : go rest
        Just ('?', rest) -> GlobQuestion : go rest
        Just ('[', rest) ->
          let (cls, r) = T.breakOn "]" rest
          in case T.uncons r of
              Just (_, r') -> GlobCharClass cls : go r'
              Nothing -> GlobLiteral ("[" <> cls) : go r
        Just ('{', rest) ->
          let (inner, r) = T.breakOn "}" rest
              alts = filter (not . T.null) (T.splitOn "," inner)
          in case (alts, T.uncons r) of
              (a:as, Just (_, r')) -> GlobBraces (a NE.:| as) : go r'
              _ -> GlobLiteral ("{" <> inner) : go r
        Just (c, rest) ->
          let (lit, r) = spanLiteral rest (T.singleton c)
          in GlobLiteral lit : go r

    spanLiteral t acc =
      case T.uncons t of
        Just (c, rest)
          | c `elem` ("*?{[" :: String) -> (acc, t)
          | otherwise -> spanLiteral rest (acc <> T.singleton c)
        Nothing -> (acc, T.empty)

    finishLiteral parts acc =
      case parts of
        [] -> reverse acc
        (GlobLiteral t1 : GlobLiteral t2 : xs) -> finishLiteral (GlobLiteral (t1 <> t2) : xs) acc
        (x:xs) -> finishLiteral xs (x:acc)

renderExtglobForFish :: String -> [Token] -> Maybe Text
renderExtglobForFish op parts =
  case op of
    "@" -> Just ("{" <> T.intercalate "," (extglobAlternatives parts) <> "}")
    _ -> Nothing

renderExtglobRaw :: String -> [Token] -> Text
renderExtglobRaw op parts =
  toText op <> "(" <> T.intercalate "|" (extglobAlternatives parts) <> ")"

extglobAlternatives :: [Token] -> [Text]
extglobAlternatives parts =
  let splitAlts = splitExtglobParts parts
      alts =
        if length splitAlts == 1 && length parts > 1
          then map tokenToLiteralText parts
          else map (T.concat . map tokenToLiteralText) splitAlts
  in filter (not . T.null) alts

splitExtglobParts :: [Token] -> [[Token]]
splitExtglobParts toks = go toks [] []
  where
    go [] current acc =
      reverse (reverse current : acc)
    go (t:ts) current acc
      | isSeparator t = go ts [] (reverse current : acc)
      | otherwise = go ts (t:current) acc

    isSeparator tok =
      case tok of
        T_Pipe {} -> True
        T_ParamSubSpecialChar _ "|" -> True
        _ -> tokenToLiteralText tok == "|"

mathExprText :: NonEmpty (FishExpr TStr) -> Text
mathExprText args =
  "(math " <> renderMathArgs args <> ")"

renderMathArgs :: NonEmpty (FishExpr TStr) -> Text
renderMathArgs args =
  T.intercalate " " (map renderMathArg (NE.toList args))

renderMathArg :: FishExpr TStr -> Text
renderMathArg = \case
  ExprLiteral txt -> txt
  ExprVariable (VarScalar name) -> "$" <> name
  _ -> "<expr>"


adjustIndexText :: Text -> Text
adjustIndexText txt =
  case parseInt txt of
    Just n -> show (adjustIndex n)
    Nothing -> txt

adjustIndex :: Int -> Int
adjustIndex n
  | n >= 0 = n + 1
  | otherwise = n

parseInt :: Text -> Maybe Int
parseInt t = readMaybe (toString (T.strip t))

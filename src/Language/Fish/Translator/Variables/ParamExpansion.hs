{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Variables.ParamExpansion
  ( ParamExpansion,
    parseParamExpansion,
    parseParamExpansionStr,
    renderParamExpansion,
    renderParamExpansionWithPrelude,
    noSplitParamExpansion,
    translateSimpleVar,
    splitParamOperator,
    paramIndexFrom,
    translateDefaultExpansionWith,
    translateAssignDefaultExpansionWith,
    translateErrorExpansionWith,
    translateAltExpansionWith,
    varNonEmptyCond,
    varSetCond,
    emitList,
    commandSubst,
  )
where

import Data.Char (isDigit)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Language.Fish.AST
import Language.Fish.Translator.Cond
  ( testBinaryCommand,
    testUnaryCommand,
  )
import Language.Fish.Translator.Hoist (Hoisted (..))
import Language.Fish.Translator.Hoist.Monad (HoistedM, hoistM)
import Language.Fish.Translator.Token (tokenRawText, tokenToLiteralText)
import Language.Fish.Translator.Variables.Common (paramNameFrom, scopeFlagsForVarM, specialVarName)
import Language.Fish.Translator.Variables.Index
  ( parseArithExpr,
    parseArithExprAdjusted,
    parseIndexSpec,
  )
import ShellCheck.AST
import ShellCheck.ASTLib (getBracedModifier)

data ParamExpansion (t :: FishType) where
  ParamExpansionList :: ParamCore -> ParamExpansion (TList TStr)
  ParamExpansionStr :: ParamCore -> ParamExpansion TStr

data ParamCore
  = ParamCoreSimple ParamSimple
  | ParamCoreOperator Text ParamOperator
  | ParamCoreModifier Text ParamModifier

data ParamSimple = ParamSimple
  { simpleName :: Maybe Text,
    simpleIndex :: Maybe (FishIndex TStr (TList TStr))
  }

data ParamOpKind = OpDefault | OpAssign | OpError | OpAlt

data ParamOpCond = CondSet | CondNonEmpty

data ParamOperator = ParamOperator ParamOpKind ParamOpCond [Token]

data ParamModifier
  = ModAltSelf
  | ModLength Text
  | ModSubstring Text (Maybe Text)
  | ModPatternRemoval Bool Bool Text
  | ModPatternReplacement Bool Text Text PatternAnchor
  | ModCase CaseMod

parseParamExpansion :: Token -> ParamExpansion (TList TStr)
parseParamExpansion = ParamExpansionList . parseParamCore

parseParamExpansionStr :: Token -> ParamExpansion TStr
parseParamExpansionStr = ParamExpansionStr . parseParamCore

renderParamExpansion :: ([Token] -> FishExpr (TList TStr)) -> ParamExpansion t -> FishExpr t
renderParamExpansion tokensToListExpr = \case
  ParamExpansionList core -> renderParamCore tokensToListExpr core
  ParamExpansionStr core -> ExprJoinList (renderParamCore tokensToListExpr core)

renderParamExpansionWithPrelude ::
  ([Token] -> FishExpr (TList TStr)) ->
  ([Token] -> HoistedM (FishExpr (TList TStr))) ->
  ParamExpansion t ->
  HoistedM (FishExpr t)
renderParamExpansionWithPrelude tokensToListExpr tokensToListExprM = \case
  ParamExpansionList core -> renderParamCoreWithPrelude tokensToListExpr tokensToListExprM core
  ParamExpansionStr core -> do
    Hoisted pre listExpr <- renderParamCoreWithPrelude tokensToListExpr tokensToListExprM core
    hoistM pre (ExprJoinList listExpr)

parseParamCore :: Token -> ParamCore
parseParamCore word =
  case parseParamOperator word of
    Just (name, op) -> ParamCoreOperator name op
    Nothing ->
      case parseParamModifier word of
        Just (name, modifier) -> ParamCoreModifier name modifier
        Nothing -> ParamCoreSimple (parseSimpleVar word)

parseSimpleVar :: Token -> ParamSimple
parseSimpleVar word =
  ParamSimple
    { simpleName = paramNameFrom word,
      simpleIndex = paramIndexFrom word
    }

renderParamCore :: ([Token] -> FishExpr (TList TStr)) -> ParamCore -> FishExpr (TList TStr)
renderParamCore tokensToListExpr = \case
  ParamCoreSimple simple -> renderSimpleVar simple
  ParamCoreOperator name op -> renderParamOperator tokensToListExpr name op
  ParamCoreModifier name modifier -> renderModifierExpansion name modifier

renderParamCoreWithPrelude ::
  ([Token] -> FishExpr (TList TStr)) ->
  ([Token] -> HoistedM (FishExpr (TList TStr))) ->
  ParamCore ->
  HoistedM (FishExpr (TList TStr))
renderParamCoreWithPrelude tokensToListExpr tokensToListExprM = \case
  ParamCoreOperator name (ParamOperator kind cond rest) ->
    case kind of
      OpAssign -> renderAssignDefault tokensToListExprM name cond rest
      OpError -> renderErrorDefault tokensToListExprM name cond rest
      _ ->
        hoistM
          []
          (renderParamOperator tokensToListExpr name (ParamOperator kind cond rest))
  ParamCoreModifier name modifier -> hoistM [] (renderModifierExpansion name modifier)
  ParamCoreSimple simple -> hoistM [] (renderSimpleVar simple)
  where
    condFrom = \case
      CondNonEmpty -> varNonEmptyCond
      CondSet -> varSetCond
    renderAssignDefault tokensToListExprM' name cond rest = do
      let condFn = condFrom cond
          varName = specialVarName name
      flags <- scopeFlagsForVarM varName
      Hoisted pre defaultExpr <- tokensToListExprM' rest
      let setStmt = Stmt (Set flags varName defaultExpr)
          thenStmt = Stmt (Command "true" [])
          elseStmts = pre <> [setStmt]
          ifStmt = Stmt (If (condFn name) (thenStmt NE.:| []) elseStmts [])
      hoistM [ifStmt] (ExprVariable (VarAll varName))
    renderErrorDefault tokensToListExprM' name cond rest = do
      let condFn = condFrom cond
          varName = specialVarName name
      Hoisted pre errExpr <- tokensToListExprM' rest
      let errStmt =
            Stmt
              ( Command
                  "printf"
                  [ ExprVal (ExprLiteral "%s\\n"),
                    ExprVal (ExprJoinList errExpr),
                    RedirectVal (Redirect RedirectStdout RedirectOut (RedirectTargetFD 2))
                  ]
              )
          elseStmts = pre <> [errStmt, Stmt (Exit (Just (ExprNumLiteral 1)))]
          thenStmt = Stmt (Command "true" [])
          ifStmt = Stmt (If (condFn name) (thenStmt NE.:| []) elseStmts [])
      hoistM [ifStmt] (ExprVariable (VarAll varName))

parseParamOperator :: Token -> Maybe (Text, ParamOperator)
parseParamOperator word = do
  name <- paramNameFrom word
  (opTxt, rest) <- splitParamOperator word
  op <- parseParamOperatorText opTxt rest
  pure (name, op)
  where
    parseParamOperatorText opTxt rest =
      case opTxt of
        ":-" -> Just (ParamOperator OpDefault CondNonEmpty rest)
        "-" -> Just (ParamOperator OpDefault CondSet rest)
        ":=" -> Just (ParamOperator OpAssign CondNonEmpty rest)
        "=" -> Just (ParamOperator OpAssign CondSet rest)
        ":?" -> Just (ParamOperator OpError CondNonEmpty rest)
        "?" -> Just (ParamOperator OpError CondSet rest)
        ":+" -> Just (ParamOperator OpAlt CondNonEmpty rest)
        "+" -> Just (ParamOperator OpAlt CondSet rest)
        _ -> Nothing

renderParamOperator :: ([Token] -> FishExpr (TList TStr)) -> Text -> ParamOperator -> FishExpr (TList TStr)
renderParamOperator tokensToListExpr name (ParamOperator kind cond rest) =
  let condFn = case cond of
        CondNonEmpty -> varNonEmptyCond
        CondSet -> varSetCond
   in case kind of
        OpDefault ->
          translateDefaultExpansionWith condFn name (tokensToListExpr rest)
        OpAssign ->
          translateAssignDefaultExpansionWith condFn name (tokensToListExpr rest)
        OpError ->
          translateErrorExpansionWith condFn name (tokensToListExpr rest)
        OpAlt ->
          translateAltExpansionWith condFn name (tokensToListExpr rest)

renderSimpleVar :: ParamSimple -> FishExpr (TList TStr)
renderSimpleVar (ParamSimple name idx) =
  case (name, idx) of
    (Just name', _)
      | name' == "#" ->
          commandSubst (Stmt (Command "count" [ExprVal (ExprVariable (VarAll "argv"))]) NE.:| [])
    (Just name', Just idx') ->
      ExprVariable (VarIndex (specialVarName name') idx')
    (Just name', Nothing)
      | T.all isDigit name',
        Just n <- readMaybe (toString name') ->
          ExprVariable (VarIndex "argv" (IndexList (ExprNumLiteral n NE.:| [])))
      | otherwise ->
          ExprVariable (VarAll (specialVarName name'))
    _ -> ExprListLiteral []

translateSimpleVar :: Token -> FishExpr (TList TStr)
translateSimpleVar = renderSimpleVar . parseSimpleVar

parseParamModifier :: Token -> Maybe (Text, ParamModifier)
parseParamModifier word = do
  name <- paramNameFrom word
  let rawTxt = tokenRawText word
      modifier = toText (getBracedModifier (toString rawTxt))
  modifierParsed <- parseModifierExpansion name rawTxt modifier
  pure (name, modifierParsed)

parseModifierExpansion :: Text -> Text -> Text -> Maybe ParamModifier
parseModifierExpansion name rawTxt modifier
  | parseAltModifier name modifier = Just ModAltSelf
  | isLengthExpansion rawTxt modifier = Just (ModLength modifier)
  | Just (offsetTxt, lenTxt) <- parseSubstringModifier modifier =
      Just (ModSubstring offsetTxt lenTxt)
  | Just (isPrefix, greedy, pat) <- parsePatternRemoval modifier =
      Just (ModPatternRemoval isPrefix greedy pat)
  | Just (allMatches, pat, repl, anchor) <- parsePatternReplacement modifier =
      Just (ModPatternReplacement allMatches pat repl anchor)
  | Just caseMod <- parseCaseModifier modifier = Just (ModCase caseMod)
  | otherwise = Nothing

renderModifierExpansion :: Text -> ParamModifier -> FishExpr (TList TStr)
renderModifierExpansion name = \case
  ModAltSelf ->
    translateAltExpansionWith
      varNonEmptyCond
      name
      (ExprVariable (VarAll (specialVarName name)))
  ModLength modifier ->
    translateLengthExpansion name (specialVarName name) modifier
  ModSubstring offsetTxt lenTxt ->
    translateSubstringExpansion (specialVarName name) offsetTxt lenTxt
  ModPatternRemoval isPrefix greedy pat ->
    translatePatternRemoval (specialVarName name) isPrefix greedy pat
  ModPatternReplacement allMatches pat repl anchor ->
    translatePatternReplacement (specialVarName name) allMatches pat repl anchor
  ModCase CaseUpper ->
    translateCaseModification (specialVarName name) True
  ModCase CaseLower ->
    translateCaseModification (specialVarName name) False

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
      errStmt =
        Stmt
          ( Command
              "printf"
              [ ExprVal (ExprLiteral "%s\\n"),
                ExprVal (ExprJoinList errExpr),
                RedirectVal (Redirect RedirectStdout RedirectOut (RedirectTargetFD 2))
              ]
          )
      elseStmt = StmtList [errStmt, Stmt (Exit (Just (ExprNumLiteral 1)))]
   in commandSubst (Stmt (If cond (thenStmt NE.:| []) [elseStmt] []) :| [])

translateAltExpansionWith :: (Text -> FishJobList) -> Text -> FishExpr (TList TStr) -> FishExpr (TList TStr)
translateAltExpansionWith condFn name altExpr =
  let cond = condFn name
      thenStmt = emitList altExpr
   in commandSubst (Stmt (If cond (thenStmt NE.:| []) [] []) :| [])

noSplitParamExpansion :: Token -> Bool
noSplitParamExpansion tok =
  case paramNameFrom tok of
    Just "@" -> True
    _ ->
      let rawTxt = tokenRawText tok
          modifier = toText (getBracedModifier (toString rawTxt))
          paramName = paramNameFrom tok
          hasArrayIndex txt = "[@]" `T.isInfixOf` txt || "[*]" `T.isInfixOf` txt
          altSelf =
            case paramName of
              Just base -> parseAltModifier base modifier
              Nothing -> False
          literal =
            case tok of
              T_DollarBraced _ _ inner -> tokenToLiteralText inner
              T_ParamSubSpecialChar _ specialName -> toText specialName
              _ -> ""
       in hasArrayIndex modifier
            || hasArrayIndex rawTxt
            || hasArrayIndex literal
            || literal `elem` ["@", "*"]
            || altSelf

parseAltModifier :: Text -> Text -> Bool
parseAltModifier name modifier =
  isJust $ do
    let trimmed = stripIndexPrefix modifier
    rest <- T.stripPrefix ":+" trimmed
    let inner0 = fromMaybe rest (T.stripPrefix "\"" rest >>= T.stripSuffix "\"")
    inner1 <-
      case T.stripPrefix "${" inner0 >>= T.stripSuffix "}" of
        Just braceInner -> Just braceInner
        Nothing -> T.stripPrefix "$" inner0
    let base =
          fromMaybe inner1
            ( T.stripSuffix "[@]" inner1
                <|> T.stripSuffix "[*]" inner1
            )
    guard (base == name)

splitParamOperator :: Token -> Maybe (Text, [Token])
splitParamOperator word@(T_NormalWord _ parts) =
  case break isParamOp parts of
    (_, []) -> splitLiteralOperator word parts
    (_, T_ParamSubSpecialChar _ op : rest) ->
      let opTxt = toText op
       in case opTxt of
            ":" -> parseColonOp rest
            _ ->
              if opTxt `elem` paramOps
                then Just (opTxt, rest)
                else splitLiteralOperator word parts
    _ -> splitLiteralOperator word parts
  where
    isParamOp (T_ParamSubSpecialChar _ _) = True
    isParamOp _ = False
    paramOps =
      [ ":-",
        "-",
        ":=",
        "=",
        ":?",
        "?",
        ":+",
        "+"
      ]
    parseColonOp rest =
      case rest of
        (T_ParamSubSpecialChar _ next : rest') ->
          let combined = ":" <> toText next
           in if combined `elem` paramOps
                then Just (combined, rest')
                else splitLiteralOperator word parts
        (T_Literal _ s : rest') ->
          case T.uncons (toText s) of
            Just (c, suffix)
              | T.singleton c `elem` ["-", "+", "=", "?"] ->
                  let combined = ":" <> T.singleton c
                      suffixTokens =
                        [T_Literal (Id 0) (toString suffix) | not (T.null suffix)]
                   in if combined `elem` paramOps
                        then Just (combined, suffixTokens <> rest')
                        else splitLiteralOperator word parts
            _ -> splitLiteralOperator word parts
        _ -> splitLiteralOperator word parts
    splitLiteralOperator word' parts' = do
      name <- paramNameFrom word'
      (literal, rest) <- case parts' of
        (T_Literal _ s : xs) -> Just (toText s, xs)
        _ -> Nothing
      (prefix, op) <- findOp name literal
      let suffix = T.drop (T.length (prefix <> op)) literal
          suffixTokens =
            ([T_Literal (Id 0) (toString suffix) | not (T.null suffix)])
      pure (op, suffixTokens <> rest)
    findOp name literal =
      let nameWithIndex =
            case T.stripPrefix name literal of
              Just rest ->
                case T.uncons rest of
                  Just ('[', more) ->
                    let (idx, remainder) = T.breakOn "]" more
                     in if T.null remainder
                          then name
                          else name <> "[" <> idx <> "]"
                  _ -> name
              Nothing -> name
          matches prefix op = T.isPrefixOf (prefix <> op) literal
          findMatch prefix = fmap (prefix,) (find (matches prefix) paramOps)
       in findMatch nameWithIndex <|> findMatch name
splitParamOperator _ = Nothing

paramIndexFrom :: Token -> Maybe (FishIndex TStr (TList TStr))
paramIndexFrom word =
  let rawFull = tokenIndexText word
      rawMod = toText (getBracedModifier (toString rawFull))
      idxRaw =
        if T.null rawMod
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

tokenIndexText :: Token -> Text
tokenIndexText tok =
  case tok of
    T_NormalWord _ parts ->
      let combined = T.concat (map indexPartText parts)
       in if T.null combined then tokenRawText tok else combined
    _ -> tokenRawText tok

indexPartText :: Token -> Text
indexPartText = \case
  T_DollarBraced _ _ inner ->
    fromMaybe "" (paramNameFrom inner)
  other -> tokenToLiteralText other

data CaseMod = CaseUpper | CaseLower

data PatternAnchor = AnchorStart | AnchorEnd | AnchorNone

isLengthExpansion :: Text -> Text -> Bool
isLengthExpansion rawTxt modifier =
  T.isPrefixOf "#" rawTxt && (T.null modifier || isArrayLengthModifier modifier)

isArrayLengthModifier :: Text -> Bool
isArrayLengthModifier modifier =
  modifier `elem` ["[@]", "[*]", "@", "*"]

translateLengthExpansion :: Text -> Text -> Text -> FishExpr (TList TStr)
translateLengthExpansion rawName varName modifier
  | rawName == "#" = countVarExpr "argv"
  | isCountLengthExpansion rawName modifier = countVarExpr varName
  | otherwise = ExprListLiteral [ExprStringOp StrLength (varAsString varName)]
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
    else
      if T.null remainder
        then Just (offsetTxt, Nothing)
        else Just (offsetTxt, Just (T.drop 1 remainder))

translateSubstringExpansion :: Text -> Text -> Maybe Text -> FishExpr (TList TStr)
translateSubstringExpansion name offsetTxt lenTxt =
  let offsetExpr = parseArithExprAdjusted offsetTxt
      lenExpr = lenTxt >>= parseArithExpr
      args =
        [ ExprVal (ExprLiteral "sub"),
          ExprVal (ExprLiteral "--start"),
          ExprVal offsetExpr
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
        [ ExprVal (ExprLiteral "replace"),
          ExprVal (ExprLiteral "-r"),
          ExprVal (ExprLiteral "--"),
          ExprVal (ExprLiteral anchored),
          ExprVal (ExprLiteral ""),
          ExprVal (ExprVariable (VarAll name))
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
      flags =
        ExprVal (ExprLiteral "replace")
          : ExprVal (ExprLiteral "-r")
          : ([ExprVal (ExprLiteral "-a") | allMatches])
      args =
        flags
          <> [ ExprVal (ExprLiteral "--"),
               ExprVal (ExprLiteral anchored),
               ExprVal (ExprLiteral repl),
               ExprVal (ExprVariable (VarAll name))
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

varNonEmptyCond :: Text -> FishJobList
varNonEmptyCond name =
  let varExpr = ExprJoinList (ExprVariable (VarAll (specialVarName name)))
      setq =
        FishJobPipeline
          False
          []
          ( Stmt
              ( Command
                  "set"
                  [ ExprVal (ExprLiteral "-q"),
                    ExprVal (ExprLiteral (specialVarName name))
                  ]
              )
          )
          []
          False
      test =
        FishJobPipeline
          False
          []
          (Stmt (testUnaryCommand "-n" varExpr))
          []
          False
   in FishJobList (FishJobConjunction Nothing setq [JCAnd test] NE.:| [])

varSetCond :: Text -> FishJobList
varSetCond name =
  let setq =
        FishJobPipeline
          False
          []
          ( Stmt
              ( Command
                  "set"
                  [ ExprVal (ExprLiteral "-q"),
                    ExprVal (ExprLiteral (specialVarName name))
                  ]
              )
          )
          []
          False
   in FishJobList (FishJobConjunction Nothing setq [] NE.:| [])

jobListFromStatus :: FishCommand TStatus -> FishJobList
jobListFromStatus cmd =
  FishJobList
    ( FishJobConjunction
        Nothing
        (FishJobPipeline False [] (Stmt cmd) [] False)
        []
        NE.:| []
    )

emitList :: FishExpr (TList TStr) -> FishStatement
emitList expr =
  let countExpr =
        ExprCommandSubst
          ( Stmt
              (Command "count" [ExprVal expr])
              NE.:| []
          )
      cond =
        jobListFromStatus
          (testBinaryCommand "-gt" countExpr (ExprLiteral "0"))
      thenStmt =
        Stmt
          ( Command
              "printf"
              [ ExprVal (ExprLiteral "%s\\n"),
                ExprVal expr
              ]
          )
   in Stmt (If cond (thenStmt NE.:| []) [] [])

commandSubst :: NonEmpty FishStatement -> FishExpr (TList TStr)
commandSubst = ExprCommandSubst

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

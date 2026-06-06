{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Variables.ParamExpansion.Render
  ( renderParamExpansion,
    renderParamExpansionWithPrelude,
    translateSimpleVar,
    translateSimpleVarM,
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
import Language.Fish.Translator.Background (noteBackgroundTracking)
import Language.Fish.Translator.Cond
  ( testBinaryCommand,
    testUnaryCommand,
  )
import Language.Fish.Translator.DSL
import Language.Fish.Translator.Hoist (Hoisted (..))
import Language.Fish.Translator.Hoist.Monad (HoistedM, hoistM)
import Language.Fish.Translator.Monad (TranslateM)
import Language.Fish.Translator.Variables.Common (scopeFlagsForVarM, specialVarName)
import Language.Fish.Translator.Variables.Index
  ( parseArithExpr,
    parseArithExprAdjusted,
  )
import Language.Fish.Translator.Variables.ParamExpansion.Parse
  ( parseSimpleVar,
  )
import Language.Fish.Translator.Variables.ParamExpansion.Types
import ShellCheck.AST (Token)

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
    MkHoisted pre listExpr <- renderParamCoreWithPrelude tokensToListExpr tokensToListExprM core
    hoistM pre (ExprJoinList listExpr)

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
  ParamCoreOperator name (MkParamOperator kind cond rest) ->
    case kind of
      OpAssign -> renderAssignDefault tokensToListExprM name cond rest
      OpError -> renderErrorDefault tokensToListExprM name cond rest
      _ ->
        hoistM
          []
          (renderParamOperator tokensToListExpr name (MkParamOperator kind cond rest))
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
      MkHoisted pre defaultExpr <- tokensToListExprM' rest
      let setStmt = Stmt (Set flags varName defaultExpr)
          thenStmt = Stmt (Command "true" [])
          elseStmts = pre <> [setStmt]
          ifStmt = Stmt (If (condFn name) (thenStmt NE.:| []) elseStmts [])
      hoistM [ifStmt] (ExprVariable (VarAll varName))
    renderErrorDefault tokensToListExprM' name cond rest = do
      let condFn = condFrom cond
          varName = specialVarName name
      MkHoisted pre errExpr <- tokensToListExprM' rest
      let errStmt =
            Stmt
              ( Command
                  "printf"
                  [ ExprVal (ExprLiteral "%s\\n"),
                    ExprVal (ExprJoinList errExpr),
                    RedirectVal (MkRedirect RedirectStdout RedirectOut (RedirectTargetFD 2))
                  ]
              )
          elseStmts = pre <> [errStmt, Stmt (Exit (Just (ExprNumLiteral 1)))]
          thenStmt = Stmt (Command "true" [])
          ifStmt = Stmt (If (condFn name) (thenStmt NE.:| []) elseStmts [])
      hoistM [ifStmt] (ExprVariable (VarAll varName))

renderParamOperator :: ([Token] -> FishExpr (TList TStr)) -> Text -> ParamOperator -> FishExpr (TList TStr)
renderParamOperator tokensToListExpr name (MkParamOperator kind cond rest) =
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
renderSimpleVar (MkParamSimple name idx) =
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

translateSimpleVarM :: Token -> TranslateM (FishExpr (TList TStr))
translateSimpleVarM tok = do
  case simpleName (parseSimpleVar tok) of
    Just "!" -> noteBackgroundTracking
    _ -> pure ()
  pure (translateSimpleVar tok)

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
                RedirectVal (MkRedirect RedirectStdout RedirectOut (RedirectTargetFD 2))
              ]
          )
      elseStmt = StmtList [errStmt, Stmt (Exit (Just (ExprNumLiteral 1)))]
   in commandSubst (Stmt (If cond (thenStmt NE.:| []) [elseStmt] []) :| [])

translateAltExpansionWith :: (Text -> FishJobList) -> Text -> FishExpr (TList TStr) -> FishExpr (TList TStr)
translateAltExpansionWith condFn name altExpr =
  let cond = condFn name
      thenStmt = emitList altExpr
   in commandSubst (Stmt (If cond (thenStmt NE.:| []) [] []) :| [])

translateLengthExpansion :: Text -> Text -> Text -> FishExpr (TList TStr)
translateLengthExpansion rawName varName modifier
  | rawName == "#" = countVarExpr "argv"
  | isCountLengthExpansion rawName modifier = countVarExpr varName
  | otherwise = ExprListLiteral [ExprStringOp StrLength (varAsString varName)]
  where
    isCountLengthExpansion name modTxt =
      name `elem` ["@", "*"] || modTxt `elem` ["[@]", "[*]", "@", "*"]
    countVarExpr name =
      commandSubst (Stmt (Command "count" [ExprVal (ExprVariable (VarAll name))]) NE.:| [])

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

translateCaseModification :: Text -> Bool -> FishExpr (TList TStr)
translateCaseModification name upper =
  let op = if upper then StrUpper else StrLower
   in ExprListLiteral [ExprStringOp op (varAsString name)]

varAsString :: Text -> FishExpr TStr
varAsString name = ExprJoinList (ExprVariable (VarAll name))

varNonEmptyCond :: Text -> FishJobList
varNonEmptyCond name =
  let varExpr = ExprJoinList (ExprVariable (VarAll (specialVarName name)))
      setq =
        MkFishJobPipeline
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
        MkFishJobPipeline
          False
          []
          (Stmt (testUnaryCommand "-n" varExpr))
          []
          False
   in MkFishJobList (MkFishJobConjunction Nothing setq [JCAnd test] NE.:| [])

varSetCond :: Text -> FishJobList
varSetCond name =
  let setq =
        MkFishJobPipeline
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
   in MkFishJobList (MkFishJobConjunction Nothing setq [] NE.:| [])

jobListFromStatus :: FishCommand TStatus -> FishJobList
jobListFromStatus cmd =
  MkFishJobList
    ( MkFishJobConjunction
        Nothing
        (MkFishJobPipeline False [] (Stmt cmd) [] False)
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

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Variables.Expressions.Lists
  ( translateTokenToListExprWith,
    translateTokenToListExprMWith,
    translateTokensToListExprWith,
    translateTokensToListExprMWith,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Language.Fish.AST
import Language.Fish.Translator.Hoist (Hoisted (..))
import Language.Fish.Translator.Hoist.Monad (HoistedM, hoistM)
import Language.Fish.Translator.Token (tokenToLiteralText)
import Language.Fish.Translator.Variables.Arithmetic
  ( arithArgsPlanM,
    mathCommandFromArgs,
    mathCommandFromToken,
  )
import Language.Fish.Translator.Variables.Expressions.Arrays
  ( translateArrayElementsMWith,
    translateArrayElementsWith,
  )
import Language.Fish.Translator.Variables.Expressions.Split
  ( splitOnIfsExpr,
    splitOnIfsListExpr,
  )
import Language.Fish.Translator.Variables.Expressions.Words
  ( isNoSplitParamExpansion,
    translateDoubleQuotedExprMWith,
    translateDoubleQuotedExprWith,
    translateWordPartsToExprMWith,
    translateWordPartsToExprWith,
    unwrapBraced,
    wordNeedsSplit,
  )
import Language.Fish.Translator.Variables.Glob
  ( extglobShimListExpr,
    parseGlobPattern,
    renderExtglobForFish,
    renderExtglobRaw,
    renderGlobWord,
    renderGlobWordRaw,
    wordIsGlob,
    wordNeedsExtglobShim,
  )
import Language.Fish.Translator.Variables.ParamExpansion (translateSimpleVar)
import Language.Fish.Translator.Variables.ProcessSubst
  ( procSubListExpr,
  )
import ShellCheck.AST

translateTokenToListExprWith ::
  (Token -> FishExpr TStr) ->
  (Token -> FishExpr (TList TStr)) ->
  ([Token] -> FishExpr (TList TStr)) ->
  (Token -> FishStatement) ->
  Bool ->
  Token ->
  FishExpr (TList TStr)
translateTokenToListExprWith translateTokenToExpr translateDollarBraced commandSubstExprList translateSubstToken splitEnabled = go
  where
    go = \case
      T_Literal _ s -> ExprListLiteral [ExprLiteral (T.pack s)]
      T_Glob _ s -> ExprGlob (parseGlobPattern (toText s))
      T_Extglob _ op parts ->
        case renderExtglobForFish op parts of
          Just pat -> ExprGlob (parseGlobPattern pat)
          Nothing -> extglobShimListExpr (renderExtglobRaw op parts)
      T_SingleQuoted _ s -> ExprListLiteral [ExprLiteral (T.pack s)]
      T_DoubleQuoted _ parts ->
        case parts of
          [tok]
            | isNoSplitParamExpansion tok -> translateDollarBraced (unwrapBraced tok)
          _ -> ExprListLiteral [translateDoubleQuotedExprWith translateTokenToExpr parts]
      T_NormalWord _ parts ->
        case parts of
          [T_DoubleQuoted _ [innerTok]]
            | isNoSplitParamExpansion innerTok -> translateDollarBraced (unwrapBraced innerTok)
          [T_DollarBraced _ _ word]
            | not splitEnabled -> translateDollarBraced word
            | isNoSplitParamExpansion word -> translateDollarBraced word
            | otherwise ->
                let expr = translateWordPartsToExprWith translateTokenToExpr parts
                 in if wordNeedsSplit parts
                      then splitOnIfsExpr expr
                      else ExprListLiteral [expr]
          _
            | wordIsGlob parts ->
                if wordNeedsExtglobShim parts
                  then extglobShimListExpr (renderGlobWordRaw parts)
                  else ExprGlob (parseGlobPattern (renderGlobWord parts))
            | otherwise ->
                let expr = translateWordPartsToExprWith translateTokenToExpr parts
                 in if splitEnabled && wordNeedsSplit parts
                      then splitOnIfsExpr expr
                      else ExprListLiteral [expr]
      tok@T_ParamSubSpecialChar {} -> translateSimpleVar tok
      T_DollarBraced _ _ word
        | not splitEnabled -> translateDollarBraced word
        | isNoSplitParamExpansion word -> translateDollarBraced word
        | otherwise -> splitOnIfsListExpr (translateDollarBraced word)
      T_DollarArithmetic _ exprTok ->
        ExprCommandSubst (Stmt (mathCommandFromToken False exprTok) NE.:| [])
      T_Arithmetic _ exprTok ->
        ExprCommandSubst (Stmt (mathCommandFromToken True exprTok) NE.:| [])
      T_Backticked _ stmts ->
        if splitEnabled
          then splitOnIfsListExpr (commandSubstExprList stmts)
          else commandSubstExprList stmts
      T_DollarExpansion _ stmts ->
        if splitEnabled
          then splitOnIfsListExpr (commandSubstExprList stmts)
          else commandSubstExprList stmts
      T_DollarBraceCommandExpansion _ _ stmts ->
        if splitEnabled
          then splitOnIfsListExpr (commandSubstExprList stmts)
          else commandSubstExprList stmts
      T_ProcSub _ dir stmts ->
        case NE.nonEmpty (map translateStmt stmts) of
          Just neBody -> procSubListExpr dir neBody
          Nothing -> ExprListLiteral []
      T_Array _ elems -> translateArrayElementsWith go elems
      other -> ExprListLiteral [ExprLiteral (tokenToLiteralText other)]

    translateStmt = translateSubstToken

translateTokenToListExprMWith ::
  (Token -> HoistedM (FishExpr TStr)) ->
  (Token -> HoistedM (FishExpr (TList TStr))) ->
  ([Token] -> FishExpr (TList TStr)) ->
  (Token -> FishStatement) ->
  Bool ->
  Token ->
  HoistedM (FishExpr (TList TStr))
translateTokenToListExprMWith translateTokenToExprM translateDollarBracedWithPrelude commandSubstExprList translateSubstToken splitEnabled =
  go
  where
    go :: Token -> HoistedM (FishExpr (TList TStr))
    go = \case
      T_Literal _ s -> hoistM [] (ExprListLiteral [ExprLiteral (T.pack s)])
      T_Glob _ s -> hoistM [] (ExprGlob (parseGlobPattern (toText s)))
      T_Extglob _ op parts ->
        case renderExtglobForFish op parts of
          Just pat -> hoistM [] (ExprGlob (parseGlobPattern pat))
          Nothing -> hoistM [] (extglobShimListExpr (renderExtglobRaw op parts))
      T_SingleQuoted _ s -> hoistM [] (ExprListLiteral [ExprLiteral (T.pack s)])
      T_DoubleQuoted _ parts ->
        case parts of
          [tok]
            | isNoSplitParamExpansion tok ->
                translateDollarBracedWithPrelude (unwrapBraced tok)
          _ -> do
            Hoisted pre expr <- translateDoubleQuotedExprMWith translateTokenToExprM parts
            hoistM pre (ExprListLiteral [expr])
      T_NormalWord _ parts ->
        case parts of
          [T_DoubleQuoted _ [innerTok]]
            | isNoSplitParamExpansion innerTok ->
                translateDollarBracedWithPrelude (unwrapBraced innerTok)
          [T_DollarBraced _ _ word]
            | not splitEnabled -> translateDollarBracedWithPrelude word
            | isNoSplitParamExpansion word -> translateDollarBracedWithPrelude word
            | otherwise -> do
                Hoisted pre expr <- translateWordPartsToExprMWith translateTokenToExprM parts
                let listExpr =
                      if wordNeedsSplit parts
                        then splitOnIfsExpr expr
                        else ExprListLiteral [expr]
                hoistM pre listExpr
          _
            | wordIsGlob parts ->
                if wordNeedsExtglobShim parts
                  then hoistM [] (extglobShimListExpr (renderGlobWordRaw parts))
                  else hoistM [] (ExprGlob (parseGlobPattern (renderGlobWord parts)))
            | otherwise -> do
                Hoisted pre expr <- translateWordPartsToExprMWith translateTokenToExprM parts
                let listExpr =
                      if splitEnabled && wordNeedsSplit parts
                        then splitOnIfsExpr expr
                        else ExprListLiteral [expr]
                hoistM pre listExpr
      tok@T_ParamSubSpecialChar {} -> hoistM [] (translateSimpleVar tok)
      T_DollarBraced _ _ word -> do
        Hoisted pre expr <- translateDollarBracedWithPrelude word
        hoistM pre $
          if not splitEnabled || isNoSplitParamExpansion word
            then expr
            else splitOnIfsListExpr expr
      T_DollarArithmetic _ exprTok -> do
        Hoisted pre args <- arithArgsPlanM exprTok
        let cmd = mathCommandFromArgs False args
        hoistM pre (ExprCommandSubst (Stmt cmd NE.:| []))
      T_Arithmetic _ exprTok -> do
        Hoisted pre args <- arithArgsPlanM exprTok
        let cmd = mathCommandFromArgs True args
        hoistM pre (ExprCommandSubst (Stmt cmd NE.:| []))
      T_Backticked _ stmts ->
        hoistM
          []
          ( if splitEnabled
              then splitOnIfsListExpr (commandSubstExprList stmts)
              else commandSubstExprList stmts
          )
      T_DollarExpansion _ stmts ->
        hoistM
          []
          ( if splitEnabled
              then splitOnIfsListExpr (commandSubstExprList stmts)
              else commandSubstExprList stmts
          )
      T_DollarBraceCommandExpansion _ _ stmts ->
        hoistM
          []
          ( if splitEnabled
              then splitOnIfsListExpr (commandSubstExprList stmts)
              else commandSubstExprList stmts
          )
      T_ProcSub _ dir stmts ->
        case NE.nonEmpty (map translateStmt stmts) of
          Just neBody -> hoistM [] (procSubListExpr dir neBody)
          Nothing -> hoistM [] (ExprListLiteral [])
      T_Array _ elems -> do
        Hoisted pre expr <- translateArrayElementsMWith go elems
        hoistM pre expr
      other -> hoistM [] (ExprListLiteral [ExprLiteral (tokenToLiteralText other)])

    translateStmt = translateSubstToken

translateTokensToListExprWith ::
  (Token -> FishExpr (TList TStr)) ->
  [Token] ->
  FishExpr (TList TStr)
translateTokensToListExprWith translateTokenToListExpr = \case
  [] -> ExprListLiteral []
  [t] -> translateTokenToListExpr t
  (t : ts) ->
    foldl' ExprListConcat (translateTokenToListExpr t) (map translateTokenToListExpr ts)

translateTokensToListExprMWith ::
  (Token -> HoistedM (FishExpr (TList TStr))) ->
  [Token] ->
  HoistedM (FishExpr (TList TStr))
translateTokensToListExprMWith translateTokenToListExprM =
  go
  where
    go :: [Token] -> HoistedM (FishExpr (TList TStr))
    go = \case
      [] -> hoistM [] (ExprListLiteral [])
      tokens -> do
        translated <- mapM translateTokenToListExprM tokens
        let Hoisted pre exprs = sequenceA translated
        case exprs of
          [] -> hoistM pre (ExprListLiteral [])
          (x : xs) -> hoistM pre (foldl' ExprListConcat x xs)

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Variables.Expressions.Core
  ( translateTokenToExprWith,
    translateTokenToExprMWith,
    translateTokenToArgWith,
    translateTokenToArgMWith,
    translateTokenToExprOrRedirectWith,
    translateTokenToExprOrRedirectMWith,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Language.Fish.AST
import Language.Fish.Translator.Args
  ( Arg,
    argExpr,
    renderArg,
  )
import Language.Fish.Translator.Hoist (Hoisted (..))
import Language.Fish.Translator.Hoist.Monad (HoistedM, hoistM)
import Language.Fish.Translator.Token (tokenToLiteralText)
import Language.Fish.Translator.Variables.Arithmetic
  ( arithArgsPlanM,
    mathCommandFromArgs,
    mathCommandFromToken,
  )
import Language.Fish.Translator.Variables.Expressions.Words
  ( translateDoubleQuotedExprMWith,
    translateDoubleQuotedExprWith,
    translateWordPartsToExprMWith,
    translateWordPartsToExprWith,
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
import Language.Fish.Translator.Variables.ProcessSubst (procSubExpr)
import ShellCheck.AST

translateTokenToExprWith ::
  (Token -> FishExpr TStr) ->
  ([Token] -> FishExpr TStr) ->
  Token ->
  FishExpr TStr
translateTokenToExprWith translateDollarBracedStr commandSubstExprStr = go
  where
    go = \case
      T_Literal _ s -> ExprLiteral (T.pack s)
      T_Glob _ s -> ExprJoinList (ExprGlob (parseGlobPattern (toText s)))
      T_Extglob _ op parts ->
        case renderExtglobForFish op parts of
          Just pat -> ExprJoinList (ExprGlob (parseGlobPattern pat))
          Nothing -> ExprJoinList (extglobShimListExpr (renderExtglobRaw op parts))
      T_SingleQuoted _ s -> ExprLiteral (T.pack s)
      T_DoubleQuoted _ parts -> translateDoubleQuotedExprWith go parts
      T_NormalWord _ parts ->
        if wordIsGlob parts
          then
            if wordNeedsExtglobShim parts
              then ExprJoinList (extglobShimListExpr (renderGlobWordRaw parts))
              else ExprJoinList (ExprGlob (parseGlobPattern (renderGlobWord parts)))
          else translateWordPartsToExprWith go parts
      tok@T_ParamSubSpecialChar {} -> ExprJoinList (translateSimpleVar tok)
      T_DollarBraced _ _ word -> translateDollarBracedStr word
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
      -- \${ ... $(cmd) ... } style
      T_DollarBraceCommandExpansion _ _ stmts ->
        commandSubstExprStr stmts
      T_ProcSub _ dir stmts ->
        case NE.nonEmpty (map translateStmt stmts) of
          Just neBody -> procSubExpr dir neBody
          Nothing -> ExprLiteral ""
      -- Fallback: take literal interpretation where possible
      other -> ExprLiteral (tokenToLiteralText other)

    translateStmt t = case t of
      T_Script _ _ ts -> StmtList (map translateStmt ts)
      _ -> Stmt (Command (tokenToLiteralText t) [])

-- | Translate a token to a string expression, hoisting side-effecting expansions
--   into statements that must run before the command.
translateTokenToExprMWith ::
  (Token -> HoistedM (FishExpr TStr)) ->
  ([Token] -> FishExpr TStr) ->
  Token ->
  HoistedM (FishExpr TStr)
translateTokenToExprMWith translateDollarBracedWithPrelude commandSubstExprStr =
  translateTokenToExprHoistedWith translateDollarBracedWithPrelude commandSubstExprStr

translateTokenToExprHoistedWith ::
  (Token -> HoistedM (FishExpr TStr)) ->
  ([Token] -> FishExpr TStr) ->
  Token ->
  HoistedM (FishExpr TStr)
translateTokenToExprHoistedWith translateDollarBracedWithPrelude commandSubstExprStr = go
  where
    go = \case
      T_Literal _ s -> hoistM [] (ExprLiteral (T.pack s))
      T_Glob _ s -> hoistM [] (ExprJoinList (ExprGlob (parseGlobPattern (toText s))))
      T_Extglob _ op parts ->
        case renderExtglobForFish op parts of
          Just pat -> hoistM [] (ExprJoinList (ExprGlob (parseGlobPattern pat)))
          Nothing -> hoistM [] (ExprJoinList (extglobShimListExpr (renderExtglobRaw op parts)))
      T_SingleQuoted _ s -> hoistM [] (ExprLiteral (T.pack s))
      T_DoubleQuoted _ parts -> translateDoubleQuotedExprMWith go parts
      T_NormalWord _ parts ->
        if wordIsGlob parts
          then
            if wordNeedsExtglobShim parts
              then hoistM [] (ExprJoinList (extglobShimListExpr (renderGlobWordRaw parts)))
              else hoistM [] (ExprJoinList (ExprGlob (parseGlobPattern (renderGlobWord parts))))
          else translateWordPartsToExprMWith go parts
      tok@T_ParamSubSpecialChar {} -> hoistM [] (ExprJoinList (translateSimpleVar tok))
      T_DollarBraced _ _ word ->
        translateDollarBracedWithPrelude word
      T_DollarArithmetic _ exprTok -> do
        Hoisted pre args <- arithArgsPlanM exprTok
        let cmd = mathCommandFromArgs False args
        hoistM pre (ExprJoinList (ExprCommandSubst (Stmt cmd NE.:| [])))
      T_Arithmetic _ exprTok -> do
        Hoisted pre args <- arithArgsPlanM exprTok
        let cmd = mathCommandFromArgs True args
        hoistM pre (ExprJoinList (ExprCommandSubst (Stmt cmd NE.:| [])))
      T_Backticked _ stmts ->
        hoistM [] (commandSubstExprStr stmts)
      T_DollarExpansion _ stmts ->
        hoistM [] (commandSubstExprStr stmts)
      T_DollarBraceCommandExpansion _ _ stmts ->
        hoistM [] (commandSubstExprStr stmts)
      T_ProcSub _ dir stmts ->
        case NE.nonEmpty (map translateStmt stmts) of
          Just neBody -> hoistM [] (procSubExpr dir neBody)
          Nothing -> hoistM [] (ExprLiteral "")
      other -> hoistM [] (ExprLiteral (tokenToLiteralText other))

    translateStmt t = case t of
      T_Script _ _ ts -> StmtList (map translateStmt ts)
      _ -> Stmt (Command (tokenToLiteralText t) [])

translateTokenToExprOrRedirectWith ::
  (Token -> FishExpr (TList TStr)) ->
  Token ->
  ExprOrRedirect
translateTokenToExprOrRedirectWith translateTokenToListExpr tok =
  renderArg (translateTokenToArgWith translateTokenToListExpr tok)

translateTokenToExprOrRedirectMWith ::
  (Token -> HoistedM (FishExpr (TList TStr))) ->
  Token ->
  HoistedM ExprOrRedirect
translateTokenToExprOrRedirectMWith translateTokenToListExprM =
  translateTokenToExprOrRedirectHoistedWith translateTokenToListExprM

translateTokenToArgWith ::
  (Token -> FishExpr (TList TStr)) ->
  Token ->
  Arg
translateTokenToArgWith translateTokenToListExpr tok =
  argExpr (translateTokenToListExpr tok)

translateTokenToArgMWith ::
  (Token -> HoistedM (FishExpr (TList TStr))) ->
  Token ->
  HoistedM Arg
translateTokenToArgMWith translateTokenToListExprM tok = do
  Hoisted pre expr <- translateTokenToListExprM tok
  hoistM pre (argExpr expr)

translateTokenToExprOrRedirectHoistedWith ::
  (Token -> HoistedM (FishExpr (TList TStr))) ->
  Token ->
  HoistedM ExprOrRedirect
translateTokenToExprOrRedirectHoistedWith translateTokenToListExprM tok = do
  Hoisted pre arg <- translateTokenToArgMWith translateTokenToListExprM tok
  hoistM pre (renderArg arg)

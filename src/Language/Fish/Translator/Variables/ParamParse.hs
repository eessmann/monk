{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Variables.ParamParse
  ( translateDollarBracedWith,
    translateDollarBracedStrWith,
    translateDollarBracedWithPreludeWith,
    translateDollarBracedStrWithPreludeWith,
  )
where

import Language.Fish.AST
import Language.Fish.Translator.Hoist.Monad (HoistedM)
import Language.Fish.Translator.Variables.ParamExpansion
  ( parseParamExpansion,
    parseParamExpansionStr,
    renderParamExpansion,
    renderParamExpansionWithPrelude,
  )
import ShellCheck.AST

translateDollarBracedWith :: ([Token] -> FishExpr (TList TStr)) -> Token -> FishExpr (TList TStr)
translateDollarBracedWith tokensToListExpr word =
  renderParamExpansion tokensToListExpr (parseParamExpansion word)

translateDollarBracedStrWith :: ([Token] -> FishExpr (TList TStr)) -> Token -> FishExpr TStr
translateDollarBracedStrWith tokensToListExpr word =
  renderParamExpansion tokensToListExpr (parseParamExpansionStr word)

translateDollarBracedWithPreludeWith ::
  ([Token] -> FishExpr (TList TStr)) ->
  ([Token] -> HoistedM (FishExpr (TList TStr))) ->
  Token ->
  HoistedM (FishExpr (TList TStr))
translateDollarBracedWithPreludeWith tokensToListExpr tokensToListExprM word =
  renderParamExpansionWithPrelude tokensToListExpr tokensToListExprM (parseParamExpansion word)

translateDollarBracedStrWithPreludeWith ::
  ([Token] -> FishExpr (TList TStr)) ->
  ([Token] -> HoistedM (FishExpr (TList TStr))) ->
  Token ->
  HoistedM (FishExpr TStr)
translateDollarBracedStrWithPreludeWith tokensToListExpr tokensToListExprM word =
  renderParamExpansionWithPrelude tokensToListExpr tokensToListExprM (parseParamExpansionStr word)

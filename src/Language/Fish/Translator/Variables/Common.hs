{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Variables.Common
  ( paramNameFrom,
    specialVarName,
    scopeFlagsForVarM,
  )
where

import Prelude hiding (gets)
import Data.Text qualified as T
import Language.Fish.AST
import Language.Fish.Translator.Monad (TranslationContext (..), TranslateM, context, isLocalVar)
import Language.Fish.Translator.Token (tokenRawText)
import Polysemy.State (gets)
import ShellCheck.AST (Token)
import ShellCheck.ASTLib (getBracedReference)

paramNameFrom :: Token -> Maybe Text
paramNameFrom word =
  let rawTxt = tokenRawText word
      nameTxt = toText (getBracedReference (toString rawTxt))
      fallback = T.takeWhile (/= '[') rawTxt
      finalName = if T.null nameTxt then fallback else nameTxt
   in if T.null finalName then Nothing else Just finalName

specialVarName :: Text -> Text
specialVarName = \case
  "?" -> "status"
  "$" -> "fish_pid"
  "!" -> "last_pid"
  "@" -> "argv"
  "*" -> "argv"
  n -> n

scopeFlagsForVarM :: Text -> TranslateM [SetFlag]
scopeFlagsForVarM name = do
  isLocal <- isLocalVar name
  inFunc <- gets (inFunction . context)
  let localFlag = if inFunc then SetFunction else SetLocal
  pure (if isLocal then [localFlag] else [SetGlobal])

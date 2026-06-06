{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Variables.Common
  ( paramNameFrom,
    specialVarName,
    scopeFlagsForVarM,
  )
where

import Control.Monad.State.Strict (gets)
import Data.Text qualified as T
import Language.Fish.Translator.Monad (TranslateM, TranslationContext (..), context, isLocalVar)
import Language.Fish.Translator.Syntax
import Language.Fish.Translator.Token (tokenRawText)
import ShellCheck.AST (Token)
import ShellCheck.ASTLib (getBracedReference)
import Prelude hiding (gets)

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
  "!" -> "__monk_last_job"
  "@" -> "argv"
  "*" -> "argv"
  n -> n

scopeFlagsForVarM :: Text -> TranslateM [SetFlag]
scopeFlagsForVarM name = do
  isLocal <- isLocalVar name
  inFunc <- gets (inFunction . context)
  let localFlag = if inFunc then SetFunction else SetLocal
  pure (if isLocal then [localFlag] else [SetGlobal])

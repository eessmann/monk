{-# LANGUAGE LambdaCase #-}

module Language.Fish.Translator.Token
  ( tokenToLiteralText,
    tokenRawText,
    tokenHasExpansion,
    wordHasExpansion,
  )
where

import Data.Text qualified as T
import ShellCheck.AST
import ShellCheck.ASTLib (getLiteralStringDef, oversimplify)

-- | Utility: get literal text from a token if available.
tokenToLiteralText :: Token -> Text
tokenToLiteralText = T.pack . getLiteralStringDef ""

-- | Best-effort raw token text for cases where the literal is empty.
tokenRawText :: Token -> Text
tokenRawText tok =
  let literal = tokenToLiteralText tok
   in if T.null literal
        then toText (concat (oversimplify tok))
        else literal

tokenHasExpansion :: Token -> Bool
tokenHasExpansion = \case
  T_NormalWord _ parts -> wordHasExpansion parts
  T_DoubleQuoted _ parts -> wordHasExpansion parts
  T_DollarBraced {} -> True
  T_DollarArithmetic {} -> True
  T_Arithmetic {} -> True
  T_DollarExpansion {} -> True
  T_Backticked {} -> True
  T_DollarBraceCommandExpansion {} -> True
  T_ProcSub {} -> True
  _ -> False

wordHasExpansion :: [Token] -> Bool
wordHasExpansion = any isExpansionPart
  where
    isExpansionPart = \case
      T_DollarBraced {} -> True
      T_DollarArithmetic {} -> True
      T_Arithmetic {} -> True
      T_DollarExpansion {} -> True
      T_Backticked {} -> True
      T_DollarBraceCommandExpansion {} -> True
      T_ProcSub {} -> True
      _ -> False

module Language.Fish.Translator.Variables.Expressions.Subst
  ( commandSubstExprList,
    commandSubstExprStr,
    translateSubstTokenWith,
  )
where

import Language.Fish.AST
import Language.Fish.Translator.Variables.Substitution
  ( commandSubstExprListWith,
    commandSubstExprStrWith,
    translateSubstTokenWith,
  )
import ShellCheck.AST

commandSubstExprList :: (Token -> FishStatement) -> [Token] -> FishExpr (TList TStr)
commandSubstExprList = commandSubstExprListWith

commandSubstExprStr :: (Token -> FishStatement) -> [Token] -> FishExpr TStr
commandSubstExprStr = commandSubstExprStrWith

module Language.Fish.Translator.Variables.Expressions.Subst
  ( commandSubstExprList,
    commandSubstExprListM,
    commandSubstExprStr,
    commandSubstExprStrM,
    translateSubstTokenWith,
    translateSubstTokenMWith,
  )
where

import Language.Fish.AST
import Language.Fish.Translator.Monad (TranslateM)
import Language.Fish.Translator.Variables.Substitution
  ( commandSubstExprListWith,
    commandSubstExprListMWith,
    commandSubstExprStrWith,
    commandSubstExprStrMWith,
    translateSubstTokenWith,
    translateSubstTokenMWith,
  )
import ShellCheck.AST

commandSubstExprList :: (Token -> FishStatement) -> [Token] -> FishExpr (TList TStr)
commandSubstExprList = commandSubstExprListWith

commandSubstExprListM :: (Token -> TranslateM FishStatement) -> [Token] -> TranslateM (FishExpr (TList TStr))
commandSubstExprListM = commandSubstExprListMWith

commandSubstExprStr :: (Token -> FishStatement) -> [Token] -> FishExpr TStr
commandSubstExprStr = commandSubstExprStrWith

commandSubstExprStrM :: (Token -> TranslateM FishStatement) -> [Token] -> TranslateM (FishExpr TStr)
commandSubstExprStrM = commandSubstExprStrMWith

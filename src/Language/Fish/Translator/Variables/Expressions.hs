module Language.Fish.Translator.Variables.Expressions
  ( translateTokenToExpr,
    translateTokenToExprM,
    translateTokenToListExpr,
    translateTokenToListExprM,
    translateTokenToListExprNoSplit,
    translateTokenToListExprMNoSplit,
    translateTokenToArg,
    translateTokenToArgM,
    translateTokenToExprOrRedirect,
    translateTokenToExprOrRedirectM,
    translateAssignment,
    translateAssignmentWithFlags,
    translateAssignmentWithFlagsM,
    patternExprFromToken,
    translateTokensToListExpr,
    translateTokensToListExprM,
    translateArrayElements,
    translateArrayElementsM,
  )
where

import Data.Text qualified as T
import Language.Fish.AST
import Language.Fish.Translator.Args (Arg)
import Language.Fish.Translator.Hoist (Hoisted (..))
import Language.Fish.Translator.Hoist.Monad (HoistedM)
import Language.Fish.Translator.Monad (TranslateM)
import Language.Fish.Translator.Variables.Expressions.Arrays
  ( translateArrayAssignmentMWith,
    translateArrayAssignmentWith,
    translateArrayElementsMWith,
    translateArrayElementsWith,
  )
import Language.Fish.Translator.Variables.Expressions.Core
  ( translateTokenToArgMWith,
    translateTokenToArgWith,
    translateTokenToExprMWith,
    translateTokenToExprOrRedirectMWith,
    translateTokenToExprOrRedirectWith,
    translateTokenToExprWith,
  )
import Language.Fish.Translator.Variables.Expressions.Lists
  ( translateTokenToListExprMWith,
    translateTokenToListExprWith,
    translateTokensToListExprMWith,
    translateTokensToListExprWith,
  )
import Language.Fish.Translator.Variables.Expressions.Subst qualified as Subst
import Language.Fish.Translator.Variables.Glob (patternExprFromToken)
import Language.Fish.Translator.Variables.Index (indexedVarText)
import Language.Fish.Translator.Variables.ParamParse
  ( translateDollarBracedWith,
    translateDollarBracedStrWith,
    translateDollarBracedWithPreludeWith,
    translateDollarBracedStrWithPreludeWith,
  )
import ShellCheck.AST

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

translateTokenToExpr :: Token -> FishExpr TStr
translateTokenToExpr =
  translateTokenToExprWith translateDollarBracedStr commandSubstExprStr

translateTokenToExprM :: Token -> HoistedM (FishExpr TStr)
translateTokenToExprM =
  translateTokenToExprMWith translateDollarBracedStrWithPrelude commandSubstExprStr

translateTokenToListExpr :: Token -> FishExpr (TList TStr)
translateTokenToListExpr =
  translateTokenToListExprWith
    translateTokenToExpr
    translateDollarBraced
    commandSubstExprList
    translateSubstToken
    True

translateTokenToListExprNoSplit :: Token -> FishExpr (TList TStr)
translateTokenToListExprNoSplit =
  translateTokenToListExprWith
    translateTokenToExpr
    translateDollarBraced
    commandSubstExprList
    translateSubstToken
    False

translateTokenToListExprM :: Token -> HoistedM (FishExpr (TList TStr))
translateTokenToListExprM =
  translateTokenToListExprMWith
    translateTokenToExprM
    translateDollarBracedWithPrelude
    commandSubstExprList
    translateSubstToken
    True

translateTokenToListExprMNoSplit :: Token -> HoistedM (FishExpr (TList TStr))
translateTokenToListExprMNoSplit =
  translateTokenToListExprMWith
    translateTokenToExprM
    translateDollarBracedWithPrelude
    commandSubstExprList
    translateSubstToken
    False

translateTokenToExprOrRedirect :: Token -> ExprOrRedirect
translateTokenToExprOrRedirect = translateTokenToExprOrRedirectWith translateTokenToListExpr

translateTokenToExprOrRedirectM :: Token -> HoistedM ExprOrRedirect
translateTokenToExprOrRedirectM = translateTokenToExprOrRedirectMWith translateTokenToListExprM

translateTokenToArg :: Token -> Arg
translateTokenToArg = translateTokenToArgWith translateTokenToListExpr

translateTokenToArgM :: Token -> HoistedM Arg
translateTokenToArgM = translateTokenToArgMWith translateTokenToListExprM

--------------------------------------------------------------------------------
-- Assignments
--------------------------------------------------------------------------------

translateAssignment :: Token -> [FishStatement]
translateAssignment = translateAssignmentWithFlags [SetGlobal]

translateAssignmentWithFlags :: [SetFlag] -> Token -> [FishStatement]
translateAssignmentWithFlags baseFlags tok =
  case tok of
    T_Assignment _ mode var indices val ->
      let fishVar = T.pack var
          flags = case mode of
            Assign -> baseFlags
            Append -> baseFlags <> [SetAppend]
          indexedVar = indexedVarText fishVar indices
       in case indexedVar of
            Just varTxt ->
              [Stmt (Set flags varTxt (translateTokenToListExprNoSplit val))]
            Nothing ->
              case val of
                T_Array _ elems ->
                  translateArrayAssignment fishVar flags elems
                _ ->
                  [Stmt (Set flags fishVar (translateTokenToListExprNoSplit val))]
    _ -> []

translateAssignmentWithFlagsM :: [SetFlag] -> Token -> TranslateM [FishStatement]
translateAssignmentWithFlagsM baseFlags tok =
  case tok of
    T_Assignment _ mode var indices val -> do
      let fishVar = T.pack var
          flags = case mode of
            Assign -> baseFlags
            Append -> baseFlags <> [SetAppend]
          indexedVar = indexedVarText fishVar indices
      case indexedVar of
        Just varTxt -> do
          Hoisted pre expr <- translateTokenToListExprMNoSplit val
          pure (pre <> [Stmt (Set flags varTxt expr)])
        Nothing ->
          case val of
            T_Array _ elems -> translateArrayAssignmentM fishVar flags elems
            _ -> do
              Hoisted pre expr <- translateTokenToListExprMNoSplit val
              pure (pre <> [Stmt (Set flags fishVar expr)])
    _ -> pure []

-- Parameter expansion helpers
--------------------------------------------------------------------------------

translateDollarBraced :: Token -> FishExpr (TList TStr)
translateDollarBraced = translateDollarBracedWith translateTokensToListExpr

translateDollarBracedStr :: Token -> FishExpr TStr
translateDollarBracedStr = translateDollarBracedStrWith translateTokensToListExpr

translateDollarBracedWithPrelude :: Token -> HoistedM (FishExpr (TList TStr))
translateDollarBracedWithPrelude =
  translateDollarBracedWithPreludeWith translateTokensToListExpr translateTokensToListExprM

translateDollarBracedStrWithPrelude :: Token -> HoistedM (FishExpr TStr)
translateDollarBracedStrWithPrelude =
  translateDollarBracedStrWithPreludeWith translateTokensToListExpr translateTokensToListExprM

translateTokensToListExpr :: [Token] -> FishExpr (TList TStr)
translateTokensToListExpr = translateTokensToListExprWith translateTokenToListExpr

translateTokensToListExprM :: [Token] -> HoistedM (FishExpr (TList TStr))
translateTokensToListExprM = translateTokensToListExprMWith translateTokenToListExprM

translateArrayElements :: [Token] -> FishExpr (TList TStr)
translateArrayElements = translateArrayElementsWith translateTokenToListExpr

translateArrayElementsM :: [Token] -> HoistedM (FishExpr (TList TStr))
translateArrayElementsM = translateArrayElementsMWith translateTokenToListExprM

translateArrayAssignment :: Text -> [SetFlag] -> [Token] -> [FishStatement]
translateArrayAssignment = translateArrayAssignmentWith translateTokenToListExpr translateTokenToListExprNoSplit

translateArrayAssignmentM :: Text -> [SetFlag] -> [Token] -> TranslateM [FishStatement]
translateArrayAssignmentM = translateArrayAssignmentMWith translateTokenToListExprM translateTokenToListExprMNoSplit

--------------------------------------------------------------------------------
-- Command substitution helpers (nested substitutions)
--------------------------------------------------------------------------------

commandSubstExprList :: [Token] -> FishExpr (TList TStr)
commandSubstExprList = Subst.commandSubstExprList translateSubstToken

commandSubstExprStr :: [Token] -> FishExpr TStr
commandSubstExprStr = Subst.commandSubstExprStr translateSubstToken

translateSubstToken :: Token -> FishStatement
translateSubstToken =
  Subst.translateSubstTokenWith translateAssignmentWithFlags translateTokenToExpr translateTokenToExprOrRedirect

{-# LANGUAGE LambdaCase #-}

-- | Parser inspection and initial name inventories. These are the original
-- bootstrap helpers; executable syntax still enters one normalization walker.
module Language.Bash.Plan.Normalize.Syntax (parameterText, functionNames, sourceNames, children, tokenKind) where

import Data.Set qualified as S
import ShellCheck.AST

parameterText :: Token -> Maybe Text
parameterText = \case
  T_Literal _ value -> Just (toText value)
  T_ParamSubSpecialChar _ value -> Just (toText value)
  T_NormalWord _ values -> mconcat <$> traverse parameterText values
  _ -> Nothing

functionNames :: Token -> S.Set Text
functionNames token = case token of
  T_Function _ _ _ name _ -> S.singleton (toText name) <> foldMap functionNames (children token)
  _ -> foldMap functionNames (children token)

sourceNames :: Token -> S.Set Text
sourceNames token = maybe mempty S.singleton (parameterText token) <> foldMap sourceNames (children token)

children :: Token -> [Token]
children (OuterToken _ inner) = toList inner

-- Every unknown constructor is rejected. This exhaustive name mapping also
-- makes parser dependency additions visible at compile time.
tokenKind :: Token -> Text
tokenKind = \case
  T_AND_IF {} -> "T_AND_IF"
  T_AndIf {} -> "T_AndIf"
  T_Annotation {} -> "T_Annotation"
  T_Arithmetic {} -> "T_Arithmetic"
  T_Array {} -> "T_Array"
  T_Assignment {} -> "T_Assignment"
  T_Backgrounded {} -> "T_Backgrounded"
  T_Backticked {} -> "T_Backticked"
  T_Bang {} -> "T_Bang"
  T_Banged {} -> "T_Banged"
  T_BatsTest {} -> "T_BatsTest"
  T_BraceExpansion {} -> "T_BraceExpansion"
  T_BraceGroup {} -> "T_BraceGroup"
  T_CLOBBER {} -> "T_CLOBBER"
  T_Case {} -> "T_Case"
  T_CaseExpression {} -> "T_CaseExpression"
  T_CoProc {} -> "T_CoProc"
  T_CoProcBody {} -> "T_CoProcBody"
  T_Condition {} -> "T_Condition"
  T_DGREAT {} -> "T_DGREAT"
  T_DLESS {} -> "T_DLESS"
  T_DLESSDASH {} -> "T_DLESSDASH"
  T_DSEMI {} -> "T_DSEMI"
  T_Do {} -> "T_Do"
  T_DollarArithmetic {} -> "T_DollarArithmetic"
  T_DollarBraceCommandExpansion {} -> "T_DollarBraceCommandExpansion"
  T_DollarBraced {} -> "T_DollarBraced"
  T_DollarBracket {} -> "T_DollarBracket"
  T_DollarDoubleQuoted {} -> "T_DollarDoubleQuoted"
  T_DollarExpansion {} -> "T_DollarExpansion"
  T_DollarSingleQuoted {} -> "T_DollarSingleQuoted"
  T_Done {} -> "T_Done"
  T_DoubleQuoted {} -> "T_DoubleQuoted"
  T_EOF {} -> "T_EOF"
  T_Elif {} -> "T_Elif"
  T_Else {} -> "T_Else"
  T_Esac {} -> "T_Esac"
  T_Extglob {} -> "T_Extglob"
  T_FdRedirect {} -> "T_FdRedirect"
  T_Fi {} -> "T_Fi"
  T_For {} -> "T_For"
  T_ForArithmetic {} -> "T_ForArithmetic"
  T_ForIn {} -> "T_ForIn"
  T_Function {} -> "T_Function"
  T_GREATAND {} -> "T_GREATAND"
  T_Glob {} -> "T_Glob"
  T_Greater {} -> "T_Greater"
  T_HereDoc {} -> "T_HereDoc"
  T_HereString {} -> "T_HereString"
  T_If {} -> "T_If"
  T_IfExpression {} -> "T_IfExpression"
  T_In {} -> "T_In"
  T_Include {} -> "T_Include"
  T_IndexedElement {} -> "T_IndexedElement"
  T_IoDuplicate {} -> "T_IoDuplicate"
  T_IoFile {} -> "T_IoFile"
  T_LESSAND {} -> "T_LESSAND"
  T_LESSGREAT {} -> "T_LESSGREAT"
  T_Lbrace {} -> "T_Lbrace"
  T_Less {} -> "T_Less"
  T_Literal {} -> "T_Literal"
  T_Lparen {} -> "T_Lparen"
  T_NEWLINE {} -> "T_NEWLINE"
  T_NormalWord {} -> "T_NormalWord"
  T_OR_IF {} -> "T_OR_IF"
  T_OrIf {} -> "T_OrIf"
  T_ParamSubSpecialChar {} -> "T_ParamSubSpecialChar"
  T_Pipe {} -> "T_Pipe"
  T_Pipeline {} -> "T_Pipeline"
  T_ProcSub {} -> "T_ProcSub"
  T_Rbrace {} -> "T_Rbrace"
  T_Redirecting {} -> "T_Redirecting"
  T_Rparen {} -> "T_Rparen"
  T_Script {} -> "T_Script"
  T_Select {} -> "T_Select"
  T_SelectIn {} -> "T_SelectIn"
  T_Semi {} -> "T_Semi"
  T_SimpleCommand {} -> "T_SimpleCommand"
  T_SingleQuoted {} -> "T_SingleQuoted"
  T_SourceCommand {} -> "T_SourceCommand"
  T_Subshell {} -> "T_Subshell"
  T_Then {} -> "T_Then"
  T_UnparsedIndex {} -> "T_UnparsedIndex"
  T_Until {} -> "T_Until"
  T_UntilExpression {} -> "T_UntilExpression"
  T_While {} -> "T_While"
  T_WhileExpression {} -> "T_WhileExpression"
  TA_Assignment {} -> "TA_Assignment"
  TA_Binary {} -> "TA_Binary"
  TA_Expansion {} -> "TA_Expansion"
  TA_Parenthesis {} -> "TA_Parenthesis"
  TA_Sequence {} -> "TA_Sequence"
  TA_Trinary {} -> "TA_Trinary"
  TA_Unary {} -> "TA_Unary"
  TA_Variable {} -> "TA_Variable"
  TC_And {} -> "TC_And"
  TC_Binary {} -> "TC_Binary"
  TC_Empty {} -> "TC_Empty"
  TC_Group {} -> "TC_Group"
  TC_Nullary {} -> "TC_Nullary"
  TC_Or {} -> "TC_Or"
  TC_Unary {} -> "TC_Unary"

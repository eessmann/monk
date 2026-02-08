module Language.Fish.Translator.Variables
  ( translateTokenToExpr,
    translateTokenToExprM,
    translateTokenToListExpr,
    translateTokenToListExprM,
    translateTokenToArg,
    translateTokenToArgM,
    translateTokenToExprOrRedirect,
    translateTokenToExprOrRedirectM,
    translateAssignment,
    translateAssignmentWithFlags,
    translateAssignmentWithFlagsM,
    translateArithmetic,
    translateArithmeticStatusM,
    arithArgsFromToken,
    arithArgsFromText,
    tokenToLiteralText,
    patternExprFromToken,
  )
where

import Language.Fish.Translator.Token (tokenToLiteralText)
import Language.Fish.Translator.Variables.Arithmetic
  ( arithArgsFromText,
    arithArgsFromToken,
    translateArithmetic,
    translateArithmeticStatusM,
  )
import Language.Fish.Translator.Variables.Assignments
  ( translateAssignment,
    translateAssignmentWithFlags,
    translateAssignmentWithFlagsM,
  )
import Language.Fish.Translator.Variables.Expressions
  ( patternExprFromToken,
    translateTokenToArg,
    translateTokenToArgM,
    translateTokenToExpr,
    translateTokenToExprM,
    translateTokenToExprOrRedirect,
    translateTokenToExprOrRedirectM,
    translateTokenToListExpr,
    translateTokenToListExprM,
  )

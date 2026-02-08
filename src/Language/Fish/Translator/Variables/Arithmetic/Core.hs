{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Variables.Arithmetic.Core
  ( translateArithmetic,
    translateArithmeticStatusM,
    arithArgsFromToken,
    arithArgsFromText,
    arithArgsPlanM,
    arithStatementPlanM,
    arithVarName,
    mathCommandFromToken,
    mathCommandFromArgs,
    mathSubstFromArgs,
    mathSubstFromArgsNoScale,
  )
where

import Data.List.NonEmpty qualified as NE
import Language.Fish.AST
import Language.Fish.Translator.Cond
  ( testBinaryCommand,
    testNonZeroCommand,
  )
import Language.Fish.Translator.Hoist (Hoisted (..), beginIfNeeded)
import Language.Fish.Translator.Hoist.Monad (HoistedM, hoistM)
import Language.Fish.Translator.Monad (TranslateM)
import Language.Fish.Translator.Variables.Arithmetic.Expr
  ( arithArgsFromTokenList,
    arithArgsPlan,
  )
import Language.Fish.Translator.Variables.Arithmetic.Helpers
  ( arithArgsFromText,
    arithCompareOp,
    arithVarName,
    mathSubstFromArgs,
    mathSubstFromArgsNoScale,
    stripArithWrappers,
  )
import ShellCheck.AST

translateArithmetic :: Token -> FishCommand TStatus
translateArithmetic exprToken =
  case stripArithWrappers exprToken of
    TA_Binary _ op lhs rhs
      | Just testOp <- arithCompareOp (toText op) ->
          let lhsExpr = ExprMath (arithArgsFromToken lhs)
              rhsExpr = ExprMath (arithArgsFromToken rhs)
           in testBinaryCommand testOp lhsExpr rhsExpr
    _ ->
      let expr = ExprMath (arithArgsFromToken exprToken)
       in testNonZeroCommand expr

translateArithmeticStatusM :: Token -> TranslateM (FishCommand TStatus)
translateArithmeticStatusM exprTok =
  case stripArithWrappers exprTok of
    TA_Binary _ op lhs rhs
      | Just testOp <- arithCompareOp (toText op) -> do
          Hoisted preL lhsExpr <- arithStatementPlanM lhs
          Hoisted preR rhsExpr <- arithStatementPlanM rhs
          let testCmd = testBinaryCommand testOp lhsExpr rhsExpr
              pre = preL <> preR
          pure (beginIfNeeded pre testCmd)
    _ -> do
      Hoisted pre valueExpr <- arithStatementPlanM exprTok
      let testCmd = testNonZeroCommand valueExpr
      pure (beginIfNeeded pre testCmd)

arithArgsFromToken :: Token -> NonEmpty (FishExpr TStr)
arithArgsFromToken exprToken =
  fromMaybe (ExprLiteral "0" NE.:| []) (NE.nonEmpty (arithArgsFromTokenList exprToken))

arithArgsPlanM :: Token -> HoistedM (NonEmpty (FishExpr TStr))
arithArgsPlanM exprTok = do
  Hoisted pre args <- arithArgsPlan exprTok
  hoistM pre (fromMaybe (ExprLiteral "0" NE.:| []) (NE.nonEmpty args))

arithStatementPlanM :: Token -> HoistedM (FishExpr TInt)
arithStatementPlanM exprTok = do
  Hoisted pre args <- arithArgsPlanM exprTok
  hoistM pre (ExprMath args)

mathCommandFromToken :: Bool -> Token -> FishCommand TStatus
mathCommandFromToken suppressOutput exprToken =
  mathCommandFromArgs suppressOutput (arithArgsFromToken exprToken)

mathCommandFromArgs :: Bool -> NonEmpty (FishExpr TStr) -> FishCommand TStatus
mathCommandFromArgs suppressOutput args =
  let scaleArgs = [ExprVal (ExprLiteral "--scale"), ExprVal (ExprLiteral "0")]
      mathArgs = scaleArgs <> map ExprVal (NE.toList args)
      mathRedir = RedirectVal (Redirect RedirectStdout RedirectOut (RedirectFile (ExprLiteral "/dev/null")))
      allArgs = if suppressOutput then mathArgs <> [mathRedir] else mathArgs
   in Command "math" allArgs

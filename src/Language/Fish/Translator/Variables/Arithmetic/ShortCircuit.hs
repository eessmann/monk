{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Variables.Arithmetic.ShortCircuit
  ( arithShortCircuitPlan,
    arithTernaryPlan,
  )
where

import Data.List.NonEmpty qualified as NE
import Language.Fish.AST
import Language.Fish.Translator.Cond (testNonZeroCommand)
import Language.Fish.Translator.Hoist (Hoisted (..))
import Language.Fish.Translator.Hoist.Monad (HoistedM, hoistM)
import Language.Fish.Translator.Pipeline (pipelineOf)
import Language.Fish.Translator.Variables.Arithmetic.Helpers
  ( arithTempNameWith,
    ensureArithArgs,
    mathSubstFromArgs,
  )
import ShellCheck.AST

arithShortCircuitPlan ::
  (Token -> HoistedM [FishExpr TStr]) ->
  Token ->
  Text ->
  Token ->
  Token ->
  HoistedM [FishExpr TStr]
arithShortCircuitPlan arithArgsPlan tok opTxt l r = do
  Hoisted preL argsL <- arithArgsPlan l
  Hoisted preR argsR <- arithArgsPlan r
  let condVar = arithTempNameWith tok "cond"
      rhsVar = arithTempNameWith tok "rhs"
      resVar = arithTempNameWith tok "res"
      setCond = setLocalFromArgs condVar (fromMaybe (ExprLiteral "0" NE.:| []) (NE.nonEmpty (ensureArithArgs argsL)))
      setRhs = setLocalFromArgs rhsVar (fromMaybe (ExprLiteral "0" NE.:| []) (NE.nonEmpty (ensureArithArgs argsR)))
      setResTrue = setLocalLiteral resVar "1"
      setResFalse = setLocalLiteral resVar "0"
      rhsCond = testNonZeroCond rhsVar
      rhsIf = Stmt (If rhsCond (setResTrue NE.:| []) [setResFalse] [])
      thenStmts =
        if opTxt == "&&"
          then preR <> [setRhs, rhsIf]
          else [setResTrue]
      elseStmts =
        if opTxt == "&&"
          then [setResFalse]
          else preR <> [setRhs, rhsIf]
      condJob = testNonZeroCond condVar
      ifStmt =
        Stmt
          ( If
              condJob
              (NE.fromList thenStmts)
              elseStmts
              []
          )
  hoistM (preL <> [setCond, ifStmt]) [ExprVariable (VarScalar resVar)]

arithTernaryPlan ::
  (Token -> HoistedM [FishExpr TStr]) ->
  Token ->
  Token ->
  Token ->
  Token ->
  HoistedM [FishExpr TStr]
arithTernaryPlan arithArgsPlan tok condTok thenTok elseTok = do
  Hoisted preCond argsCond <- arithArgsPlan condTok
  Hoisted preThen argsThen <- arithArgsPlan thenTok
  Hoisted preElse argsElse <- arithArgsPlan elseTok
  let condVar = arithTempNameWith tok "cond"
      resVar = arithTempNameWith tok "res"
      setCond = setLocalFromArgs condVar (fromMaybe (ExprLiteral "0" NE.:| []) (NE.nonEmpty (ensureArithArgs argsCond)))
      setThen = setLocalFromArgs resVar (fromMaybe (ExprLiteral "0" NE.:| []) (NE.nonEmpty (ensureArithArgs argsThen)))
      setElse = setLocalFromArgs resVar (fromMaybe (ExprLiteral "0" NE.:| []) (NE.nonEmpty (ensureArithArgs argsElse)))
      condJob = testNonZeroCond condVar
      ifStmt =
        Stmt
          ( If
              condJob
              (NE.fromList (preThen <> [setThen]))
              (preElse <> [setElse])
              []
          )
  hoistM (preCond <> [setCond, ifStmt]) [ExprVariable (VarScalar resVar)]

setLocalFromArgs :: Text -> NonEmpty (FishExpr TStr) -> FishStatement
setLocalFromArgs name args =
  Stmt (Set [SetLocal] name (mathSubstFromArgs args))

setLocalLiteral :: Text -> Text -> FishStatement
setLocalLiteral name val =
  Stmt (Set [SetLocal] name (ExprListLiteral [ExprLiteral val]))

testNonZeroCond :: Text -> FishJobList
testNonZeroCond name =
  jobListFromStatus
    (testNonZeroCommand (ExprVariable (VarScalar name)))

jobListFromStatus :: FishCommand TStatus -> FishJobList
jobListFromStatus cmd =
  FishJobList (FishJobConjunction Nothing (pipelineOf cmd) [] NE.:| [])

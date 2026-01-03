{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Gen
  ( genTextNoQuote,
    genExprStr,
    genStatusCommand,
    genPipeline,
    genConjunction,
    genNonEmptyStmts,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Monk
import Test.QuickCheck

-- Basic text generator avoiding single quotes to simplify pretty expectations
genTextNoQuote :: Gen Text
genTextNoQuote = do
  chars <- listOf (suchThat arbitrary (/= '\''))
  pure (T.pack (take 10 chars))

genExprStr :: Gen (FishExpr 'TStr)
genExprStr =
  oneof
    [ ExprLiteral <$> genTextNoQuote,
      pure (ExprProcessSubst (NE.fromList [Stmt (Command "echo" [])]))
    ]

genStatusCommand :: Gen (FishCommand 'TStatus)
genStatusCommand =
  oneof
    [ pure (Command "true" []),
      pure (Command "false" []),
      do
        t <- genTextNoQuote
        pure (Command "echo" [ExprVal (ExprLiteral t)]),
      pure (Exit Nothing),
      do
        n <- chooseInt (0, 3)
        pure (Exit (Just (ExprNumLiteral n))),
      do
        t <- genTextNoQuote
        pure (Eval (ExprLiteral t)),
      do
        f <- genTextNoQuote
        pure (Source (ExprLiteral f)),
      pure (Exec (ExprLiteral "true") []),
      pure (Read [ReadPrompt "Enter:", ReadLocal] ["x"])
    ]

genPipeline :: Gen FishJobPipeline
genPipeline = do
  headCmd <- genStatusCommand
  k <- chooseInt (0, 3)
  contCmds <- vectorOf k genStatusCommand
  bg <- arbitrary
  let toCont c = PipeTo {jpcVariables = [], jpcStatement = Stmt c}
  pure
    FishJobPipeline
      { jpTime = False,
        jpVariables = [],
        jpStatement = Stmt headCmd,
        jpCont = map toCont contCmds,
        jpBackgrounded = bg
      }

genConjunction :: Gen FishJobConjunction
genConjunction = do
  headP <- genPipeline
  k <- chooseInt (0, 3)
  bools <- vectorOf k arbitrary
  tailPipes <- vectorOf k genPipeline
  let mk b p = if b then JCAnd p else JCOr p
  pure
    FishJobConjunction
      { jcDecorator = Nothing,
        jcJob = headP,
        jcContinuations = zipWith mk bools tailPipes
      }

genNonEmptyStmts :: Gen (NE.NonEmpty FishStatement)
genNonEmptyStmts = do
  n <- chooseInt (1, 3)
  xs <- vectorOf n genStatusCommand
  pure (NE.fromList (map Stmt xs))

-- Arbitrary instances for convenience ---------------------------------------

-- Arbitrary instances are intentionally omitted to avoid orphan warnings.

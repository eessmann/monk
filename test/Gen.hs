{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Gen
  ( genTextNoQuote
  , genExprStr
  , genStatusCommand
  , genPipeline
  , genConjunction
  , genNonEmptyStmts
  ) where

import Monk
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import Test.QuickCheck

-- Basic text generator avoiding single quotes to simplify pretty expectations
genTextNoQuote :: Gen Text
genTextNoQuote = do
  chars <- listOf (suchThat arbitrary (/= '\''))
  pure (T.pack (take 10 chars))

genExprStr :: Gen (FishExpr 'TStr)
genExprStr = ExprLiteral <$> genTextNoQuote

genStatusCommand :: Gen (FishCommand 'TStatus)
genStatusCommand = oneof
  [ pure (Command "true" [])
  , pure (Command "false" [])
  , do t <- genTextNoQuote
       pure (Command "echo" [ExprVal (ExprLiteral t)])
  ]

genPipeline :: Gen FishJobPipeline
genPipeline = do
  headCmd <- genStatusCommand
  k <- chooseInt (0, 3)
  contCmds <- vectorOf k genStatusCommand
  bg <- arbitrary
  let toCont c = PipeTo { jpcVariables = [], jpcStatement = Stmt c }
  pure FishJobPipeline
        { jpTime = False
        , jpVariables = []
        , jpStatement = Stmt headCmd
        , jpCont = map toCont contCmds
        , jpBackgrounded = bg
        }

genConjunction :: Gen FishJobConjunction
genConjunction = do
  headP <- genPipeline
  k <- chooseInt (0, 3)
  bools <- vectorOf k arbitrary
  tails <- vectorOf k genPipeline
  let mk b p = if b then JCAnd p else JCOr p
  semi <- arbitrary
  pure FishJobConjunction
        { jcDecorator = Nothing
        , jcJob = headP
        , jcContinuations = zipWith mk bools tails
        , jcSemiNl = semi
        }

genNonEmptyStmts :: Gen (NE.NonEmpty FishStatement)
genNonEmptyStmts = do
  n <- chooseInt (1, 3)
  xs <- vectorOf n genStatusCommand
  pure (NE.fromList (map Stmt xs))

-- Arbitrary instances for convenience ---------------------------------------

instance Arbitrary (FishExpr 'TStr) where
  arbitrary = genExprStr

instance Arbitrary (FishExpr 'TBool) where
  arbitrary = oneof
    [ pure (ExprBoolLiteral True)
    , pure (ExprBoolLiteral False)
    , pure (ExprBoolExpr (BoolCommand (Command "true" [])))
    ]

instance Arbitrary (FishCommand 'TStatus) where
  arbitrary = genStatusCommand

instance Arbitrary FishJobPipeline where
  arbitrary = genPipeline

instance Arbitrary FishJobConjunction where
  arbitrary = genConjunction


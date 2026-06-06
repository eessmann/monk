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
import Monk.AST
import Test.QuickCheck

-- Basic text generator avoiding single quotes to simplify pretty expectations
genTextNoQuote :: Gen Text
genTextNoQuote = do
  chars <- listOf (suchThat arbitrary (/= '\''))
  pure (T.pack (take 10 chars))

genExprStr :: Gen (Expr 'TStr)
genExprStr =
  oneof
    [ str <$> genTextNoQuote,
      pure (processSubst (NE.fromList [stmt (command "echo" [])]))
    ]

genStatusCommand :: Gen (Command 'ReturnsStatus)
genStatusCommand =
  oneof
    [ pure (command "true" []),
      pure (command "false" []),
      do
        t <- genTextNoQuote
        pure (command "echo" [arg (str t)]),
      pure (exit Nothing),
      do
        n <- chooseInt (0, 3)
        pure (exit (Just (int n))),
      eval . str <$> genTextNoQuote,
      source . str <$> genTextNoQuote,
      pure (exec (str "true") []),
      pure (read_ [ReadPrompt "Enter:", ReadLocal] ["x"])
    ]

genPipeline :: Gen (Command 'ReturnsStatus)
genPipeline = pipeline <$> genStages

genConjunction :: Gen JobConjunction
genConjunction = do
  headP <- genPipelineValue
  k <- chooseInt (0, 3)
  bools <- vectorOf k arbitrary
  tailPipes <- vectorOf k genPipelineValue
  let mk b p = if b then andThen p else orElse p
  pure (jobConjunction Nothing headP (zipWith mk bools tailPipes))

genNonEmptyStmts :: Gen (NE.NonEmpty Stmt)
genNonEmptyStmts = do
  n <- chooseInt (1, 3)
  xs <- vectorOf n genStatusCommand
  pure (NE.fromList (stmt <$> xs))

genStages :: Gen (NE.NonEmpty Stage)
genStages = do
  headCmd <- genStatusCommand
  k <- chooseInt (0, 3)
  contCmds <- vectorOf k genStatusCommand
  pure (NE.fromList (stage <$> (headCmd : contCmds)))

genPipelineValue :: Gen Pipeline
genPipelineValue = pipelineValue <$> genStages

-- Arbitrary instances for convenience ---------------------------------------

-- Arbitrary instances are intentionally omitted to avoid orphan warnings.

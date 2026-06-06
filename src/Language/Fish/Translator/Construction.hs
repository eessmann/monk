{-# LANGUAGE DataKinds #-}

module Language.Fish.Translator.Construction
  ( toRawExpr,
    toRawArg,
    toRawStatusCommand,
    toRawUnitCommand,
    toRawStmt,
    toRawBlock,
    toRawPipeline,
    toRawScript,
  )
where

import Language.Fish.DSL qualified as DSL
import Language.Fish.DSL.Internal qualified as DSLI
import Language.Fish.DSL.Lower qualified as Lower
import Language.Fish.Translator.Types

toRawExpr :: DSL.Expr t -> FishExpr t
toRawExpr = Lower.lowerExpr

toRawArg :: DSL.Arg -> ExprOrRedirect
toRawArg = Lower.lowerArg

toRawStatusCommand :: DSL.Command 'DSL.ReturnsStatus -> FishCommand TStatus
toRawStatusCommand = Lower.lowerCommand

toRawUnitCommand :: DSL.Command 'DSL.ReturnsUnit -> FishCommand TUnit
toRawUnitCommand = Lower.lowerCommand

toRawStmt :: DSL.Stmt -> FishStatement
toRawStmt = Lower.lowerStmt

toRawBlock :: DSL.Block -> NonEmpty FishStatement
toRawBlock = Lower.lowerBlock

toRawPipeline :: DSL.Pipeline -> FishJobPipeline
toRawPipeline = DSLI.lowerPipelineValue

toRawScript :: DSL.Script -> [FishStatement]
toRawScript = Lower.lowerScript

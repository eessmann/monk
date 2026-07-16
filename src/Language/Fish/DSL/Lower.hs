{-# LANGUAGE DataKinds #-}

-- | Private compatibility adapter for the pretty-printing backend.
--
-- The DSL is now the canonical structural Fish representation, so lowering is
-- intentionally an identity operation except for invariant wrappers such as
-- 'Block', 'Stage', and 'Script'.
module Language.Fish.DSL.Lower
  ( lowerExpr,
    lowerArg,
    lowerCommand,
    lowerStmt,
    lowerBlock,
    lowerStage,
    lowerPipeline,
    lowerPipelineWithTime,
    lowerScript,
  )
where

import Language.Fish.DSL.Internal

lowerExpr :: Expr t -> FishExpr t
lowerExpr = id

lowerArg :: Arg -> ExprOrRedirect
lowerArg = id

lowerCommand :: FishCommand t -> FishCommand t
lowerCommand = id

lowerStmt :: Stmt -> FishStatement
lowerStmt = id

lowerBlock :: Block -> NonEmpty FishStatement
lowerBlock (MkBlock body) = body

lowerStage :: Stage -> FishStatement
lowerStage (MkStage command) = Stmt command

lowerPipeline :: NonEmpty Stage -> FishJobPipeline
lowerPipeline = lowerPipelineWithTime False

lowerPipelineWithTime :: Bool -> NonEmpty Stage -> FishJobPipeline
lowerPipelineWithTime timed (MkStage headStage :| rest) =
  MkFishJobPipeline
    { jpTime = timed,
      jpVariables = [],
      jpStatement = Stmt headStage,
      jpCont = pipeContinuation <$> rest,
      jpBackgrounded = False
    }
  where
    pipeContinuation (MkStage next) =
      PipeTo
        { jpcVariables = [],
          jpcStatement = Stmt next
        }

lowerScript :: Script -> [FishStatement]
lowerScript (MkScript statements) = statements

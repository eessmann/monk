{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Pipeline
  ( pipelineOf,
    jobPipelineFromList,
    jobPipelineFromListWithTime,
    wrapErrexitIfEnabled,
    wrapErrexitStatusCommand,
    shouldWrapErrexit,
    applyPipefailIfEnabled,
  )
where

import Data.List.NonEmpty qualified as NE
import Language.Fish.AST
import Language.Fish.Translator.Monad
  ( TranslateM,
    TranslationContext (..),
    TranslateState (..),
    isErrexitEnabled,
    isPipefailEnabled,
  )
import Language.Fish.Translator.Pipefail (ensurePipefailHelper)
import Polysemy.State qualified as State

pipelineOf :: FishCommand TStatus -> FishJobPipeline
pipelineOf cmd =
  FishJobPipeline {jpTime = False, jpVariables = [], jpStatement = Stmt cmd, jpCont = [], jpBackgrounded = False}

jobPipelineFromList :: [FishCommand TStatus] -> FishJobPipeline
jobPipelineFromList = jobPipelineFromListWithTime False

jobPipelineFromListWithTime :: Bool -> [FishCommand TStatus] -> FishJobPipeline
jobPipelineFromListWithTime _ [] = pipelineOf (Command "true" [])
jobPipelineFromListWithTime timed (c : cs) =
  FishJobPipeline
    { jpTime = timed,
      jpVariables = [],
      jpStatement = Stmt c,
      jpCont = map (\cmd' -> PipeTo {jpcVariables = [], jpcStatement = Stmt cmd'}) cs,
      jpBackgrounded = False
    }

wrapErrexitIfEnabled :: FishCommand TStatus -> TranslateM (FishCommand TStatus)
wrapErrexitIfEnabled cmd = do
  enabled <- isErrexitEnabled
  inCmdSubst <- State.gets (inCommandSubst . context)
  if not enabled || inCmdSubst || not (shouldWrapErrexit cmd)
    then pure cmd
    else pure (wrapErrexitStatusCommand cmd)

wrapErrexitStatusCommand :: FishCommand TStatus -> FishCommand TStatus
wrapErrexitStatusCommand cmd
  | not (shouldWrapErrexit cmd) = cmd
  | otherwise =
      let cmdPipe = pipelineOf cmd
          guardPipe = pipelineOf errexitGuard
       in JobConj (FishJobConjunction Nothing cmdPipe [JCOr guardPipe])
  where
    errexitGuard =
      let statusVar = "__monk_errexit_status"
          saveStatus =
            Stmt
              ( Command
                  "set"
                  [ ExprVal (ExprLiteral "--local"),
                    ExprVal (ExprLiteral statusVar),
                    ExprVal (ExprSpecialVar SVStatus)
                  ]
              )
          checkPipe =
            pipelineOf
              ( Command
                  "status"
                  [ExprVal (ExprLiteral "is-command-substitution")]
              )
          exitPipe =
            pipelineOf
              ( Exit
                  (Just (ExprMath (ExprVariable (VarScalar statusVar) NE.:| [])))
              )
          checkOrExit =
            Stmt
              ( JobConj
                  (FishJobConjunction Nothing checkPipe [JCOr exitPipe])
              )
       in Begin (saveStatus NE.:| [checkOrExit]) []

shouldWrapErrexit :: FishCommand TStatus -> Bool
shouldWrapErrexit = \case
  JobConj {} -> False
  Not {} -> False
  Background {} -> False
  Exit {} -> False
  Return {} -> False
  _ -> True

applyPipefailIfEnabled :: FishCommand TStatus -> TranslateM (FishCommand TStatus)
applyPipefailIfEnabled cmd = do
  enabled <- isPipefailEnabled
  if not enabled
    then pure cmd
    else case cmd of
      Pipeline pipe | hasPipes pipe -> wrapPipefail cmd
      Not inner@(Pipeline pipe) | hasPipes pipe -> Not <$> wrapPipefail inner
      _ -> pure cmd
  where
    hasPipes pipe = not (null (jpCont pipe))
    wrapPipefail inner = do
      ensurePipefailHelper
      let helperCall =
            Stmt
              ( Command
                  "__monk_pipefail"
                  [ExprVal (ExprSpecialVar SVPipestatus)]
              )
      pure (Begin (Stmt inner NE.:| [helperCall]) [])

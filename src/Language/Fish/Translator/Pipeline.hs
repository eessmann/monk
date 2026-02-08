{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Pipeline
  ( pipelineOf,
    jobPipelineFromList,
    jobPipelineFromListWithTime,
    wrapErrexitIfEnabled,
    shouldWrapErrexit,
    applyPipefailIfEnabled,
  )
where

import Data.List.NonEmpty qualified as NE
import Language.Fish.AST
import Language.Fish.Translator.Monad
  ( TranslateM,
    isErrexitEnabled,
    isPipefailEnabled,
  )
import Language.Fish.Translator.Pipefail (ensurePipefailHelper)

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
  if not enabled || not (shouldWrapErrexit cmd)
    then pure cmd
    else
      let exitCmd = Exit (Just (ExprSpecialVar SVStatus))
          cmdPipe = pipelineOf cmd
          exitPipe = pipelineOf exitCmd
       in pure (JobConj (FishJobConjunction Nothing cmdPipe [JCOr exitPipe]))

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

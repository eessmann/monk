{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.IO
  ( translatePipeline
  , translateTokenToMaybeStatusCmd
  , translatePipelineToStatus
  , pipelineOf
  , jobPipelineFromList
  ) where

import Language.Fish.AST
import Language.Fish.Translator.Commands (translateCommandTokensToStatus, translateTokenToStatusCmd)
import ShellCheck.AST

--------------------------------------------------------------------------------
-- Pipelines and status commands
--------------------------------------------------------------------------------

pipelineOf :: FishCommand TStatus -> FishJobPipeline
pipelineOf cmd =
  FishJobPipeline { jpTime = False, jpVariables = [], jpStatement = Stmt cmd, jpCont = [], jpBackgrounded = False }

jobPipelineFromList :: [FishCommand TStatus] -> FishJobPipeline
jobPipelineFromList [] = pipelineOf (Command "true" [])
jobPipelineFromList (c:cs) =
  FishJobPipeline
    { jpTime = False
    , jpVariables = []
    , jpStatement = Stmt c
    , jpCont = map (\cmd' -> PipeTo { jpcVariables = [], jpcStatement = Stmt cmd' }) cs
    , jpBackgrounded = False
    }

translatePipeline :: [Token] -> [Token] -> FishStatement
translatePipeline bang cmds =
  case mapMaybe translateTokenToMaybeStatusCmd cmds of
    [] -> Stmt (Command "true" [])
    (c:cs) ->
      let pipe = Pipeline (jobPipelineFromList (c:cs))
       in if null bang then Stmt pipe else Stmt (Not pipe)

translateTokenToMaybeStatusCmd :: Token -> Maybe (FishCommand TStatus)
translateTokenToMaybeStatusCmd token =
  case token of
    T_SimpleCommand _ assignments rest -> Just (translateCommandTokensToStatus assignments rest)
    T_Condition {} -> Just (translateTokenToStatusCmd token)
    T_Redirecting _ _ inner -> translateTokenToMaybeStatusCmd inner
    T_Pipeline _ bang cmds -> Just (translatePipelineToStatus bang cmds)
    T_AndIf _ l r ->
      let lp = pipelineOf (translateTokenToStatusCmd l)
          rp = pipelineOf (translateTokenToStatusCmd r)
       in Just (JobConj (FishJobConjunction Nothing lp [JCAnd rp]))
    T_OrIf _ l r ->
      let lp = pipelineOf (translateTokenToStatusCmd l)
          rp = pipelineOf (translateTokenToStatusCmd r)
       in Just (JobConj (FishJobConjunction Nothing lp [JCOr rp]))
    _ -> Nothing

translatePipelineToStatus :: [Token] -> [Token] -> FishCommand TStatus
translatePipelineToStatus bang cmds =
  case mapMaybe translateTokenToMaybeStatusCmd cmds of
    [] -> Command "true" []
    (c:cs) -> let pipe = Pipeline (jobPipelineFromList (c:cs))
              in if null bang then pipe else Not pipe

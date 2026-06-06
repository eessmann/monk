{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.IO
  ( translatePipeline,
    translatePipelineM,
    translateTokenToMaybeStatusCmd,
    translatePipelineToStatus,
    pipelineOf,
    jobPipelineFromList,
  )
where

import Data.List.NonEmpty qualified as NE
import Language.Fish.Translator.Commands
  ( stripTimePrefix,
    translateCommandTokensToStatus,
    translateTokenToStatusCmd,
    translateTokenToStatusCmdM,
  )
import Language.Fish.Translator.Monad (TranslateM)
import Language.Fish.Translator.Pipeline
  ( applyPipefailIfEnabled,
    jobPipelineFromList,
    jobPipelineFromListWithTime,
    pipelineOf,
    wrapErrexitIfEnabled,
  )
import Language.Fish.Translator.Statement (statusConjunction)
import Language.Fish.Translator.Token (tokensHaveBang)
import Language.Fish.Translator.Types
import ShellCheck.AST

--------------------------------------------------------------------------------
-- Pipelines and status commands
--------------------------------------------------------------------------------

translatePipeline :: [Token] -> [Token] -> FishStatement
translatePipeline bang cmds =
  let (timed, cmds') = stripTimePrefix cmds
   in case map translateTokenToStatusCmd cmds' of
        [] -> Stmt (Command "true" [])
        (c : cs) ->
          let pipe = Pipeline (jobPipelineFromListWithTime timed (c NE.:| cs))
           in if tokensHaveBang bang then Stmt (Not pipe) else Stmt pipe

translatePipelineM :: [Token] -> [Token] -> TranslateM FishStatement
translatePipelineM bang cmds = do
  let (timed, cmds') = stripTimePrefix cmds
  cmds'' <- mapM translateTokenToStatusCmdM cmds'
  case cmds'' of
    [] -> pure (Stmt (Command "true" []))
    (c : cs) -> do
      let pipe = Pipeline (jobPipelineFromListWithTime timed (c NE.:| cs))
      pipe' <- applyPipefailIfEnabled pipe
      let cmd = if tokensHaveBang bang then Not pipe' else pipe'
      cmd' <- wrapErrexitIfEnabled cmd
      pure (Stmt cmd')

translateTokenToMaybeStatusCmd :: Token -> Maybe (FishCommand TStatus)
translateTokenToMaybeStatusCmd token =
  case token of
    T_SimpleCommand _ assignments rest -> Just (translateCommandTokensToStatus assignments rest)
    T_Condition {} -> Just (translateTokenToStatusCmd token)
    T_Redirecting _ _ inner -> translateTokenToMaybeStatusCmd inner
    T_Banged _ inner -> Just (Not (translateTokenToStatusCmd inner))
    T_Pipeline _ bang cmds -> Just (translatePipelineToStatus bang cmds)
    T_AndIf _ l r -> Just (statusConjunction ConjAnd (translateTokenToStatusCmd l) (translateTokenToStatusCmd r))
    T_OrIf _ l r -> Just (statusConjunction ConjOr (translateTokenToStatusCmd l) (translateTokenToStatusCmd r))
    _ -> Just (Command "false" [])

translatePipelineToStatus :: [Token] -> [Token] -> FishCommand TStatus
translatePipelineToStatus bang cmds =
  let (timed, cmds') = stripTimePrefix cmds
   in case map translateTokenToStatusCmd cmds' of
        [] -> Command "true" []
        (c : cs) ->
          let pipe = Pipeline (jobPipelineFromListWithTime timed (c NE.:| cs))
           in if tokensHaveBang bang then Not pipe else pipe

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Commands.CommandTokens.Status
  ( translateCommandTokensToStatus,
    translateTokensToStatusCmd,
    translatePipelineToStatus,
    translateTokenToMaybeStatusCmd,
    translateTokenToStatusCmd,
    translateTimeReserved,
  )
where

import Language.Fish.AST
import Language.Fish.Translator.Commands.CommandTokens.Core (translateCommandTokensWithoutTime)
import Language.Fish.Translator.Commands.Tests (translateConditionToken)
import Language.Fish.Translator.Hoist (beginIfNeeded)
import Language.Fish.Translator.Commands.Time (stripTimePrefix)
import Language.Fish.Translator.Pipeline (jobPipelineFromListWithTime, pipelineOf)
import Language.Fish.Translator.Token (tokenToLiteralText)
import Language.Fish.Translator.Variables
  ( translateAssignmentWithFlags,
    translateArithmetic,
    translateTokenToExprOrRedirect,
  )
import ShellCheck.AST

--------------------------------------------------------------------------------
-- Status command helpers (non-M)
--------------------------------------------------------------------------------

translateCommandTokensToStatus :: [Token] -> [Token] -> FishCommand TStatus
translateCommandTokensToStatus assignments cmdTokens = fromMaybe (Command "true" []) $ do
  fishCmd <- translateCommandTokensWithoutTime cmdTokens
  if null assignments
    then pure fishCmd
    else
      let envFlags = [SetLocal, SetExport]
          envAssigns = concatMap (translateAssignmentWithFlags envFlags) assignments
       in pure (beginIfNeeded envAssigns fishCmd)

translatePipelineToStatus :: [Token] -> [Token] -> FishCommand TStatus
translatePipelineToStatus bang cmds =
  let (timed, cmds') = stripTimePrefix cmds
   in case mapMaybe translateTokenToMaybeStatusCmd cmds' of
        [] -> Command "true" []
        (c : cs) ->
          let pipe = Pipeline (jobPipelineFromListWithTime timed (c : cs))
           in if hasBang bang then Not pipe else pipe

translateTokenToMaybeStatusCmd :: Token -> Maybe (FishCommand TStatus)
translateTokenToMaybeStatusCmd token =
  case token of
    T_SimpleCommand _ assignments rest -> Just (translateCommandTokensToStatus assignments rest)
    T_Condition _ _ condTok -> Just (translateConditionToken condTok)
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

translateTokensToStatusCmd :: [Token] -> FishCommand TStatus
translateTokensToStatusCmd tokens =
  case tokens of
    [] -> Command "true" []
    [T_Condition _ _ condTok] -> translateConditionToken condTok
    [T_Arithmetic _ exprTok] -> translateArithmetic exprTok
    [T_SimpleCommand _ a r] -> translateCommandTokensToStatus a r
    [T_Pipeline _ b c] -> translatePipelineToStatus b c
    [T_AndIf _ l r] ->
      let lp = pipelineOf (translateTokenToStatusCmd l)
          rp = pipelineOf (translateTokenToStatusCmd r)
       in JobConj (FishJobConjunction Nothing lp [JCAnd rp])
    [T_OrIf _ l r] ->
      let lp = pipelineOf (translateTokenToStatusCmd l)
          rp = pipelineOf (translateTokenToStatusCmd r)
       in JobConj (FishJobConjunction Nothing lp [JCOr rp])
    (c : args) -> Command (tokenToLiteralText c) (map translateTokenToExprOrRedirect args)

translateTokenToStatusCmd :: Token -> FishCommand TStatus
translateTokenToStatusCmd = translateTokensToStatusCmd . pure

--------------------------------------------------------------------------------
-- Time handling
--------------------------------------------------------------------------------

translateTimeReserved :: Text -> [Token] -> Maybe (FishCommand TStatus)
translateTimeReserved name args
  | name /= "time" = Nothing
  | otherwise = case args of
      [tok] -> translateTimedToken tok
      _ -> Nothing
  where
    translateTimedToken tok =
      case tok of
        T_Pipeline _ bang cmds -> Just (timedPipeline bang cmds)
        T_Redirecting _ _ inner -> translateTimedToken inner
        _ -> Nothing

    timedPipeline bang cmds =
      case mapMaybe translateTokenToMaybeStatusCmd cmds of
        [] -> Command "true" []
        (c : cs) ->
          let pipe = Pipeline (jobPipelineFromListWithTime True (c : cs))
           in if hasBang bang then Not pipe else pipe

hasBang :: [Token] -> Bool
hasBang = any (\tok -> tokenToLiteralText tok == "!")

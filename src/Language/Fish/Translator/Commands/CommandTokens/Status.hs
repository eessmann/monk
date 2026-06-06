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

import Data.List.NonEmpty qualified as NE
import Language.Fish.Translator.Args (renderArgs)
import Language.Fish.Translator.Commands.CommandTokens.Core (translateCommandTokensWithoutTime)
import Language.Fish.Translator.Commands.Tests (translateConditionToken)
import Language.Fish.Translator.Commands.Time (stripTimePrefix)
import Language.Fish.Translator.DSL
import Language.Fish.Translator.Hoist (beginIfNeeded)
import Language.Fish.Translator.Pipeline (jobPipelineFromListWithTime)
import Language.Fish.Translator.Redirections (translateRedirectToken)
import Language.Fish.Translator.Statement
  ( statusCommandBlock,
    statusConjunction,
  )
import Language.Fish.Translator.Token
  ( stripSeparatorTokens,
    tokenToLiteralText,
    tokensHaveBang,
  )
import Language.Fish.Translator.Variables
  ( translateArithmetic,
    translateAssignmentWithFlags,
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
          let pipe = Pipeline (jobPipelineFromListWithTime timed (c NE.:| cs))
           in if tokensHaveBang bang then Not pipe else pipe

translateTokenToMaybeStatusCmd :: Token -> Maybe (FishCommand TStatus)
translateTokenToMaybeStatusCmd token =
  case token of
    T_SimpleCommand _ assignments rest -> Just (translateCommandTokensToStatus assignments rest)
    T_Condition _ _ condTok -> Just (translateConditionToken condTok)
    T_BraceGroup _ tokens -> Just (translateStatusBlock tokens)
    T_Subshell _ tokens -> Just (translateSubshellStatus tokens)
    T_Redirecting _ redirs inner ->
      let cmd = translateTokenToStatusCmd inner
          redirExprs = renderArgs (mapMaybe translateRedirectToken redirs)
       in Just (attachRedirsToStatus redirExprs cmd)
    T_Pipeline _ bang cmds -> Just (translatePipelineToStatus bang cmds)
    T_AndIf _ l r -> Just (statusConjunction ConjAnd (translateTokenToStatusCmd l) (translateTokenToStatusCmd r))
    T_OrIf _ l r -> Just (statusConjunction ConjOr (translateTokenToStatusCmd l) (translateTokenToStatusCmd r))
    _ -> Nothing

translateTokensToStatusCmd :: [Token] -> FishCommand TStatus
translateTokensToStatusCmd tokens =
  case tokens of
    [] -> Command "true" []
    [T_Condition _ _ condTok] -> translateConditionToken condTok
    [T_Arithmetic _ exprTok] -> translateArithmetic exprTok
    [T_SimpleCommand _ a r] -> translateCommandTokensToStatus a r
    [T_Pipeline _ b c] -> translatePipelineToStatus b c
    [T_BraceGroup _ innerTokens] -> translateStatusBlock innerTokens
    [T_Subshell _ innerTokens] -> translateSubshellStatus innerTokens
    [T_AndIf _ l r] -> statusConjunction ConjAnd (translateTokenToStatusCmd l) (translateTokenToStatusCmd r)
    [T_OrIf _ l r] -> statusConjunction ConjOr (translateTokenToStatusCmd l) (translateTokenToStatusCmd r)
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
          let pipe = Pipeline (jobPipelineFromListWithTime True (c NE.:| cs))
           in if tokensHaveBang bang then Not pipe else pipe

translateStatusBlock :: [Token] -> FishCommand TStatus
translateStatusBlock tokens =
  statusCommandBlock
    (mapMaybe translateTokenToMaybeStatusCmd (stripSeparatorTokens tokens))

translateSubshellStatus :: [Token] -> FishCommand TStatus
translateSubshellStatus tokens =
  statusCommandBlock
    (mapMaybe translateTokenToMaybeStatusCmd (stripSeparatorTokens tokens))

attachRedirsToStatus :: [ExprOrRedirect] -> FishCommand TStatus -> FishCommand TStatus
attachRedirsToStatus = attachRedirectsToCommand

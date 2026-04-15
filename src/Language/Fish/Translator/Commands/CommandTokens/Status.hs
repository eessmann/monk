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

import Data.List.NonEmpty qualified as NE
import Language.Fish.AST
import Language.Fish.Pretty (renderFish)
import Language.Fish.Translator.Args (renderArgs)
import Language.Fish.Translator.Commands.CommandTokens.Core (translateCommandTokensWithoutTime)
import Language.Fish.Translator.Commands.Tests (translateConditionToken)
import Language.Fish.Translator.Hoist (beginIfNeeded)
import Language.Fish.Translator.Commands.Time (stripTimePrefix)
import Language.Fish.Translator.Pipeline (jobPipelineFromListWithTime, pipelineOf)
import Language.Fish.Translator.Redirections (translateRedirectToken)
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
    T_BraceGroup _ tokens -> Just (translateStatusBlock tokens)
    T_Subshell _ tokens -> Just (translateSubshellStatus tokens)
    T_Redirecting _ redirs inner ->
      let cmd = translateTokenToStatusCmd inner
          redirExprs = renderArgs (catMaybes (map translateRedirectToken redirs))
       in Just (attachRedirsToStatus redirExprs cmd)
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
    [T_BraceGroup _ innerTokens] -> translateStatusBlock innerTokens
    [T_Subshell _ innerTokens] -> translateSubshellStatus innerTokens
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

translateStatusBlock :: [Token] -> FishCommand TStatus
translateStatusBlock tokens =
  case mapMaybe translateTokenToMaybeStatusCmd (filter (not . isSeparatorToken) tokens) of
    [] -> Command "true" []
    (cmd : rest) -> Begin (Stmt cmd NE.:| map Stmt rest) []

translateSubshellStatus :: [Token] -> FishCommand TStatus
translateSubshellStatus tokens =
  let script =
        case mapMaybe translateTokenToMaybeStatusCmd (filter (not . isSeparatorToken) tokens) of
          [] -> "true"
          cmds -> renderFish (map Stmt cmds)
   in Command
        "fish"
        [ ExprVal (ExprLiteral "--no-config"),
          ExprVal (ExprLiteral "-c"),
          ExprVal (ExprLiteral script)
        ]

attachRedirsToStatus :: [ExprOrRedirect] -> FishCommand TStatus -> FishCommand TStatus
attachRedirsToStatus redirs cmd =
  case cmd of
    Command name args -> Command name (args ++ redirs)
    Exec c args -> Exec c (args ++ redirs)
    Begin body suffix -> Begin body (suffix ++ redirs)
    If cond thn els suffix -> If cond thn els (suffix ++ redirs)
    Switch expr cases suffix -> Switch expr cases (suffix ++ redirs)
    While cond body suffix -> While cond body (suffix ++ redirs)
    For var listExpr body suffix -> For var listExpr body (suffix ++ redirs)
    other ->
      case redirs of
        [] -> other
        _ -> Begin (Stmt other NE.:| []) redirs

isSeparatorToken :: Token -> Bool
isSeparatorToken tok =
  let txt = tokenToLiteralText tok
   in txt == ";" || txt == "\n"

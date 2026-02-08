{-# LANGUAGE LambdaCase #-}

module Language.Fish.Translator.Commands.Status
  ( translateTokensToStatusCmd,
    translateTokenToStatusCmd,
    translateTokensToStatusCmdM,
    translateTokenToStatusCmdM,
    translatePipelineToStatusM,
    translateTokenToMaybeStatusCmdM,
    stmtToStatusCommand,
  )
where

import Prelude hiding (gets)
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Language.Fish.AST
import Language.Fish.Translator.Commands.CommandTokens (translateTokensToStatusCmd)
import Language.Fish.Translator.Commands.Tests (translateConditionTokenM)
import Language.Fish.Translator.Commands.Time (stripTimePrefix)
import Language.Fish.Translator.Commands.SimpleCommand (translateSimpleCommandMWith)
import Language.Fish.Translator.Hoist (Hoisted (..), beginIfNeeded)
import Language.Fish.Translator.Monad (TranslateM, TranslationContext (..), context)
import Language.Fish.Translator.Pipeline
  ( applyPipefailIfEnabled,
    jobPipelineFromListWithTime,
    pipelineOf,
  )
import Language.Fish.Translator.Statement (toNonEmptyStmtList)
import Language.Fish.Translator.Token (tokenToLiteralText)
import Language.Fish.Translator.Variables (translateArithmeticStatusM)
import Polysemy.State (gets)
import ShellCheck.AST

translateTokenToStatusCmd :: Token -> FishCommand TStatus
translateTokenToStatusCmd = translateTokensToStatusCmd . pure

translateTokensToStatusCmdM :: [Token] -> TranslateM (FishCommand TStatus)
translateTokensToStatusCmdM tokens =
  case tokens of
    [] -> pure (Command "true" [])
    [tok] -> translateTokenToStatusCmdM tok
    _ -> do
      let toks = filter (not . isSeparatorToken) tokens
      cmds <- mapM translateTokenToStatusCmdM toks
      case NE.nonEmpty (map Stmt cmds) of
        Just body -> pure (Begin body [])
        Nothing -> pure (Command "true" [])

translateTokenToStatusCmdM :: Token -> TranslateM (FishCommand TStatus)
translateTokenToStatusCmdM tok =
  case tok of
    T_SimpleCommand _ assignments cmdToks -> do
      locals <- gets (localVars . context)
      inFunc <- gets (inFunction . context)
      let localFlag = if inFunc then SetFunction else SetLocal
      let scopeFlags name =
            if Set.member name locals
              then [localFlag]
              else [SetGlobal]
      stmt <- translateSimpleCommandMWith False scopeFlags assignments cmdToks
      pure (stmtToStatusCommand stmt)
    T_Pipeline _ bang cmds ->
      translatePipelineToStatusM bang cmds
    T_Condition _ _ condTok ->
      do
        Hoisted pre cmd <- translateConditionTokenM condTok
        pure (beginIfNeeded pre cmd)
    T_Redirecting _ _ inner ->
      translateTokenToStatusCmdM inner
    T_Arithmetic _ exprTok ->
      translateArithmeticStatusM exprTok
    T_AndIf _ l r -> do
      lp <- pipelineOf <$> translateTokenToStatusCmdM l
      rp <- pipelineOf <$> translateTokenToStatusCmdM r
      pure (JobConj (FishJobConjunction Nothing lp [JCAnd rp]))
    T_OrIf _ l r -> do
      lp <- pipelineOf <$> translateTokenToStatusCmdM l
      rp <- pipelineOf <$> translateTokenToStatusCmdM r
      pure (JobConj (FishJobConjunction Nothing lp [JCOr rp]))
    _ ->
      pure (Command "true" [])

translatePipelineToStatusM :: [Token] -> [Token] -> TranslateM (FishCommand TStatus)
translatePipelineToStatusM bang cmds = do
  let (timed, cmds') = stripTimePrefix cmds
  mCmds <- mapM translateTokenToMaybeStatusCmdM cmds'
  case catMaybes mCmds of
    [] -> pure (Command "true" [])
    (c : cs) -> do
      let pipe = Pipeline (jobPipelineFromListWithTime timed (c : cs))
      pipe' <- applyPipefailIfEnabled pipe
      pure (if hasBang bang then Not pipe' else pipe')

translateTokenToMaybeStatusCmdM :: Token -> TranslateM (Maybe (FishCommand TStatus))
translateTokenToMaybeStatusCmdM token =
  case token of
    T_SimpleCommand {} -> do
      cmd <- translateTokenToStatusCmdM token
      pure (Just cmd)
    T_Condition {} -> Just <$> translateTokenToStatusCmdM token
    T_Redirecting _ _ inner -> translateTokenToMaybeStatusCmdM inner
    T_Pipeline _ bang cmds -> Just <$> translatePipelineToStatusM bang cmds
    T_AndIf _ l r -> do
      lp <- pipelineOf <$> translateTokenToStatusCmdM l
      rp <- pipelineOf <$> translateTokenToStatusCmdM r
      pure (Just (JobConj (FishJobConjunction Nothing lp [JCAnd rp])))
    T_OrIf _ l r -> do
      lp <- pipelineOf <$> translateTokenToStatusCmdM l
      rp <- pipelineOf <$> translateTokenToStatusCmdM r
      pure (Just (JobConj (FishJobConjunction Nothing lp [JCOr rp])))
    _ -> pure Nothing

stmtToStatusCommand :: FishStatement -> FishCommand TStatus
stmtToStatusCommand stmt =
  case stmt of
    Stmt cmd ->
      case commandToStatus cmd of
        Just statusCmd -> statusCmd
        Nothing -> Begin (stmt NE.:| []) []
    StmtList stmts ->
      case toNonEmptyStmtList stmts of
        Just body -> Begin body []
        Nothing -> Command "true" []
    Comment _ -> Command "true" []
    EmptyStmt -> Command "true" []

commandToStatus :: FishCommand t -> Maybe (FishCommand TStatus)
commandToStatus cmd =
  case cmd of
    Command {} -> Just cmd
    For {} -> Just cmd
    While {} -> Just cmd
    Begin {} -> Just cmd
    If {} -> Just cmd
    Switch {} -> Just cmd
    Exit {} -> Just cmd
    Source {} -> Just cmd
    Eval {} -> Just cmd
    Read {} -> Just cmd
    Pipeline {} -> Just cmd
    JobConj {} -> Just cmd
    Semicolon _ cmd2 -> commandToStatus cmd2
    Not {} -> Just cmd
    Background {} -> Just cmd
    Wait {} -> Just cmd
    Exec {} -> Just cmd
    Decorated dec inner ->
      case commandToStatus inner of
        Just innerStatus -> Just (Decorated dec innerStatus)
        Nothing -> Nothing
    Return {} -> Just cmd
    _ -> Nothing

isSeparatorToken :: Token -> Bool
isSeparatorToken tok =
  let txt = tokenToLiteralText tok
   in txt == ";" || txt == "\n"

hasBang :: [Token] -> Bool
hasBang = any (\tok -> tokenToLiteralText tok == "!")

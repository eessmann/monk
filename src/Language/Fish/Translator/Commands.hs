{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Commands
  ( translateSimpleCommand
  , translateCommandTokens
  , translateExit
  , translateSource
  , translateEval
  , translateExec
  , translateRead
  , translateTokensToStatusCmd
  , translateTokenToStatusCmd
  , translateCommandTokensToStatus
  , isSingleBracketTest
  , toNonEmptyStmtList
  , concatWithSpaces
  ) where

import Relude
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Language.Fish.AST
import Text.Read (readMaybe)
import Language.Fish.Translator.Variables
import Language.Fish.Translator.Redirections (parseRedirectTokens)
import ShellCheck.AST

--------------------------------------------------------------------------------
-- Small helpers
--------------------------------------------------------------------------------

toNonEmptyStmtList :: [FishStatement] -> Maybe (NonEmpty FishStatement)
toNonEmptyStmtList stmts = NE.nonEmpty (filter (/= SemiNl) stmts)

--------------------------------------------------------------------------------
-- Simple commands & Assignments
--------------------------------------------------------------------------------

translateSimpleCommand :: [Token] -> [Token] -> FishStatement
translateSimpleCommand assignments cmdTokens =
  let fishAssignments = mapMaybe translateAssignment assignments
   in case (cmdTokens, NE.nonEmpty fishAssignments) of
        -- Handle a brace group with possible trailing redirections: { ...; } > file
        (T_BraceGroup _ inner : rest, _) ->
          case toNonEmptyStmtList (map translateToken inner) of
            Just neBody ->
              let (reds, _unparsed) = parseRedirectTokens rest
              in BraceStmt neBody reds
            Nothing -> Comment "Skipped empty brace group in simple command"
        _ ->
          let fishCmd = translateCommandTokens cmdTokens in
          case (fishCmd, NE.nonEmpty fishAssignments) of
            (Just fCmd, Just neAssigns) ->
              case toNonEmptyStmtList (NE.toList neAssigns ++ [Stmt fCmd]) of
                Just body -> Stmt $ Begin body
                Nothing   -> Comment "Empty command with assignments"
            (Nothing, Just neAssigns) -> StmtList (NE.toList neAssigns)
            (Just fCmd, Nothing) -> Stmt fCmd
            (Nothing, Nothing) -> SemiNl

--------------------------------------------------------------------------------
-- Command translation & builtins
--------------------------------------------------------------------------------

translateCommandTokens :: [Token] -> Maybe (FishCommand TStatus)
translateCommandTokens cmdTokens =
  case cmdTokens of
    [] -> Nothing
    (c : args) ->
      let name = tokenToLiteralText c in
      Just $ case isSingleBracketTest c args of
        Just testCmd -> testCmd
        Nothing -> case T.unpack name of
          "exit" -> translateExit args
          "source" -> translateSource args
          "." -> translateSource args
          "eval" -> translateEval args
          "exec" -> translateExec args
          "read" -> translateRead args
          _ -> Command name (map translateTokenToExprOrRedirect args)

-- exit [n]
translateExit :: [Token] -> FishCommand TStatus
translateExit [] = Exit Nothing
translateExit [t] =
  case tokenToLiteralText t of
    txt | T.all isDigit txt && not (T.null txt)
        , Just n <- readMaybe (T.unpack txt) -> Exit (Just (ExprNumLiteral n))
        | otherwise -> Exit Nothing
  where
    isDigit = (\c -> c >= '0' && c <= '9')
translateExit ts = Command "exit" (map translateTokenToExprOrRedirect ts)

-- source FILE
translateSource :: [Token] -> FishCommand TStatus
translateSource [] = Command "source" []
translateSource (t:_) = Source (translateTokenToExpr t)

-- eval STRING (join args by space)
translateEval :: [Token] -> FishCommand TStatus
translateEval ts = Eval (concatWithSpaces (map translateTokenToExpr ts))

-- exec CMD [ARGS|REDIRS]
translateExec :: [Token] -> FishCommand TStatus
translateExec [] = Command "exec" []
translateExec (t:rest) = Exec (translateTokenToExpr t) (map translateTokenToExprOrRedirect rest)

-- read [flags] VARS...
translateRead :: [Token] -> FishCommand TStatus
translateRead ts =
  let (flags, vars) = parseReadArgs ts [] []
   in Read flags vars

parseReadArgs :: [Token] -> [ReadFlag] -> [Text] -> ([ReadFlag], [Text])
parseReadArgs [] fs vs = (fs, vs)
parseReadArgs (x:xs) fs vs =
  case tokenToLiteralText x of
    "-p" -> case xs of
              (p:rest) -> parseReadArgs rest (fs ++ [ReadPrompt (tokenToLiteralText p)]) vs
              []       -> parseReadArgs xs fs vs
    "--prompt" -> case xs of
              (p:rest) -> parseReadArgs rest (fs ++ [ReadPrompt (tokenToLiteralText p)]) vs
              []       -> parseReadArgs xs fs vs
    "--local" -> parseReadArgs xs (fs ++ [ReadLocal]) vs
    "--global" -> parseReadArgs xs (fs ++ [ReadGlobal]) vs
    "--universal" -> parseReadArgs xs (fs ++ [ReadUniversal]) vs
    "--export" -> parseReadArgs xs (fs ++ [ReadExport]) vs
    "--array" -> parseReadArgs xs (fs ++ [ReadArray]) vs
    tok ->
      if T.isPrefixOf "-" tok
        then parseReadArgs xs fs vs -- unknown flag: ignore
        else parseReadArgs xs fs (vs ++ [tok])

concatWithSpaces :: [FishExpr TStr] -> FishExpr TStr
concatWithSpaces [] = ExprLiteral ""
concatWithSpaces [e] = e
concatWithSpaces (e:es) = foldl' (\acc x -> ExprStringConcat (ExprStringConcat acc (ExprLiteral " ")) x) e es

isSingleBracketTest :: Token -> [Token] -> Maybe (FishCommand TStatus)
isSingleBracketTest bracketToken args =
  let bracketTxt = tokenToLiteralText bracketToken
   in if bracketTxt == "[" then
        case NE.nonEmpty args of
          Just neArgs ->
            let lastToken = NE.last neArgs
                lastText = tokenToLiteralText lastToken
                middle = NE.init neArgs
             in if lastText == "]"
                  then Just (Command "test" (map translateTokenToExprOrRedirect middle))
                  else Nothing
          Nothing -> Nothing
      else Nothing

--------------------------------------------------------------------------------
-- Status command helpers (used by control/IO)
--------------------------------------------------------------------------------

translateTokensToStatusCmd :: [Token] -> FishCommand TStatus
translateTokensToStatusCmd tokens =
  case tokens of
    [] -> Command "true" []
    [T_SimpleCommand _ a r] -> translateCommandTokensToStatus a r
    [T_Pipeline _ b c] -> translatePipelineToStatus b c
    [T_AndIf _ l r] ->
      let lp = pipelineOf (translateTokenToStatusCmd l)
          rp = pipelineOf (translateTokenToStatusCmd r)
       in JobConj (FishJobConjunction Nothing lp [JCAnd rp] False)
    [T_OrIf _ l r] ->
      let lp = pipelineOf (translateTokenToStatusCmd l)
          rp = pipelineOf (translateTokenToStatusCmd r)
       in JobConj (FishJobConjunction Nothing lp [JCOr rp] False)
    (c:args) -> Command (tokenToLiteralText c) (map translateTokenToExprOrRedirect args)

translateTokenToStatusCmd :: Token -> FishCommand TStatus
translateTokenToStatusCmd = translateTokensToStatusCmd . pure

translateCommandTokensToStatus :: [Token] -> [Token] -> FishCommand TStatus
translateCommandTokensToStatus assignments cmdTokens = fromMaybe (Command "true" []) $ do
  fishCmd <- translateCommandTokens cmdTokens
  if null assignments
    then pure fishCmd
    else pure fishCmd -- TODO: handle env-var scoping; simplified for now

-- Placeholder to break the import cycle; IO will override via qualified import
translatePipelineToStatus :: [Token] -> [Token] -> FishCommand TStatus
translatePipelineToStatus _ _ = Command "true" []

pipelineOf :: FishCommand TStatus -> FishJobPipeline
pipelineOf cmd =
  FishJobPipeline { jpTime = False, jpVariables = [], jpStatement = Stmt cmd, jpCont = [], jpBackgrounded = False }

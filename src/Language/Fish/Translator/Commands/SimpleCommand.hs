module Language.Fish.Translator.Commands.SimpleCommand
  ( translateSimpleCommand,
    translateSimpleCommandM,
    translateSimpleCommandMWith,
  )
where

import Prelude hiding (gets)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Language.Fish.AST
import Language.Fish.Translator.Commands.CommandTokens
  ( translateCommandTokens,
    translateCommandTokensM,
  )
import Language.Fish.Translator.Hoist (Hoisted (..), beginIfNeeded)
import Language.Fish.Translator.Names (isValidVarName)
import Language.Fish.Translator.Pipeline (wrapErrexitIfEnabled)
import Language.Fish.Translator.Statement (isEmptyStatement, toNonEmptyStmtList)
import Language.Fish.Translator.Token (tokenToLiteralText)
import Language.Fish.Translator.Variables (translateAssignmentWithFlags, translateAssignmentWithFlagsM)
import Language.Fish.Translator.Monad (TranslateM)
import ShellCheck.AST

--------------------------------------------------------------------------------
-- Simple commands & Assignments
--------------------------------------------------------------------------------

translateSimpleCommand :: (Text -> [SetFlag]) -> [Token] -> [Token] -> FishStatement
translateSimpleCommand scopeFlags assignments cmdTokens =
  let fishAssignments = concatMap (translateScopedAssignment scopeFlags) assignments
      cmdIsAssignment = not (null cmdTokens) && all isAssignmentWord cmdTokens
      hasCmd = not (null cmdTokens) && not cmdIsAssignment
   in case cmdTokens of
        [] -> assignOnly fishAssignments
        (c : _)
          | T.null (tokenToLiteralText c) || cmdIsAssignment -> assignOnly fishAssignments
        _
          | hasCmd && not (null assignments) ->
              translateEnvPrefix assignments cmdTokens
          | otherwise ->
              let fishCmd = translateCommandTokens cmdTokens
               in case (fishCmd, NE.nonEmpty fishAssignments) of
                    (Just fCmd, Just neAssigns) ->
                      Stmt (beginIfNeeded (NE.toList neAssigns) fCmd)
                    (Nothing, Just neAssigns) -> StmtList (NE.toList neAssigns)
                    (Just fCmd, Nothing) -> Stmt fCmd
                    (Nothing, Nothing) -> EmptyStmt
  where
    assignOnly assigns =
      case NE.nonEmpty assigns of
        Just neAssigns -> StmtList (NE.toList neAssigns)
        Nothing -> EmptyStmt

    translateScopedAssignment :: (Text -> [SetFlag]) -> Token -> [FishStatement]
    translateScopedAssignment scope tok =
      case tok of
        T_Assignment _ _ var _ _ ->
          translateAssignmentWithFlags (scope (toText var)) tok
        _ -> translateAssignmentWithFlags (scope "") tok

    translateEnvPrefix :: [Token] -> [Token] -> FishStatement
    translateEnvPrefix assigns tokens =
      let envFlags = [SetLocal, SetExport]
          envAssigns = concatMap (translateAssignmentWithFlags envFlags) assigns
          fishCmd = translateCommandTokens tokens
       in case (fishCmd, NE.nonEmpty envAssigns) of
            (Just fCmd, Just neAssigns) ->
              Stmt (beginIfNeeded (NE.toList neAssigns) fCmd)
            (Just fCmd, Nothing) -> Stmt fCmd
            (Nothing, Just neAssigns) -> StmtList (NE.toList neAssigns)
            (Nothing, Nothing) -> EmptyStmt

translateSimpleCommandM :: (Text -> [SetFlag]) -> [Token] -> [Token] -> TranslateM FishStatement
translateSimpleCommandM = translateSimpleCommandMWith True

translateSimpleCommandMWith :: Bool -> (Text -> [SetFlag]) -> [Token] -> [Token] -> TranslateM FishStatement
translateSimpleCommandMWith allowErrexit scopeFlags assignments cmdTokens = do
  fishAssignments <- fmap concat (mapM (translateScopedAssignmentM scopeFlags) assignments)
  let cmdIsAssignment = not (null cmdTokens) && all isAssignmentWord cmdTokens
      hasCmd = not (null cmdTokens) && not cmdIsAssignment
  case cmdTokens of
    [] -> pure (assignOnly fishAssignments)
    (c : _)
      | T.null (tokenToLiteralText c) || cmdIsAssignment -> pure (assignOnly fishAssignments)
    _
      | hasCmd && not (null assignments) ->
          translateEnvPrefixM allowErrexit assignments cmdTokens
      | otherwise -> do
          Hoisted preArgs fishCmd <- translateCommandTokensM cmdTokens
          fishCmd' <-
            case fishCmd of
              Just cmd
                | allowErrexit -> Just <$> wrapErrexitIfEnabled cmd
              _ -> pure fishCmd
          let prelude = filter (not . isEmptyStatement) (fishAssignments <> preArgs)
          pure $
            case fishCmd' of
              Just cmd -> Stmt (beginIfNeeded prelude cmd)
              Nothing ->
                case toNonEmptyStmtList prelude of
                  Just body -> Stmt (Begin body [])
                  Nothing -> Comment "Empty command with assignments"
  where
    assignOnly assigns =
      case NE.nonEmpty assigns of
        Just neAssigns -> StmtList (NE.toList neAssigns)
        Nothing -> EmptyStmt

    translateScopedAssignmentM :: (Text -> [SetFlag]) -> Token -> TranslateM [FishStatement]
    translateScopedAssignmentM scope tok =
      case tok of
        T_Assignment _ _ var _ _ ->
          translateAssignmentWithFlagsM (scope (toText var)) tok
        _ -> translateAssignmentWithFlagsM (scope "") tok

    translateEnvPrefixM :: Bool -> [Token] -> [Token] -> TranslateM FishStatement
    translateEnvPrefixM allowErrexit' assigns tokens = do
      let envFlags = [SetLocal, SetExport]
          envAssigns = concatMap (translateAssignmentWithFlags envFlags) assigns
      Hoisted preArgs fishCmd <- translateCommandTokensM tokens
      fishCmd' <-
        case fishCmd of
          Just cmd
            | allowErrexit' -> Just <$> wrapErrexitIfEnabled cmd
          _ -> pure fishCmd
      let prelude = filter (not . isEmptyStatement) (preArgs <> envAssigns)
      pure $
        case fishCmd' of
          Just cmd -> Stmt (beginIfNeeded prelude cmd)
          Nothing ->
            case toNonEmptyStmtList prelude of
              Just body -> Stmt (Begin body [])
              Nothing -> Comment "Empty command with env assignments"

isAssignmentWord :: Token -> Bool
isAssignmentWord tok =
  case tok of
    T_Assignment {} -> True
    _ -> looksLikeAssignment (tokenToLiteralText tok)

looksLikeAssignment :: Text -> Bool
looksLikeAssignment txt =
  case T.breakOn "=" txt of
    (_, rhs) | T.null rhs -> False
    (lhs, _) ->
      let base = T.takeWhile (/= '[') lhs
       in isValidVarName base

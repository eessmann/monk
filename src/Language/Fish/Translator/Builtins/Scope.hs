{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Builtins.Scope
  ( translateLocalCommand,
    translateExportCommand,
    translateScopedAssignment,
  )
where

import Prelude hiding (gets)
import Data.Set qualified as Set
import Language.Fish.AST
import Language.Fish.Translator.Builtins.Common (parseAssignmentLiteral, wrapStmtList)
import Language.Fish.Translator.Monad
  ( TranslateM,
    TranslationContext (..),
    addLocalVars,
    addWarning,
    context,
  )
import Language.Fish.Translator.Names (isValidVarName)
import Language.Fish.Translator.Variables
  ( translateAssignmentWithFlags,
    translateAssignmentWithFlagsM,
    tokenToLiteralText,
  )
import Polysemy.State (gets)
import ShellCheck.AST

translateLocalCommand :: [Token] -> TranslateM FishStatement
translateLocalCommand args = do
  inFunc <- gets (inFunction . context)
  unless inFunc $
    addWarning "local used outside a function; fish will treat it as local to the current scope"
  let localFlag = if inFunc then SetFunction else SetLocal
  parsed <- mapM (parseLocalArg localFlag) args
  let names = map fst (catMaybes parsed)
      stmts = concatMap snd (catMaybes parsed)
  addLocalVars names
  pure $
    if null stmts
      then Comment "Skipped local with no assignments"
      else wrapStmtList stmts

translateExportCommand :: [Token] -> TranslateM FishStatement
translateExportCommand args = do
  inFunc <- gets (inFunction . context)
  locals <- gets (localVars . context)
  parsed <- mapM (parseExportArg inFunc locals) args
  let stmts = concat parsed
  pure $
    if null stmts
      then Comment "Skipped export with no assignments"
      else wrapStmtList stmts

translateScopedAssignment :: Set.Set Text -> Bool -> Token -> [FishStatement]
translateScopedAssignment locals inFunc tok =
  case tok of
    T_Assignment _ _ var _ _ ->
      translateAssignmentWithFlags (scopeFlagsFor locals inFunc (toText var)) tok
    _ ->
      translateAssignmentWithFlags (scopeFlagsFor locals inFunc "") tok

scopeFlagsFor :: Set.Set Text -> Bool -> Text -> [SetFlag]
scopeFlagsFor locals inFunc name =
  if Set.member name locals
    then [if inFunc then SetFunction else SetLocal]
    else [SetGlobal]

parseLocalArg :: SetFlag -> Token -> TranslateM (Maybe (Text, [FishStatement]))
parseLocalArg localFlag tok =
  case tok of
    T_Assignment _ _ var _ _ -> do
      let name = toText var
      stmts <- translateAssignmentWithFlagsM [localFlag] tok
      pure (Just (name, stmts))
    _ -> do
      let txt = tokenToLiteralText tok
      case parseAssignmentLiteral txt of
        Just (name, valueTxt) ->
          let expr = ExprListLiteral [ExprLiteral valueTxt]
           in pure (Just (name, [Stmt (Set [localFlag] name expr)]))
        Nothing ->
          if isValidVarName txt
            then pure (Just (txt, [Stmt (Set [localFlag] txt (ExprListLiteral []))]))
            else do
              addWarning ("Unsupported local argument: " <> txt)
              pure Nothing

parseExportArg :: Bool -> Set.Set Text -> Token -> TranslateM [FishStatement]
parseExportArg inFunc locals tok =
  case tok of
    T_Assignment _ _ var _ _ -> do
      let name = toText var
          flags = exportFlags inFunc locals name
      translateAssignmentWithFlagsM flags tok
    _ -> do
      let txt = tokenToLiteralText tok
      case parseAssignmentLiteral txt of
        Just (name, valueTxt) ->
          let expr = ExprListLiteral [ExprLiteral valueTxt]
           in pure [Stmt (Set (exportFlags inFunc locals name) name expr)]
        Nothing ->
          if isValidVarName txt
            then pure [Stmt (Set (exportFlags inFunc locals txt) txt (ExprVariable (VarAll txt)))]
            else do
              addWarning ("Unsupported export argument: " <> txt)
              pure []

exportFlags :: Bool -> Set.Set Text -> Text -> [SetFlag]
exportFlags inFunc locals name =
  if Set.member name locals
    then [if inFunc then SetFunction else SetLocal, SetExport]
    else [SetGlobal, SetExport]

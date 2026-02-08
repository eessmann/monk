{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Builtins.Declare
  ( translateDeclareCommand,
    translateReadonlyCommand,
  )
where

import Prelude hiding (gets)
import Control.Monad (foldM)
import Data.Text qualified as T
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
  ( translateAssignmentWithFlagsM,
    tokenToLiteralText,
  )
import Polysemy.State (gets)
import ShellCheck.AST

data DeclareFlags = DeclareFlags
  { declareGlobal :: Bool,
    declareExport :: Bool,
    declareReadonly :: Bool
  }
  deriving stock (Show, Eq)

defaultDeclareFlags :: DeclareFlags
defaultDeclareFlags = DeclareFlags False False False

translateDeclareCommand :: [Token] -> TranslateM FishStatement
translateDeclareCommand =
  translateDeclareCommandWith defaultDeclareFlags

translateReadonlyCommand :: [Token] -> TranslateM FishStatement
translateReadonlyCommand =
  translateDeclareCommandWith (defaultDeclareFlags {declareReadonly = True})

translateDeclareCommandWith :: DeclareFlags -> [Token] -> TranslateM FishStatement
translateDeclareCommandWith baseFlags args = do
  inFunc <- gets (inFunction . context)
  (flags, rest) <- parseDeclareFlags baseFlags args
  when (declareReadonly flags) $
    addWarning "readonly/declare -r has no direct fish equivalent; emitted set without enforcing readonly"
  let scopeFlags = declareScopeFlags inFunc flags
  if null rest
    then pure (Stmt (Command "set" []))
    else do
      parsed <- mapM (parseDeclareArg scopeFlags) rest
      let names = mapMaybe fst parsed
          stmts = concatMap snd parsed
      when (any (`elem` [SetLocal, SetFunction]) scopeFlags) (addLocalVars names)
      pure $
        if null stmts
          then Comment "Skipped declare with no supported arguments"
          else wrapStmtList stmts

declareScopeFlags :: Bool -> DeclareFlags -> [SetFlag]
declareScopeFlags inFunc flags =
  let scope =
        if declareGlobal flags || not inFunc
          then SetGlobal
          else SetFunction
      base = [scope]
   in if declareExport flags
        then base <> [SetExport]
        else base

parseDeclareFlags :: DeclareFlags -> [Token] -> TranslateM (DeclareFlags, [Token])
parseDeclareFlags = go
  where
    go acc [] = pure (acc, [])
    go acc (t : ts) =
      let txt = tokenToLiteralText t
       in if txt == "--"
            then pure (acc, ts)
            else
              if T.isPrefixOf "-" txt && T.length txt > 1
                then do
                  acc' <- foldM applyDeclareFlag acc (T.unpack (T.drop 1 txt))
                  go acc' ts
                else pure (acc, t : ts)

    applyDeclareFlag flags c =
      case c of
        'g' -> pure flags {declareGlobal = True}
        'x' -> pure flags {declareExport = True}
        'r' -> pure flags {declareReadonly = True}
        'a' -> pure flags
        'A' -> addWarnFlag flags "declare -A (associative arrays)"
        'i' -> addWarnFlag flags "declare -i (integer attributes)"
        _ -> addWarnFlag flags ("declare flag -" <> T.singleton c)

    addWarnFlag flags msg = do
      addWarning ("Unsupported " <> msg <> "; ignoring flag")
      pure flags

parseDeclareArg :: [SetFlag] -> Token -> TranslateM (Maybe Text, [FishStatement])
parseDeclareArg flags tok =
  case tok of
    T_Assignment _ _ var _ _ -> do
      let name = toText var
      stmts <- translateAssignmentWithFlagsM flags tok
      pure (Just name, stmts)
    _ -> do
      let txt = tokenToLiteralText tok
      case parseAssignmentLiteral txt of
        Just (name, valueTxt) ->
          let expr = ExprListLiteral [ExprLiteral valueTxt]
           in pure (Just name, [Stmt (Set flags name expr)])
        Nothing ->
          if isValidVarName txt
            then pure (Just txt, [Stmt (Set flags txt (ExprVariable (VarAll txt)))])
            else do
              addWarning ("Unsupported declare argument: " <> txt)
              pure (Nothing, [])

{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Builtins
  ( translateLocalCommand
  , translateExportCommand
  , translateDeclareCommand
  , translateReadonlyCommand
  , translateShiftCommand
  , translateUnsetCommand
  , translateTrapCommand
  , translateScopedAssignment
  ) where

import Control.Monad (foldM)
import qualified Data.Set as Set
import qualified Data.Text as T
import Language.Fish.AST
import Language.Fish.Translator.Monad
  ( TranslateM
  , TranslationContext(..)
  , addLocalVars
  , addWarning
  , context
  )
import Language.Fish.Translator.Names (isValidVarName)
import Language.Fish.Translator.Variables
import ShellCheck.AST

translateLocalCommand :: [Token] -> TranslateM FishStatement
translateLocalCommand args = do
  inFunc <- gets (inFunction . context)
  when (not inFunc) $
    addWarning "local used outside a function; fish will treat it as local to the current scope"
  parsed <- mapM parseLocalArg args
  let names = map fst (catMaybes parsed)
      stmts = concatMap snd (catMaybes parsed)
  addLocalVars names
  pure $
    if null stmts
      then Comment "Skipped local with no assignments"
      else wrapStmtList stmts

translateExportCommand :: [Token] -> TranslateM FishStatement
translateExportCommand args = do
  locals <- gets (localVars . context)
  parsed <- mapM (parseExportArg locals) args
  let stmts = concat parsed
  pure $
    if null stmts
      then Comment "Skipped export with no assignments"
      else wrapStmtList stmts

data DeclareFlags = DeclareFlags
  { declareGlobal :: Bool
  , declareExport :: Bool
  , declareReadonly :: Bool
  } deriving stock (Show, Eq)

defaultDeclareFlags :: DeclareFlags
defaultDeclareFlags = DeclareFlags False False False

translateDeclareCommand :: [Token] -> TranslateM FishStatement
translateDeclareCommand =
  translateDeclareCommandWith defaultDeclareFlags

translateReadonlyCommand :: [Token] -> TranslateM FishStatement
translateReadonlyCommand =
  translateDeclareCommandWith (defaultDeclareFlags { declareReadonly = True })

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
      when (SetLocal `elem` scopeFlags) (addLocalVars names)
      pure $
        if null stmts
          then Comment "Skipped declare with no supported arguments"
          else wrapStmtList stmts

declareScopeFlags :: Bool -> DeclareFlags -> [SetFlag]
declareScopeFlags inFunc flags =
  let scope =
        if declareGlobal flags || not inFunc
          then SetGlobal
          else SetLocal
      base = [scope]
  in if declareExport flags
      then base <> [SetExport]
      else base

parseDeclareFlags :: DeclareFlags -> [Token] -> TranslateM (DeclareFlags, [Token])
parseDeclareFlags base toks = go base toks
  where
    go acc [] = pure (acc, [])
    go acc (t:ts) =
      let txt = tokenToLiteralText t
      in if txt == "--"
           then pure (acc, ts)
           else if T.isPrefixOf "-" txt && T.length txt > 1
             then do
               acc' <- foldM applyDeclareFlag acc (T.unpack (T.drop 1 txt))
               go acc' ts
             else pure (acc, t:ts)

    applyDeclareFlag flags c =
      case c of
        'g' -> pure flags { declareGlobal = True }
        'x' -> pure flags { declareExport = True }
        'r' -> pure flags { declareReadonly = True }
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
    T_Assignment _ _ var _ _ ->
      let name = toText var
          stmts = translateAssignmentWithFlags flags tok
      in pure (Just name, stmts)
    _ -> do
      let txt = tokenToLiteralText tok
      case parseAssignmentLiteral txt of
        Just (name, valueTxt) ->
          let expr = ExprListLiteral [ExprLiteral valueTxt]
          in pure (Just name, [Stmt (Set flags name expr)])
        Nothing ->
          if isValidVarName txt
            then pure (Just txt, [Stmt (Set flags txt (ExprListLiteral []))])
            else do
              addWarning ("Unsupported declare argument: " <> txt)
              pure (Nothing, [])

translateShiftCommand :: [Token] -> TranslateM FishStatement
translateShiftCommand args =
  case args of
    [] -> pure (shiftByCount 1)
    [tok] ->
      case parseShiftCount tok of
        Just n | n >= 0 -> pure (shiftByCount n)
        Just _ -> do
          addWarning "shift count must be non-negative; emitting comment"
          pure (Comment "Unsupported shift count")
        Nothing -> do
          addWarning "Unsupported shift argument; emitting comment"
          pure (Comment "Unsupported shift argument")
    _ -> do
      addWarning "shift with multiple arguments is not supported; emitting comment"
      pure (Comment "Unsupported shift arguments")

shiftByCount :: Int -> FishStatement
shiftByCount n =
  let start = ExprNumLiteral (n + 1)
      range = IndexRange (Just start) (Just (ExprNumLiteral (-1)))
      expr = ExprVariable (VarIndex "argv" range)
  in Stmt (Set [] "argv" expr)

parseShiftCount :: Token -> Maybe Int
parseShiftCount tok =
  case tok of
    T_Literal _ s -> readMaybe (toString (toText s))
    _ -> readMaybe (toString (tokenToLiteralText tok))

data UnsetMode
  = UnsetVar
  | UnsetFunc
  deriving stock (Show, Eq)

translateUnsetCommand :: [Token] -> TranslateM FishStatement
translateUnsetCommand args = do
  stmts <- go UnsetVar [] args
  pure $
    case stmts of
      [] -> Comment "Skipped unset with no arguments"
      _ -> wrapStmtList stmts
  where
    go _ acc [] = pure (reverse acc)
    go mode acc (tok:rest) =
      let txt = tokenToLiteralText tok
      in if T.isPrefixOf "-" txt
           then case txt of
             "-f" -> go UnsetFunc acc rest
             "-v" -> go UnsetVar acc rest
             "--" -> go mode acc rest
             _ -> do
               addWarning ("Unsupported unset flag: " <> txt)
               go mode acc rest
           else if T.null txt
             then do
               addWarning "Unsupported unset argument: empty name"
               go mode acc rest
             else do
               let stmt = case mode of
                     UnsetFunc -> Stmt (Command "functions" [ExprVal (ExprLiteral "-e"), ExprVal (ExprLiteral txt)])
                     UnsetVar -> Stmt (Command "set" [ExprVal (ExprLiteral "-e"), ExprVal (ExprLiteral txt)])
               go mode (stmt : acc) rest

translateTrapCommand :: [Token] -> TranslateM FishStatement
translateTrapCommand args =
  case args of
    [] -> do
      addWarning "trap with no arguments is not supported"
      pure (Stmt (Command "trap" []))
    (cmdTok:signalToks) -> do
      let cmdExpr = translateTokenToExpr cmdTok
          rawSignals = map tokenToLiteralText signalToks
      if any isTrapOption rawSignals
        then do
          addWarning "trap options are not supported; emitting raw trap command"
          pure (Stmt (Command "trap" (map translateTokenToExprOrRedirect args)))
        else do
          let signals = if null rawSignals then ["EXIT"] else rawSignals
              trapStmts = map (trapForSignal cmdExpr) signals
          pure (wrapStmtList (map Stmt trapStmts))
  where
    isTrapOption sig = sig `elem` ["-p", "-l", "--"]

    trapForSignal :: FishExpr TStr -> Text -> FishCommand TStatus
    trapForSignal cmd sig
      | isExitSignal sig =
          Command "trap" [ExprVal (ExprLiteral "--on-exit"), ExprVal cmd]
      | otherwise =
          Command "trap"
            [ ExprVal (ExprLiteral "--on-signal")
            , ExprVal (ExprLiteral (normalizeSignal sig))
            , ExprVal cmd
            ]

    isExitSignal sig =
      let upper = T.toUpper sig
      in upper == "EXIT" || upper == "0"

    normalizeSignal sig =
      let upper = T.toUpper sig
      in fromMaybe upper (T.stripPrefix "SIG" upper)

translateScopedAssignment :: Set.Set Text -> Token -> [FishStatement]
translateScopedAssignment locals tok =
  case tok of
    T_Assignment _ _ var _ _ ->
      translateAssignmentWithFlags (scopeFlagsFor locals (toText var)) tok
    _ ->
      translateAssignmentWithFlags (scopeFlagsFor locals "") tok

scopeFlagsFor :: Set.Set Text -> Text -> [SetFlag]
scopeFlagsFor locals name =
  if Set.member name locals
    then [SetLocal]
    else [SetGlobal]

parseLocalArg :: Token -> TranslateM (Maybe (Text, [FishStatement]))
parseLocalArg tok =
  case tok of
    T_Assignment _ _ var _ _ ->
      let name = toText var
          stmts = translateAssignmentWithFlags [SetLocal] tok
      in pure (Just (name, stmts))
    _ -> do
      let txt = tokenToLiteralText tok
      case parseAssignmentLiteral txt of
        Just (name, valueTxt) ->
          let expr = ExprListLiteral [ExprLiteral valueTxt]
          in pure (Just (name, [Stmt (Set [SetLocal] name expr)]))
        Nothing ->
          if isValidVarName txt
            then pure (Just (txt, [Stmt (Set [SetLocal] txt (ExprListLiteral []))]))
            else do
              addWarning ("Unsupported local argument: " <> txt)
              pure Nothing

parseExportArg :: Set.Set Text -> Token -> TranslateM [FishStatement]
parseExportArg locals tok =
  case tok of
    T_Assignment _ _ var _ _ ->
      let name = toText var
          flags = exportFlags locals name
      in pure (translateAssignmentWithFlags flags tok)
    _ -> do
      let txt = tokenToLiteralText tok
      case parseAssignmentLiteral txt of
        Just (name, valueTxt) ->
          let expr = ExprListLiteral [ExprLiteral valueTxt]
          in pure [Stmt (Set (exportFlags locals name) name expr)]
        Nothing ->
          if isValidVarName txt
            then pure [Stmt (Set (exportFlags locals txt) txt (ExprVariable (VarAll txt)))]
            else do
              addWarning ("Unsupported export argument: " <> txt)
              pure []

exportFlags :: Set.Set Text -> Text -> [SetFlag]
exportFlags locals name =
  if Set.member name locals
    then [SetLocal, SetExport]
    else [SetGlobal, SetExport]

parseAssignmentLiteral :: Text -> Maybe (Text, Text)
parseAssignmentLiteral txt = do
  let trimmed = T.strip txt
  guard (not (T.null trimmed))
  let (lhs, rest) = T.breakOn "=" trimmed
  guard (T.isPrefixOf "=" rest)
  guard (isValidVarName lhs)
  pure (lhs, T.drop 1 rest)

wrapStmtList :: [FishStatement] -> FishStatement
wrapStmtList [stmt] = stmt
wrapStmtList stmts  = StmtList stmts

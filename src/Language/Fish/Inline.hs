{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Inline
  ( Translation (..),
    inlineStatements,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import Language.Fish.AST
import Language.Fish.Translator.Monad (TranslateState)

data Translation = Translation
  { trPath :: FilePath,
    trStatements :: [FishStatement],
    trState :: TranslateState,
    trSourceMap :: M.Map Text (Maybe FilePath)
  }
  deriving stock (Show, Eq)

type WarnFn = Text -> IO ()

inlineStatements ::
  WarnFn ->
  M.Map FilePath Translation ->
  Set.Set FilePath ->
  FilePath ->
  IO [FishStatement]
inlineStatements warn translations stack path =
  case M.lookup path translations of
    Nothing -> do
      warn ("warning: missing translation for sourced file: " <> toText path)
      pure [Comment ("Missing source: " <> toText path)]
    Just tr -> do
      let stack' = Set.insert path stack
      concatMapM (inlineStatement warn translations stack' tr) (trStatements tr)

inlineStatement ::
  WarnFn ->
  M.Map FilePath Translation ->
  Set.Set FilePath ->
  Translation ->
  FishStatement ->
  IO [FishStatement]
inlineStatement warn translations stack tr = \case
  Stmt (Source expr) ->
    case expr of
      ExprLiteral txt ->
        case M.lookup txt (trSourceMap tr) >>= id of
          Just resolved
            | Set.member resolved stack -> do
                warn ("warning: recursive source detected: " <> toText resolved)
                pure [Comment ("Skipped recursive source: " <> toText resolved)]
            | otherwise -> inlineStatements warn translations stack resolved
          Nothing -> pure [Stmt (Source expr)]
      _ -> do
        warn "warning: non-literal source path; cannot inline"
        pure [Comment "Non-literal source path; kept as source", Stmt (Source expr)]
  Stmt (Command "source" args) ->
    inlineSourceCommand warn translations stack tr args
  StmtList stmts -> concatMapM (inlineStatement warn translations stack tr) stmts
  Stmt cmd -> do
    cmd' <- inlineCommand warn translations stack tr cmd
    pure [Stmt cmd']
  other -> pure [other]

inlineSourceCommand ::
  WarnFn ->
  M.Map FilePath Translation ->
  Set.Set FilePath ->
  Translation ->
  [ExprOrRedirect] ->
  IO [FishStatement]
inlineSourceCommand warn translations stack tr args =
  case args of
    (ExprVal pathExpr : rest)
      | Just txt <- literalPath pathExpr ->
          case M.lookup txt (trSourceMap tr) >>= id of
            Just resolved
              | Set.member resolved stack -> do
                  warn ("warning: recursive source detected: " <> toText resolved)
                  pure [Comment ("Skipped recursive source: " <> toText resolved)]
              | otherwise -> do
                  inlined <- inlineStatements warn translations stack resolved
                  if null rest
                    then pure inlined
                    else
                      if all isExprVal rest
                        then do
                          let depth = Set.size stack + 1
                              saveVar = "__monk_saved_argv_" <> T.pack (show depth)
                              saveStmt =
                                Stmt
                                  ( Command
                                      "set"
                                      [ ExprVal (ExprLiteral "--local"),
                                        ExprVal (ExprLiteral saveVar),
                                        ExprVal (ExprVariable (VarAll "argv"))
                                      ]
                                  )
                              setArgvStmt =
                                Stmt
                                  ( Command
                                      "set"
                                      (ExprVal (ExprLiteral "argv") : rest)
                                  )
                              restoreStmt =
                                Stmt
                                  ( Command
                                      "set"
                                      [ ExprVal (ExprLiteral "argv"),
                                        ExprVal (ExprVariable (VarAll saveVar))
                                      ]
                                  )
                              cleanupStmt =
                                Stmt
                                  ( Command
                                      "set"
                                      [ ExprVal (ExprLiteral "-e"),
                                        ExprVal (ExprLiteral saveVar)
                                      ]
                                  )
                          pure (saveStmt : setArgvStmt : inlined <> [restoreStmt, cleanupStmt])
                        else pure [Stmt (Command "source" args)]
            Nothing -> pure [Stmt (Command "source" args)]
    _ -> do
      warn "warning: non-literal source path; cannot inline"
      pure [Comment "Non-literal source path; kept as source", Stmt (Command "source" args)]
  where
    isExprVal = \case
      ExprVal {} -> True
      _ -> False

    literalPath :: FishExpr t -> Maybe Text
    literalPath = \case
      ExprLiteral txt -> Just txt
      ExprListLiteral [ExprLiteral txt] -> Just txt
      _ -> Nothing

inlineCommand ::
  WarnFn ->
  M.Map FilePath Translation ->
  Set.Set FilePath ->
  Translation ->
  FishCommand t ->
  IO (FishCommand t)
inlineCommand warn translations stack tr = \case
  Begin body suffix -> do
    body' <- inlineBody warn translations stack tr body
    pure (Begin body' suffix)
  If cond thn els suffix -> do
    thn' <- inlineBody warn translations stack tr thn
    els' <- inlineBodyList warn translations stack tr els
    pure (If cond thn' els' suffix)
  While cond body suffix -> do
    body' <- inlineBody warn translations stack tr body
    pure (While cond body' suffix)
  For var listExpr body suffix -> do
    body' <- inlineBody warn translations stack tr body
    pure (For var listExpr body' suffix)
  Switch expr cases suffix -> do
    cases' <- traverse (inlineCaseItem warn translations stack tr) cases
    pure (Switch expr cases' suffix)
  Function func -> do
    body' <- inlineBody warn translations stack tr (funcBody func)
    pure (Function func {funcBody = body'})
  other -> pure other

inlineBody ::
  WarnFn ->
  M.Map FilePath Translation ->
  Set.Set FilePath ->
  Translation ->
  NonEmpty FishStatement ->
  IO (NonEmpty FishStatement)
inlineBody warn translations stack tr body = do
  body' <- inlineBodyList warn translations stack tr (NE.toList body)
  case NE.nonEmpty body' of
    Just neBody -> pure neBody
    Nothing -> pure (Comment "Skipped empty inlined body" NE.:| [])

inlineBodyList ::
  WarnFn ->
  M.Map FilePath Translation ->
  Set.Set FilePath ->
  Translation ->
  [FishStatement] ->
  IO [FishStatement]
inlineBodyList warn translations stack tr stmts =
  concatMapM (inlineStatement warn translations stack tr) stmts

inlineCaseItem ::
  WarnFn ->
  M.Map FilePath Translation ->
  Set.Set FilePath ->
  Translation ->
  CaseItem ->
  IO CaseItem
inlineCaseItem warn translations stack tr (CaseItem pats body) = do
  body' <- inlineBody warn translations stack tr body
  pure (CaseItem pats body')

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = fmap concat (mapM f xs)

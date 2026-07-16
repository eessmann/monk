{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Inline translated scripts by replacing literal @source@ calls with
-- previously translated statements.
module Language.Fish.Inline
  ( Translation (..),
    InlineEvent (..),
    WarnFn,
    inlineStatements,
    sourceStatusHelperStatement,
  )
where

import Control.Monad.Extra (concatMapM)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import Language.Fish.DSL.Internal
import Monk.Translation.Types (Diagnostic, RuntimeRequirement)

-- | Translation artifacts for one source file.
data Translation = MkTranslation
  { -- | Source path used for this translation.
    trPath :: FilePath,
    -- | Structurally typed translated fish script.
    trScript :: Script,
    -- | Ordered diagnostics for this source.
    trDiagnostics :: [Diagnostic],
    -- | Deduplicated generated runtime dependencies.
    trRuntimeRequirements :: [RuntimeRequirement],
    -- | Mapping from literal source paths to resolved files.
    trSourceMap :: M.Map Text (Maybe FilePath)
  }
  deriving stock (Show, Eq)

data InlineEvent
  = InlineWarning Text
  | InlineNeedsSourceStatusHelper

-- | Callback used to report inlining events.
type WarnFn = InlineEvent -> IO ()

-- | Inline transitive @source@ statements for the file at @path@.
--
-- The @translations@ map should contain all translated files, and @stack@
-- tracks the current include chain so recursion can be detected.
inlineStatements ::
  WarnFn ->
  M.Map FilePath Translation ->
  Set.Set FilePath ->
  FilePath ->
  IO [FishStatement]
inlineStatements warn translations stack path =
  case M.lookup path translations of
    Nothing -> do
      warn (InlineWarning ("warning: missing translation for sourced file: " <> toText path))
      pure [Comment ("Missing source: " <> toText path)]
    Just tr -> do
      let stack' = Set.insert path stack
          MkScript statements = trScript tr
      concatMapM (inlineStatement warn translations stack' tr) statements

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
        case join (M.lookup txt (trSourceMap tr)) of
          Just resolved
            | Set.member resolved stack -> do
                warn (InlineWarning ("warning: recursive source detected: " <> toText resolved))
                pure [Comment ("Skipped recursive source: " <> toText resolved)]
            | otherwise -> inlineStatements warn translations stack resolved
          Nothing -> pure [Stmt (Source expr)]
      _ -> do
        warn (InlineWarning "warning: non-literal source path; cannot inline")
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
          case join (M.lookup txt (trSourceMap tr)) of
            Just resolved
              | Set.member resolved stack -> do
                  warn (InlineWarning ("warning: recursive source detected: " <> toText resolved))
                  pure [Comment ("Skipped recursive source: " <> toText resolved)]
              | otherwise -> do
                  inlined <- inlineStatements warn translations stack resolved
                  let sourceArgs = [arg | arg@ExprVal {} <- rest]
                      sourceRedirects = [redirect | redirect@RedirectVal {} <- rest]
                      depth = Set.size stack + 1
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
                              (ExprVal (ExprLiteral "argv") : sourceArgs)
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
                      sourceStatusVar = "__monk_source_status_" <> T.pack (show depth)
                      captureStatusStmt =
                        Stmt
                          ( Command
                              "set"
                              [ ExprVal (ExprLiteral "--local"),
                                ExprVal (ExprLiteral sourceStatusVar),
                                ExprVal (ExprSpecialVar SVStatus)
                              ]
                          )
                      returnStatusStmt =
                        Stmt
                          ( Command
                              "__monk_source_return_status"
                              [ExprVal (ExprVariable (VarScalar sourceStatusVar))]
                          )
                  if null sourceArgs
                    then
                      pure $
                        if null sourceRedirects
                          then inlined
                          else [Stmt (Begin (statementsOrPlaceholder inlined) sourceRedirects)]
                    else do
                      warn InlineNeedsSourceStatusHelper
                      let withArgv =
                            saveStmt
                              : setArgvStmt
                              : inlined
                                <> [captureStatusStmt, restoreStmt, cleanupStmt, returnStatusStmt]
                      pure [Stmt (Begin (statementsOrPlaceholder withArgv) sourceRedirects)]
            Nothing -> pure [Stmt (Command "source" args)]
    _ -> do
      warn (InlineWarning "warning: non-literal source path; cannot inline")
      pure [Comment "Non-literal source path; kept as source", Stmt (Command "source" args)]
  where
    literalPath :: FishExpr t -> Maybe Text
    literalPath = \case
      ExprLiteral txt -> Just txt
      ExprListLiteral [ExprLiteral txt] -> Just txt
      _ -> Nothing

sourceStatusHelperStatement :: FishStatement
sourceStatusHelperStatement =
  Stmt
    ( Function
        MkFishFunction
          { funcName = "__monk_source_return_status",
            funcFlags = [],
            funcParams = ["code"],
            funcBody =
              Stmt
                ( Return
                    ( Just
                        (ExprMath (ExprVariable (VarScalar "code") NE.:| []))
                    )
                )
                NE.:| []
          }
    )

inlineCommand ::
  WarnFn ->
  M.Map FilePath Translation ->
  Set.Set FilePath ->
  Translation ->
  FishCommand t ->
  IO (FishCommand t)
inlineCommand warn translations stack tr = \case
  Source expr -> do
    statements <- inlineStatement warn translations stack tr (Stmt (Source expr))
    pure (statementsAsStatusCommand statements)
  Command "source" args -> do
    statements <- inlineSourceCommand warn translations stack tr args
    pure (statementsAsStatusCommand statements)
  Command name args -> Command name <$> traverse (inlineArg warn translations stack tr) args
  Set flags name values -> Set flags name <$> inlineExpr warn translations stack tr values
  Begin body suffix -> do
    body' <- inlineBody warn translations stack tr body
    suffix' <- traverse (inlineArg warn translations stack tr) suffix
    pure (Begin body' suffix')
  If cond thn els suffix -> do
    cond' <- inlineJobList warn translations stack tr cond
    thn' <- inlineBody warn translations stack tr thn
    els' <- inlineBodyList warn translations stack tr els
    suffix' <- traverse (inlineArg warn translations stack tr) suffix
    pure (If cond' thn' els' suffix')
  While cond body suffix -> do
    cond' <- inlineJobList warn translations stack tr cond
    body' <- inlineBody warn translations stack tr body
    suffix' <- traverse (inlineArg warn translations stack tr) suffix
    pure (While cond' body' suffix')
  For var listExpr body suffix -> do
    listExpr' <- inlineExpr warn translations stack tr listExpr
    body' <- inlineBody warn translations stack tr body
    suffix' <- traverse (inlineArg warn translations stack tr) suffix
    pure (For var listExpr' body' suffix')
  Switch expr cases suffix -> do
    expr' <- inlineExpr warn translations stack tr expr
    cases' <- traverse (inlineCaseItem warn translations stack tr) cases
    suffix' <- traverse (inlineArg warn translations stack tr) suffix
    pure (Switch expr' cases' suffix')
  Function func -> do
    body' <- inlineBody warn translations stack tr (funcBody func)
    pure (Function func {funcBody = body'})
  Return status -> Return <$> traverse (inlineExpr warn translations stack tr) status
  Exit status -> Exit <$> traverse (inlineExpr warn translations stack tr) status
  Eval expr -> Eval <$> inlineExpr warn translations stack tr expr
  Echo exprs -> Echo <$> traverse (inlineExpr warn translations stack tr) exprs
  Printf format args ->
    Printf
      <$> inlineExpr warn translations stack tr format
      <*> traverse (inlineExpr warn translations stack tr) args
  Pipeline pipeline -> Pipeline <$> inlinePipeline warn translations stack tr pipeline
  JobConj conjunction -> JobConj <$> inlineConjunction warn translations stack tr conjunction
  Semicolon left right ->
    Semicolon
      <$> inlineCommand warn translations stack tr left
      <*> inlineCommand warn translations stack tr right
  Not command -> Not <$> inlineCommand warn translations stack tr command
  Background command -> Background <$> inlineCommand warn translations stack tr command
  Wait status -> Wait <$> traverse (inlineExpr warn translations stack tr) status
  Exec command args ->
    Exec
      <$> inlineExpr warn translations stack tr command
      <*> traverse (inlineArg warn translations stack tr) args
  Decorated _ sourceCommand@(Source _) ->
    inlineCommand warn translations stack tr sourceCommand
  Decorated _ sourceCommand@(Command "source" _) ->
    inlineCommand warn translations stack tr sourceCommand
  Decorated decoration command ->
    Decorated decoration <$> inlineCommand warn translations stack tr command
  other -> pure other

statementsAsStatusCommand :: [FishStatement] -> FishCommand TStatus
statementsAsStatusCommand statements =
  Begin
    (statementsOrPlaceholder statements)
    []

statementsOrPlaceholder :: [FishStatement] -> NonEmpty FishStatement
statementsOrPlaceholder =
  fromMaybe (Comment "Skipped empty inlined source" NE.:| []) . NE.nonEmpty

inlineStatementAsOne ::
  WarnFn ->
  M.Map FilePath Translation ->
  Set.Set FilePath ->
  Translation ->
  FishStatement ->
  IO FishStatement
inlineStatementAsOne warn translations stack tr statement = do
  statements <- inlineStatement warn translations stack tr statement
  pure $
    case statements of
      [single] -> single
      multiple -> Stmt (statementsAsStatusCommand multiple)

inlineJobList ::
  WarnFn ->
  M.Map FilePath Translation ->
  Set.Set FilePath ->
  Translation ->
  FishJobList ->
  IO FishJobList
inlineJobList warn translations stack tr (MkFishJobList conjunctions) =
  MkFishJobList <$> traverse (inlineConjunction warn translations stack tr) conjunctions

inlineConjunction ::
  WarnFn ->
  M.Map FilePath Translation ->
  Set.Set FilePath ->
  Translation ->
  FishJobConjunction ->
  IO FishJobConjunction
inlineConjunction warn translations stack tr conjunction = do
  job' <- inlinePipeline warn translations stack tr (jcJob conjunction)
  continuations' <- traverse inlineContinuation (jcContinuations conjunction)
  pure conjunction {jcJob = job', jcContinuations = continuations'}
  where
    inlineContinuation = \case
      JCAnd pipeline -> JCAnd <$> inlinePipeline warn translations stack tr pipeline
      JCOr pipeline -> JCOr <$> inlinePipeline warn translations stack tr pipeline

inlinePipeline ::
  WarnFn ->
  M.Map FilePath Translation ->
  Set.Set FilePath ->
  Translation ->
  FishJobPipeline ->
  IO FishJobPipeline
inlinePipeline warn translations stack tr pipeline = do
  variables' <- traverse (inlineAssignment warn translations stack tr) (jpVariables pipeline)
  statement' <- inlineStatementAsOne warn translations stack tr (jpStatement pipeline)
  continuations' <- traverse inlineContinuation (jpCont pipeline)
  pure
    pipeline
      { jpVariables = variables',
        jpStatement = statement',
        jpCont = continuations'
      }
  where
    inlineContinuation continuation = do
      variables' <- traverse (inlineAssignment warn translations stack tr) (jpcVariables continuation)
      statement' <- inlineStatementAsOne warn translations stack tr (jpcStatement continuation)
      pure continuation {jpcVariables = variables', jpcStatement = statement'}

inlineAssignment ::
  WarnFn ->
  M.Map FilePath Translation ->
  Set.Set FilePath ->
  Translation ->
  VariableAssignment ->
  IO VariableAssignment
inlineAssignment warn translations stack tr assignment = do
  value' <- traverse (inlineExpr warn translations stack tr) (vaValue assignment)
  pure assignment {vaValue = value'}

inlineArg ::
  WarnFn ->
  M.Map FilePath Translation ->
  Set.Set FilePath ->
  Translation ->
  ExprOrRedirect ->
  IO ExprOrRedirect
inlineArg warn translations stack tr = \case
  ExprVal expr -> ExprVal <$> inlineExpr warn translations stack tr expr
  RedirectVal redirect -> RedirectVal <$> inlineRedirect warn translations stack tr redirect

inlineRedirect ::
  WarnFn ->
  M.Map FilePath Translation ->
  Set.Set FilePath ->
  Translation ->
  Redirect ->
  IO Redirect
inlineRedirect warn translations stack tr redirect = do
  target' <-
    case redirTarget redirect of
      RedirectFile expr -> RedirectFile <$> inlineExpr warn translations stack tr expr
      other -> pure other
  pure redirect {redirTarget = target'}

inlineExpr ::
  WarnFn ->
  M.Map FilePath Translation ->
  Set.Set FilePath ->
  Translation ->
  FishExpr t ->
  IO (FishExpr t)
inlineExpr warn translations stack tr = \case
  ExprVariable (VarIndex name index) ->
    ExprVariable . VarIndex name <$> inlineIndex warn translations stack tr index
  ExprStringConcat left right ->
    ExprStringConcat
      <$> inlineExpr warn translations stack tr left
      <*> inlineExpr warn translations stack tr right
  ExprStringOp operation expr -> ExprStringOp operation <$> inlineExpr warn translations stack tr expr
  ExprJoinList expr -> ExprJoinList <$> inlineExpr warn translations stack tr expr
  ExprMath exprs -> ExprMath <$> traverse (inlineExpr warn translations stack tr) exprs
  ExprCommandSubst body -> ExprCommandSubst <$> inlineBody warn translations stack tr body
  ExprListLiteral exprs -> ExprListLiteral <$> traverse (inlineExpr warn translations stack tr) exprs
  ExprListConcat left right ->
    ExprListConcat
      <$> inlineExpr warn translations stack tr left
      <*> inlineExpr warn translations stack tr right
  ExprProcessSubst body -> ExprProcessSubst <$> inlineBody warn translations stack tr body
  other -> pure other

inlineIndex ::
  WarnFn ->
  M.Map FilePath Translation ->
  Set.Set FilePath ->
  Translation ->
  FishIndex a b ->
  IO (FishIndex a b)
inlineIndex warn translations stack tr = \case
  IndexSingle expr -> IndexSingle <$> inlineExpr warn translations stack tr expr
  IndexRange start end ->
    IndexRange
      <$> traverse (inlineExpr warn translations stack tr) start
      <*> traverse (inlineExpr warn translations stack tr) end
  IndexList exprs -> IndexList <$> traverse (inlineExpr warn translations stack tr) exprs

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
inlineBodyList warn translations stack tr =
  concatMapM (inlineStatement warn translations stack tr)

inlineCaseItem ::
  WarnFn ->
  M.Map FilePath Translation ->
  Set.Set FilePath ->
  Translation ->
  CaseItem ->
  IO CaseItem
inlineCaseItem warn translations stack tr (MkCaseItem pats body) = do
  pats' <- traverse (inlineExpr warn translations stack tr) pats
  body' <- inlineBody warn translations stack tr body
  pure (MkCaseItem pats' body')

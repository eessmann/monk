{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | Native specialization of the authoritative source plan. Failure to prove
-- every operation eligible leaves the plan to the general materializer.
module Language.Fish.Translator.Native
  ( nativeStatements,
    nativeEcho,
    nativeRegionPrefix,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Set qualified as S
import Data.Text qualified as T
import Language.Bash.Plan qualified as P
import Language.Bash.Plan.Control qualified as Control
import Language.Bash.Plan.Effects qualified as Effects
import Language.Fish.DSL.Internal
import Language.Fish.Translator.Identifier (compilerCommandName, compilerIdentifier)
import Language.Fish.Translator.NativeRuntime qualified as NativeRuntime
import Prelude hiding (Word)

-- Fish set preserves status; a Bash assignment-only command produces zero.
-- Keeping that fact here avoids both a status slot and per-assignment commands.
data Status = FishStatus | ZeroStatus | SourceStatus Text

data NativeState = MkNativeState
  { nativeStatus :: Status,
    nativePrefix :: Text,
    nativeUsesExec :: Bool,
    nativeUsesWriter :: Bool,
    nativeRegionMode :: Bool
  }

type Native = StateT NativeState Maybe

getStatus :: Native Status
getStatus = gets nativeStatus

putStatus :: Status -> Native ()
putStatus value = modify' (\nativeState -> nativeState {nativeStatus = value})

nativeStatements :: Control.Root scope -> Text -> [P.Statement scope] -> Maybe ([FishStatement], Set Text, Bool)
nativeStatements root prefix statements = do
  (body, final) <- runStateT (lowerStatements root statements) (MkNativeState ZeroStatus prefix (not (S.null (foldMap commands statements))) False False)
  pure (body <> settle (nativeStatus final), foldMap commands statements, nativeUsesWriter final)
  where
    commands :: P.Statement scope -> Set Text
    commands (P.Statement _ node) = case node of
      P.Invoke (P.External name) _ -> S.singleton name
      P.Sequence body -> foldMap commands body
      P.AssignmentCommand _ body -> foldMap commands body
      P.And left right -> commands left <> commands right
      P.Or left right -> commands left <> commands right
      P.Negate value -> commands value
      P.Conditional predicate yes no -> foldMap commands (predicate <> yes <> no)
      P.DefineFunction _ body -> P.bodyEffects commands body
      P.Pipeline children -> foldMap (P.bodyEffects commands . P.childBody) children
      _ -> mempty

-- | Specialize a maximal prefix of a mixed region in one linear pass. The
-- general materializer owns binding writes, descriptor scopes and callbacks;
-- this mode admits native scalar/control consumers only. Source status enters
-- explicitly and Fish status is captured once by the enclosing materializer.
nativeRegionPrefix :: Control.Root scope -> Text -> Text -> [P.Statement scope] -> Maybe ([FishStatement], [P.Statement scope], Bool)
nativeRegionPrefix root prefix incoming = go (MkNativeState (SourceStatus incoming) prefix False False True) []
  where
    go nativeState chunks [] = finishPrefix nativeState chunks []
    go nativeState chunks remaining@(nativeStatement : rest) =
      case Effects.proveNativeRegion nativeStatement >>= \proof -> runStateT (lowerStatement root (Effects.nativeRegionStatement proof)) nativeState of
        Nothing -> finishPrefix nativeState chunks remaining
        Just (body, next) -> go next (body : chunks) rest
    finishPrefix _ [] _ = Nothing
    finishPrefix nativeState chunks remaining = case nativeStatus nativeState of
      SourceStatus _ -> Nothing
      final -> Just (concat (reverse chunks) <> settle final, remaining, nativeUsesWriter nativeState)

wholeProgramOnly :: Native ()
wholeProgramOnly = gets nativeRegionMode >>= guard . not

settle :: Status -> [FishStatement]
settle FishStatus = []
settle (SourceStatus _) = []
settle ZeroStatus = [builtin "true" []]

finish :: Native [FishStatement] -> Native [FishStatement]
finish action = do
  body <- action
  final <- getStatus
  putStatus FishStatus
  pure (body <> settle final)

lowerStatements :: Control.Root scope -> [P.Statement scope] -> Native [FishStatement]
lowerStatements root = fmap concat . traverse (lowerStatement root)

lowerStatement :: Control.Root scope -> P.Statement scope -> Native [FishStatement]
lowerStatement root (P.Statement range node) = case node of
  P.Sequence body -> lowerStatements root body
  P.AssignmentCommand _ body -> do
    wholeProgramOnly
    incoming <- getStatus
    output <- forM body $ \case
      P.Statement _ (P.Assign storage name value) -> do
        putStatus incoming
        assignment storage name value
      _ -> lift Nothing
    putStatus ZeroStatus
    pure (concat output)
  P.Assign storage name value -> do
    output <- assignment storage name value
    putStatus ZeroStatus
    pure output
  P.DeclarationCommand [P.DeclareExport storage name (Just value)] -> do
    ordinaryStorage storage
    guard (name /= "IFS")
    expression <- scalar value
    putStatus ZeroStatus
    pure [setValue [SetGlobal, SetExport, SetUnpath] name expression]
  P.SetArguments target values -> do
    Control.consumeSetArguments root target `seq` pure ()
    wholeProgramOnly
    arguments <- concat <$> traverse word values
    putStatus ZeroStatus
    pure [builtin "set" ([literal "argv"] <> arguments)]
  P.Invoke (P.Builtin "echo") values -> do
    literals <- lift (traverse literalWord values)
    (newline, output) <- lift (nativeEcho literals)
    putStatus FishStatus
    prefix <- gets nativePrefix
    modify' (\nativeState -> nativeState {nativeUsesWriter = True})
    pure [NativeRuntime.nativeWriterInvocation prefix range "echo-bytes" [literal (T.intercalate " " output <> if newline then "\n" else "")]]
  P.Invoke (P.Builtin "printf") values -> do
    arguments <- concat <$> traverse word values
    prefix <- gets nativePrefix
    modify' (\nativeState -> nativeState {nativeUsesWriter = True})
    putStatus FishStatus
    pure [NativeRuntime.nativeWriterInvocation prefix range "printf" arguments]
  P.Invoke target values -> do
    case target of P.Builtin _ -> pure (); _ -> wholeProgramOnly
    arguments <- concat <$> traverse word values
    incoming <- getStatus
    putStatus FishStatus
    prefix <- gets nativePrefix
    let preparation = case target of P.Function _ -> settle incoming; _ -> []
    pure
      ( preparation
          <> [ case target of
                 P.Builtin name -> builtin (if name == ":" then "true" else name) arguments
                 P.External name -> externalSite prefix range name arguments
                 P.Function name -> Stmt (Command (compilerCommandName name) arguments)
             ]
      )
  P.Pipeline children -> do
    wholeProgramOnly
    stages <- traverse stage children
    putStatus FishStatus
    let leading :| rest = stages
    pure [Stmt (Pipeline (MkFishJobPipeline False [] leading (map (PipeTo []) rest)))]
  P.And left right -> conjunction root JCAnd left right
  P.Or left right -> conjunction root JCOr left right
  P.Negate value -> do
    body <- finish (lowerStatement root value)
    pure [Stmt (Not (Begin (nonempty body) []))]
  P.Conditional predicate yes no -> do
    predicateBody <- finish (lowerStatements root predicate)
    yesBody <- finish (lowerStatements root yes)
    putStatus FishStatus
    noBody <- finish (lowerStatements root no)
    pure [Stmt (If (condition predicateBody) (nonempty yesBody) noBody [])]
  P.DefineFunction name body -> do
    wholeProgramOnly
    putStatus FishStatus
    bodyValue <- P.withScopedBody body (\owned statements -> finish (lowerStatements owned statements))
    putStatus ZeroStatus
    prefix <- gets nativePrefix
    usesExec <- gets nativeUsesExec
    pure [Stmt (Function (MkFishFunction name [FuncCaptureVariable (compilerIdentifier (NativeRuntime.runtimePathName prefix)) | usesExec] [] (nonempty bodyValue)))]
  P.Return target value -> do
    let role = Control.consumeReturn root target
    role `seq` pure ()
    wholeProgramOnly
    expression <- maybe status scalar value
    pure [Stmt (ReturnScalar expression)]
  P.Exit value -> do
    wholeProgramOnly
    expression <- maybe status scalar value
    pure [builtin "exit" [ExprVal expression]]
  _ -> lift Nothing

-- The native pipeline proof has no shell state writes, option changes or
-- effectful expansions. Bash's default pipeline status is the final stage.
stage :: P.ChildRegion -> Native FishStatement
stage region = P.withScopedBody (P.childBody region) $ \_ statements -> case concatMap flatten statements of
  [P.Statement range (P.Invoke (P.External name) values)] -> do
    arguments <- concat <$> traverse word values
    prefix <- gets nativePrefix
    pure (externalSite prefix range name arguments)
  _ -> lift Nothing
  where
    flatten :: P.Statement scope -> [P.Statement scope]
    flatten (P.Statement _ (P.Sequence body)) = concatMap flatten body
    flatten value = [value]

assignment :: P.Storage -> Text -> P.Scalar -> Native [FishStatement]
assignment storage name value = do
  ordinaryStorage storage
  guard (name /= "IFS")
  expression <- scalar value
  pure [setValue [SetGlobal, SetUnpath] name expression]

ordinaryStorage :: P.Storage -> Native ()
ordinaryStorage P.Global = wholeProgramOnly
ordinaryStorage P.Visible = wholeProgramOnly
ordinaryStorage _ = lift Nothing

conjunction :: Control.Root scope -> (FishJobPipeline -> FishJobConjCont) -> P.Statement scope -> P.Statement scope -> Native [FishStatement]
conjunction root connective left right = do
  leftBody <- finish (lowerStatement root left)
  rightBody <- finish (lowerStatement root right)
  pure [Stmt (JobConj (MkFishJobConjunction Nothing (job leftBody) [connective (job rightBody)]))]

word :: P.Word -> Native [ExprOrRedirect]
word (P.OneField value) = (: []) . ExprVal <$> scalar value
word (P.QuotedArguments (P.Literal "") (P.Literal "") False) = pure [ExprVal (ExprVariable (VarAll "argv"))]
word _ = lift Nothing

scalar :: P.Scalar -> Native (FishExpr TStr)
scalar = \case
  P.Literal value -> pure (ExprLiteral value)
  P.Variable name | name `notElem` ["IFS", "_"] -> pure (variable name)
  P.Positional index -> pure (ExprQuotedVariable (VarIndex "argv" (IndexSingle (ExprNumLiteral index))))
  P.ArgumentCount -> pure (ExprQuotedCommandSubst (builtin "count" [ExprVal (ExprVariable (VarAll "argv"))] :| []))
  P.LastStatus -> status
  P.Concat values -> foldr ExprStringConcat (ExprLiteral "") <$> traverse scalar values
  _ -> lift Nothing

status :: Native (FishExpr TStr)
status = gets ((\case FishStatus -> variable "status"; ZeroStatus -> ExprLiteral "0"; SourceStatus name -> variable name) . nativeStatus)

externalSite :: Text -> Maybe SourceRange -> Text -> [ExprOrRedirect] -> FishStatement
externalSite prefix range name arguments =
  let origin = maybe "<input>" (srcFile . rangeStart) range
      line = maybe "1" (show . srcLine . rangeStart) range
   in Stmt (Decorated DecCommand (CommandExpr (variableExecutable (compilerIdentifier (NativeRuntime.runtimePathName prefix))) (map literal ["--abi", "2", "exec-site", origin, line, name] <> arguments)))

variable :: Text -> FishExpr TStr
variable = ExprQuotedVariable . VarScalar . compilerIdentifier

literal :: Text -> ExprOrRedirect
literal = ExprVal . ExprLiteral

builtin :: Text -> [ExprOrRedirect] -> FishStatement
builtin name arguments = Stmt (Decorated DecBuiltin (Command (compilerCommandName name) arguments))

setValue :: [SetFlag] -> Text -> FishExpr TStr -> FishStatement
setValue flags name value = Stmt (Decorated DecBuiltin (Set flags (compilerIdentifier name) (ExprListLiteral [value])))

nonempty :: [FishStatement] -> NonEmpty FishStatement
nonempty = fromMaybe (builtin "true" [] :| []) . NE.nonEmpty

job :: [FishStatement] -> FishJobPipeline
job body = MkFishJobPipeline False [] (case body of [single] -> single; _ -> Stmt (Begin (nonempty body) [])) []

condition :: [FishStatement] -> FishJobList
condition body = MkFishJobList (MkFishJobConjunction Nothing (job body) [] :| [])

literalWord :: P.Word -> Maybe Text
literalWord (P.OneField (P.Literal value)) = Just value
literalWord _ = Nothing

-- Shared by ordinary and redirected invocation lowering.
nativeEcho :: [Text] -> Maybe (Bool, [Text])
nativeEcho = options True False
  where
    options newline escapes (value : rest)
      | Just flags <- T.stripPrefix "-" value,
        not (T.null flags),
        T.all (`elem` ['n', 'e', 'E']) flags =
          let step (n, e) flag = case flag of 'n' -> (False, e); 'e' -> (n, True); _ -> (n, False)
              (nextNewline, nextEscapes) = T.foldl' step (newline, escapes) flags
           in options nextNewline nextEscapes rest
    options newline escapes values
      | not escapes || not (any (T.any (== '\\')) values) = Just (newline, values)
      | otherwise = Nothing

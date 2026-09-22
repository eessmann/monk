{-# LANGUAGE DataKinds #-}

-- | Native specialization of the authoritative source plan. Failure to prove
-- every operation eligible leaves the plan to the general materializer.
module Language.Fish.Translator.Native
  ( nativeStatements,
    nativeEcho,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Set qualified as S
import Data.Text qualified as T
import Language.Bash.Plan qualified as P
import Language.Fish.DSL.Internal
import Language.Fish.Translator.NativeRuntime qualified as NativeRuntime
import Prelude hiding (Word)

-- Fish set preserves status; a Bash assignment-only command produces zero.
-- Keeping that fact here avoids both a status slot and per-assignment commands.
data Status = FishStatus | ZeroStatus

data NativeState = MkNativeState
  { nativeStatus :: Status,
    nativePrefix :: Text,
    nativeUsesExec :: Bool,
    nativeUsesWriter :: Bool
  }

type Native = StateT NativeState Maybe

getStatus :: Native Status
getStatus = gets nativeStatus

putStatus :: Status -> Native ()
putStatus value = modify' (\nativeState -> nativeState {nativeStatus = value})

nativeStatements :: Text -> [P.Statement] -> Maybe ([FishStatement], Set Text, Bool)
nativeStatements prefix statements = do
  (body, final) <- runStateT (lowerStatements statements) (MkNativeState ZeroStatus prefix (not (S.null (foldMap commands statements))) False)
  pure (body <> settle (nativeStatus final), foldMap commands statements, nativeUsesWriter final)
  where
    commands (P.Statement _ node) = case node of
      P.Invoke (P.External name) _ -> S.singleton name
      P.Sequence body -> foldMap commands body
      P.AssignmentCommand _ body -> foldMap commands body
      P.And left right -> commands left <> commands right
      P.Or left right -> commands left <> commands right
      P.Negate value -> commands value
      P.Conditional predicate yes no -> foldMap commands (predicate <> yes <> no)
      P.DefineFunction _ body -> foldMap commands body
      P.Pipeline children -> foldMap (foldMap commands . P.childStatements) children
      _ -> mempty

settle :: Status -> [FishStatement]
settle FishStatus = []
settle ZeroStatus = [builtin "true" []]

finish :: Native [FishStatement] -> Native [FishStatement]
finish action = do
  body <- action
  final <- getStatus
  putStatus FishStatus
  pure (body <> settle final)

lowerStatements :: [P.Statement] -> Native [FishStatement]
lowerStatements = fmap concat . traverse lowerStatement

lowerStatement :: P.Statement -> Native [FishStatement]
lowerStatement (P.Statement range node) = case node of
  P.Sequence body -> lowerStatements body
  P.AssignmentCommand _ body -> do
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
  P.SetArguments values -> do
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
                 P.Function name -> Stmt (Command name arguments)
             ]
      )
  P.Pipeline children -> do
    stages <- traverse stage children
    putStatus FishStatus
    let leading :| rest = stages
    pure [Stmt (Pipeline (MkFishJobPipeline False [] leading (map (PipeTo []) rest) False))]
  P.And left right -> conjunction JCAnd left right
  P.Or left right -> conjunction JCOr left right
  P.Negate value -> do
    body <- finish (lowerStatement value)
    pure [Stmt (Not (Begin (nonempty body) []))]
  P.Conditional predicate yes no -> do
    predicateBody <- finish (lowerStatements predicate)
    yesBody <- finish (lowerStatements yes)
    putStatus FishStatus
    noBody <- finish (lowerStatements no)
    pure [Stmt (If (condition predicateBody) (nonempty yesBody) noBody [])]
  P.DefineFunction name body -> do
    putStatus FishStatus
    bodyValue <- finish (lowerStatements body)
    putStatus ZeroStatus
    prefix <- gets nativePrefix
    usesExec <- gets nativeUsesExec
    pure [Stmt (Function (MkFishFunction name [FuncCaptureVariable (NativeRuntime.runtimePathName prefix) | usesExec] [] (nonempty bodyValue)))]
  P.Return value -> do
    expression <- maybe status scalar value
    pure [builtin "return" [ExprVal expression]]
  P.Exit value -> do
    expression <- maybe status scalar value
    pure [builtin "exit" [ExprVal expression]]
  _ -> lift Nothing

-- The native pipeline proof has no shell state writes, option changes or
-- effectful expansions. Bash's default pipeline status is the final stage.
stage :: P.ChildRegion -> Native FishStatement
stage region = case concatMap flatten (P.childStatements region) of
  [P.Statement range (P.Invoke (P.External name) values)] -> do
    arguments <- concat <$> traverse word values
    prefix <- gets nativePrefix
    pure (externalSite prefix range name arguments)
  _ -> lift Nothing
  where
    flatten (P.Statement _ (P.Sequence body)) = concatMap flatten body
    flatten value = [value]

assignment :: P.Storage -> Text -> P.Scalar -> Native [FishStatement]
assignment storage name value = do
  ordinaryStorage storage
  guard (name /= "IFS")
  expression <- scalar value
  pure [setValue [SetGlobal, SetUnpath] name expression]

ordinaryStorage :: P.Storage -> Native ()
ordinaryStorage P.Global = pure ()
ordinaryStorage P.Visible = pure ()
ordinaryStorage _ = lift Nothing

conjunction :: (FishJobPipeline -> FishJobConjCont) -> P.Statement -> P.Statement -> Native [FishStatement]
conjunction connective left right = do
  leftBody <- finish (lowerStatement left)
  rightBody <- finish (lowerStatement right)
  pure [Stmt (JobConj (MkFishJobConjunction Nothing (job leftBody) [connective (job rightBody)]))]

word :: P.Word -> Native [ExprOrRedirect]
word (P.OneField value) = (: []) . ExprVal <$> scalar value
word (P.QuotedArguments (P.Literal "") (P.Literal "") False) = pure [ExprVal (ExprVariable (VarAll "argv"))]
word _ = lift Nothing

scalar :: P.Scalar -> Native (FishExpr TStr)
scalar = \case
  P.Literal value -> pure (ExprLiteral value)
  P.Variable name | name /= "IFS" -> pure (variable name)
  P.Positional index -> pure (ExprQuotedVariable (VarIndex "argv" (IndexSingle (ExprNumLiteral index))))
  P.ArgumentCount -> pure (ExprQuotedCommandSubst (builtin "count" [ExprVal (ExprVariable (VarAll "argv"))] :| []))
  P.LastStatus -> status
  P.Concat values -> foldr ExprStringConcat (ExprLiteral "") <$> traverse scalar values
  _ -> lift Nothing

status :: Native (FishExpr TStr)
status = gets ((\case FishStatus -> variable "status"; ZeroStatus -> ExprLiteral "0") . nativeStatus)

externalSite :: Text -> Maybe SourceRange -> Text -> [ExprOrRedirect] -> FishStatement
externalSite prefix range name arguments =
  let origin = maybe "<input>" (srcFile . rangeStart) range
      line = maybe "1" (show . srcLine . rangeStart) range
   in Stmt (Decorated DecCommand (CommandExpr (variable (NativeRuntime.runtimePathName prefix)) (map literal ["--abi", "2", "exec-site", origin, line, name] <> arguments)))

variable :: Text -> FishExpr TStr
variable = ExprQuotedVariable . VarScalar

literal :: Text -> ExprOrRedirect
literal = ExprVal . ExprLiteral

builtin :: Text -> [ExprOrRedirect] -> FishStatement
builtin name arguments = Stmt (Decorated DecBuiltin (Command name arguments))

setValue :: [SetFlag] -> Text -> FishExpr TStr -> FishStatement
setValue flags name value = Stmt (Decorated DecBuiltin (Set flags name (ExprListLiteral [value])))

nonempty :: [FishStatement] -> NonEmpty FishStatement
nonempty = fromMaybe (builtin "true" [] :| []) . NE.nonEmpty

job :: [FishStatement] -> FishJobPipeline
job body = MkFishJobPipeline False [] (case body of [single] -> single; _ -> Stmt (Begin (nonempty body) [])) [] False

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

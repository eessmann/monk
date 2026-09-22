{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | Structural ownership of an installed or immutable-generation runtime.
module Language.Fish.Translator.NativeRuntime
  ( runtimeHelperName,
    runtimePathName,
    nativeRuntimeDefinition,
    nativeRuntimeSetup,
    entryDirectory,
    nativeWriterName,
    nativeWriterDefinition,
    nativeWriterInvocation,
  )
where

import Data.Set qualified as S
import Language.Fish.DSL.Internal
import Monk.Translation.Types

runtimeHelperName :: Text -> Text
runtimeHelperName prefix = prefix <> "native"

runtimePathName :: Text -> Text
runtimePathName prefix = prefix <> "native_path"

-- | The wrapper keeps compiler-owned exports out of the primitive environment.
-- The executable itself is a scalar capability, not a parsed command string.
nativeRuntimeDefinition :: Text -> Set Text -> FishStatement
nativeRuntimeDefinition prefix bindings =
  Stmt
    ( Function
        ( MkFishFunction
            (runtimeHelperName prefix)
            [FuncUnknownFlag "--no-scope-shadowing"]
            []
            (fromMaybe (invoke :| []) (nonEmpty (shadows <> [invoke])))
        )
    )
  where
    shadows = [Stmt (Decorated DecBuiltin (Set [SetLocal, SetUnexport, SetUnpath] name (ExprVariable (VarAll name)))) | name <- S.toAscList (S.delete "PWD" bindings)]
    invoke = Stmt (Decorated DecCommand (CommandExpr (scalar (runtimePathName prefix)) [arg (ExprVariable (VarAll "argv"))]))

-- | One source-output boundary preserves errno diagnostics and real SIGPIPE.
-- The status slot belongs to this helper, not to each scalar operand or call.
nativeWriterName :: Text -> Text
nativeWriterName prefix = prefix <> "write"

nativeWriterDefinition :: Text -> FishStatement
nativeWriterDefinition prefix =
  Stmt (Function (MkFishFunction (nativeWriterName prefix) [FuncCaptureVariable pathName] [] (producer :| [save, terminate, builtin "return" [arg (scalar resultName)]])))
  where
    pathName = runtimePathName prefix
    resultName = prefix <> "writer_status"
    producer = Stmt (Pipeline (MkFishJobPipeline False [] (builtin "printf" [lit "%s\\0", arg (ExprVariable (VarAll "argv"))]) [PipeTo [] (Stmt (Decorated DecCommand (CommandExpr (scalar pathName) [lit "--abi", lit "2", lit "write-builtin"])))] False))
    save = Stmt (Decorated DecBuiltin (Set [SetLocal] resultName (ExprListLiteral [scalar "status"])))
    terminate = choose (builtin "test" [arg (scalar resultName), lit "=", lit "141"]) [Stmt (Exec (scalar pathName) [lit "--abi", lit "2", lit "raise-signal", lit "13"])] []

nativeWriterInvocation :: Text -> Maybe SourceRange -> Text -> [ExprOrRedirect] -> FishStatement
nativeWriterInvocation prefix range name arguments =
  let origin = maybe "<input>" (srcFile . rangeStart) range
      line = maybe "1" (show . srcLine . rangeStart) range
   in Stmt (Command (nativeWriterName prefix) (map lit [origin, line, name] <> arguments))

-- | Checks run inside the owned source body, or before standalone body effects.
-- A captured capability is checked again on a deferred exported invocation.
nativeRuntimeSetup :: TranslateConfig -> Text -> Set NativeOperation -> [FishStatement]
nativeRuntimeSetup cfg prefix requiredOperations
  | S.null operations = []
  | otherwise =
      [ set "fish_read_limit" (ExprLiteral "0"),
        set pathName (scalar pathName),
        choose (builtin "test" [lit "-n", arg (scalar pathName)]) [] [Stmt (Decorated DecBuiltin (Set [] pathName (ExprListLiteral [selected])))],
        Stmt (Decorated DecBuiltin (Set [SetLocal] pathName resolvedPath)),
        Stmt (Decorated DecBuiltin (Set [SetLocal] description (ExprCommandSubst (describe :| [])))),
        choose (builtin "test" [arg (scalar "status"), lit "=", lit "0"]) [] failure,
        choose (builtin "test" [arg (member description 1), lit "=", lit "monk-runtime 2 bash53-i64"]) [] failure,
        choose (builtin "contains" [lit "--", arg (member description 3), lit "target x86_64-linux", lit "target aarch64-linux", lit "target aarch64-darwin"]) [] failure,
        Stmt (Decorated DecBuiltin (Set [SetLocal] capabilities (ExprCommandSubst (builtin "string" [lit "split", lit " ", lit "--", arg (member description 2)] :| []))))
      ]
        <> [choose (builtin "contains" [lit "--", lit (nativeOperationName operation), arg (ExprVariable (VarAll capabilities))]) [] failure | operation <- S.toAscList operations]
        <> [choose (Stmt (Decorated DecCommand (CommandExpr (scalar pathName) [lit "--abi", lit "2", lit "pipe-paths", RedirectVal (MkRedirect RedirectStdout RedirectOut (RedirectFile (ExprLiteral "/dev/null"))), RedirectVal (MkRedirect (RedirectFD 2) RedirectOut (RedirectFile (ExprLiteral "/dev/null")))]))) [] failure | S.member NativePipePaths operations]
  where
    operations = S.delete NativeLaunch requiredOperations
    pathName = runtimePathName prefix
    description = prefix <> "native_description"
    capabilities = prefix <> "native_capabilities"
    resolvedPath = ExprCommandSubst (Stmt (Pipeline (MkFishJobPipeline False [] (builtin "path" [lit "resolve", lit "--null-out", lit "--", arg (scalar pathName)]) [PipeTo [] (builtin "string" [lit "split0"])] False)) :| [])
    selected = case translationRuntime cfg of
      RuntimeOnPath -> capture (builtin "command" [lit "--search", lit "--", lit "monk-runtime"])
      RuntimePath path -> ExprLiteral (toText path)
      RuntimeGeneration relative -> ExprStringConcat (entryDirectory prefix) (ExprLiteral ("/" <> toText relative))
    describe = Stmt (Decorated DecCommand (CommandExpr (scalar pathName) [lit "--describe", RedirectVal (MkRedirect RedirectStdin RedirectIn (RedirectFile (ExprLiteral "/dev/null"))), RedirectVal (MkRedirect (RedirectFD 2) RedirectOut (RedirectFile (ExprLiteral "/dev/null")))]))
    failure =
      [ builtin "printf" [lit "%s\n", lit "monk.runtime: missing or incompatible native runtime", RedirectVal (MkRedirect RedirectStdout RedirectOut (RedirectTargetFD 2))],
        builtin (if entryMode cfg == Sourceable then "return" else "exit") [lit "125"]
      ]

-- | A launcher wrapper retains the original entry's immutable artifact base.
-- Nested source members use their own filename, not the outer launch path.
-- The trailing slash-dot keeps directory newlines inside a quoted capture.
entryDirectory :: Text -> FishExpr TStr
entryDirectory prefix = ExprQuotedCommandSubst (filename :| [original, directory, result])
  where
    fileName = prefix <> "entry_filename"
    directoryName = prefix <> "entry_directory"
    filename = set fileName (capture (builtin "status" [lit "current-filename"]))
    original = choose (builtin "test" [arg (scalar fileName), lit "=", arg (scalar "MONK_LAUNCH_WRAPPER"), lit "-a", lit "-n", arg (scalar "MONK_LAUNCH_ORIGINAL")]) [Stmt (Decorated DecBuiltin (Set [] fileName (ExprListLiteral [scalar "MONK_LAUNCH_ORIGINAL"])))] []
    directory = Stmt (Decorated DecBuiltin (Set [SetLocal] directoryName (ExprCommandSubst (Stmt (Pipeline (MkFishJobPipeline False [] (builtin "path" [lit "dirname", lit "--null-out", lit "--", arg (scalar fileName)]) [PipeTo [] (builtin "string" [lit "split0"])] False)) :| []))))
    result = builtin "printf" [lit "%s/.", arg (scalar directoryName)]

arg :: (Typeable t) => FishExpr t -> ExprOrRedirect
arg = ExprVal

lit :: Text -> ExprOrRedirect
lit = arg . ExprLiteral

scalar :: Text -> FishExpr TStr
scalar = ExprQuotedVariable . VarScalar

member :: Text -> Int -> FishExpr TStr
member name index = ExprQuotedVariable (VarIndex name (IndexSingle (ExprNumLiteral index)))

builtin :: Text -> [ExprOrRedirect] -> FishStatement
builtin name = Stmt . Decorated DecBuiltin . Command name

set :: Text -> FishExpr TStr -> FishStatement
set name value = Stmt (Decorated DecBuiltin (Set [SetLocal] name (ExprListLiteral [value])))

capture :: FishStatement -> FishExpr TStr
capture statement = ExprQuotedCommandSubst (statement :| [])

choose :: FishStatement -> [FishStatement] -> [FishStatement] -> FishStatement
choose predicate yes no =
  Stmt (If (MkFishJobList (MkFishJobConjunction Nothing (MkFishJobPipeline False [] predicate [] False) [] :| [])) (fromMaybe (builtin "true" [] :| []) (nonEmpty yes)) no [])

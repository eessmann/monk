{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | Structural ownership of an installed or immutable-generation runtime.
module Language.Fish.Translator.NativeRuntime
  ( runtimeHelperName,
    runtimePathName,
    nativeRuntimeDefinition,
    nativeRuntimeSetup,
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

-- | Checks run inside the owned source body, or before standalone body effects.
-- A captured capability is checked again on a deferred exported invocation.
nativeRuntimeSetup :: TranslateConfig -> Text -> Set NativeOperation -> [FishStatement]
nativeRuntimeSetup cfg prefix operations
  | S.null operations = []
  | otherwise =
      [ set pathName (scalar pathName),
        choose (builtin "test" [lit "-n", arg (scalar pathName)]) [] [Stmt (Decorated DecBuiltin (Set [] pathName (ExprListLiteral [selected])))],
        Stmt (Decorated DecBuiltin (Set [SetLocal] pathName resolvedPath)),
        Stmt (Decorated DecBuiltin (Set [SetLocal] description (ExprCommandSubst (describe :| [])))),
        choose (builtin "test" [arg (scalar "status"), lit "=", lit "0"]) [] failure,
        choose (builtin "test" [arg (member description 1), lit "=", lit "monk-runtime 1 bash53-i64-linux64"]) [] failure,
        Stmt (Decorated DecBuiltin (Set [SetLocal] capabilities (ExprCommandSubst (builtin "string" [lit "split", lit " ", lit "--", arg (member description 2)] :| []))))
      ]
        <> [choose (builtin "contains" [lit "--", lit (nativeOperationName operation), arg (ExprVariable (VarAll capabilities))]) [] failure | operation <- S.toAscList operations]
  where
    pathName = runtimePathName prefix
    description = prefix <> "native_description"
    capabilities = prefix <> "native_capabilities"
    resolvedPath = ExprCommandSubst (Stmt (Pipeline (MkFishJobPipeline False [] (builtin "path" [lit "resolve", lit "--null-out", lit "--", arg (scalar pathName)]) [PipeTo [] (builtin "string" [lit "split0"])] False)) :| [])
    selected = case translationRuntime cfg of
      RuntimeOnPath -> capture (builtin "command" [lit "--search", lit "--", lit "monk-runtime"])
      RuntimePath path -> ExprLiteral (toText path)
      RuntimeGeneration relative -> ExprFileRelative (toText relative)
    describe = Stmt (Decorated DecCommand (CommandExpr (scalar pathName) [lit "--describe", RedirectVal (MkRedirect RedirectStdin RedirectIn (RedirectFile (ExprLiteral "/dev/null"))), RedirectVal (MkRedirect (RedirectFD 2) RedirectOut (RedirectFile (ExprLiteral "/dev/null")))]))
    failure =
      [ builtin "printf" [lit "%s\n", lit "monk.runtime: missing or incompatible native runtime", RedirectVal (MkRedirect RedirectStdout RedirectOut (RedirectTargetFD 2))],
        builtin (if entryMode cfg == Sourceable then "return" else "exit") [lit "125"]
      ]

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

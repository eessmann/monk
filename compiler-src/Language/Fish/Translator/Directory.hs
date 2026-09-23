{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | Directory effects execute in the parent Fish process. Only actual cd
-- diagnostics and physical getcwd/stack display cross the native boundary.
module Language.Fish.Translator.Directory (directoryStatements, directorySetup) where

import Language.Bash.Plan qualified as P
import Language.Bash.Plan.Directory
import Language.Fish.DSL.Internal
import Language.Fish.Translator.Binding qualified as Binding
import Language.Fish.Translator.Identifier (compilerCommandName, compilerIdentifier)
import Language.Fish.Translator.NativeRuntime qualified as NativeRuntime
import Language.Fish.Translator.Session qualified as Session
import Language.Fish.Translator.Session.Request qualified as Request
import Monk.Translation.Types

-- | Unique temporary prefix, immutable runtime function, semantic status slot,
-- source origin/line and the admitted operation.
directoryStatements :: Bool -> EntryMode -> Text -> Text -> Text -> Text -> Text -> DirectoryOperation -> [FishStatement]
directoryStatements supervised mode prefix runtimePrefix status origin line operation = case operation of
  PrintDirectory False -> [emit Request.Pwd Request.StandardOutput (ExprStringConcat (scalar "PWD") (ExprLiteral "\n")), saveStatus]
  PrintDirectory True -> [relay Request.Pwd Request.StandardOutput (native "directory-physical" []), saveStatus]
  ChangeDirectory path -> change "cd" (ExprLiteral path) []
  ChangePreviousDirectory -> change "cd" (scalar "OLDPWD") [emit Request.Cd Request.StandardOutput (ExprStringConcat (scalar "PWD") (ExprLiteral "\n")), saveStatus]
  PushDirectory path -> change "pushd" (ExprLiteral path) [setList [SetGlobal] "dirstack" (ExprListConcat (ExprListLiteral [scalar previous]) (ExprVariable (VarAll "dirstack"))), displayStack Request.Pushd, saveStatus]
  PopDirectory ->
    [ choose
        (builtin "set" [lit "--query", lit "dirstack[1]"])
        (change "popd" (index "dirstack" 1) [setList [SetGlobal] "dirstack" (ExprVariable (VarIndex "dirstack" (IndexRange (Just (ExprNumLiteral 2)) Nothing))), displayStack Request.Popd, saveStatus])
        [emit Request.Popd Request.StandardError (ExprLiteral (origin <> ": line " <> line <> ": popd: directory stack empty\n")), saveStatus, choose (builtin "test" [val (scalar status), lit "=", lit "141"]) [] [set [] status (ExprLiteral "1")]]
    ]
  where
    runtime = runtimePrefix <> "native"
    previous = prefix <> "previous"
    statuses = prefix <> "statuses"
    target = prefix <> "target"
    saveStatus = set [] status (scalar "status")
    native name arguments
      | name == "session-directory-diagnostic" = Stmt (Decorated DecCommand (CommandExpr (variableExecutable (compilerIdentifier (NativeRuntime.runtimePathName runtimePrefix))) (map lit ["--abi", "2", name] <> arguments)))
      | otherwise = Stmt (Command (compilerCommandName runtime) (map lit ["--abi", "2", name] <> arguments))
    displayStack name =
      relay name Request.StandardOutput $
        Stmt (Pipeline (MkFishJobPipeline False [] (builtin "printf" [lit "%s\\0", val (scalar "PWD"), val (scalar "HOME"), val (ExprVariable (VarAll "dirstack"))]) [PipeTo [] (native "directory-stack" [])]))
    emit name descriptor bytes
      | supervised = Session.request runtimePrefix (Request.Run (Request.singleBody (Request.DirectoryStage (Request.Site (ExprLiteral origin) (ExprLiteral line)) name descriptor bytes)))
      | otherwise = builtin "printf" ([lit "%s", val bytes] <> [toError | descriptor == Request.StandardError])
    -- Finite physical-path/stack output preserves trailing newlines through
    -- NUL framing before the owner writes to the active descriptor table.
    -- Parent cd diagnostics use a separate external client to preserve cwd.
    relay name descriptor statement
      | not supervised = statement
      | otherwise =
          Stmt (Begin (definition :| [Stmt (Command (compilerCommandName helper) [])]) [])
      where
        helper = prefix <> "deliver"
        packet = prefix <> "output"
        code = prefix <> "output_status"
        definition =
          Stmt
            ( Function
                ( MkFishFunction
                    helper
                    [FuncUnknownFlag "--no-scope-shadowing"]
                    []
                    ( set [SetLocal] "fish_read_limit" (ExprLiteral "0")
                        :| [ setList [SetLocal] packet captured,
                             choose (builtin "test" [val (index packet 2), lit "=", lit "0"]) [] [Stmt (ReturnScalar (index packet 2))],
                             choose
                               (builtin "test" [val (index packet 1), lit "!=", lit ""])
                               [emit name descriptor (index packet 1), Stmt (ReturnScalar (scalar "status"))]
                               [],
                             Stmt (ReturnScalar (ExprLiteral "0"))
                           ]
                    )
                )
            )
        producer = Stmt (Begin (statement :| [set [SetLocal] code (scalar "status"), builtin "printf" [lit "\\0%s\\0", val (scalar code)]]) [])
        captured = ExprCommandSubst (Stmt (Pipeline (MkFishJobPipeline False [] producer [PipeTo [] (builtin "string" [lit "split0"])])) :| [])
    pathBound = Stmt (Pipeline (MkFishJobPipeline False [] (builtin "printf" [lit "%s\\0", val (scalar "PWD"), val (scalar target)]) [PipeTo [] (native "directory-path-bound" [])]))
    boundFailure = [builtin "printf" [lit "%s\n", lit "monk: stable directory contract failed: resolved logical directory path exceeds the platform limit", toError], if mode == Sourceable then Stmt (ReturnScalar (ExprLiteral "125")) else builtin "exit" [lit "125"]]
    change command operand after =
      [ set [SetLocal] previous (scalar "PWD"),
        set [SetLocal] target operand,
        choose pathBound transition boundFailure
      ]
      where
        transition =
          [ Stmt
              ( Pipeline
                  ( MkFishJobPipeline
                      False
                      []
                      (Stmt (Begin (builtin "printf" [lit "%s\\0", lit origin, lit line, lit command, val (scalar target), toError] :| [builtin "cd" [lit "--", val (scalar target)]]) []))
                      [PipeErrorTo [] (native (if supervised then "session-directory-diagnostic" else "directory-diagnostic") [])]
                  )
              ),
            setList [SetLocal] statuses (ExprVariable (VarAll "pipestatus")),
            set [] status (index statuses 1),
            choose (builtin "test" [val (index statuses 2), lit "=", lit "0"]) [] [set [] status (index statuses 2)],
            choose
              (builtin "test" [val (scalar status), lit "=", lit "0"])
              (Binding.writeBinding (Binding.bindingRuntime runtimePrefix "OLDPWD") P.Global (scalar previous) <> after)
              []
          ]

-- | Observable preconditions are checked before effects. Ancestry stability is
-- the declared obligation, including external commands and imported functions.
directorySetup :: TranslateConfig -> Text -> [FishStatement]
directorySetup cfg runtimePrefix
  | not (stableDirectoryEnabled cfg) = []
  | otherwise =
      [ require (builtin "test" [val (capture (builtin "count" [val (ExprVariable (VarAll "CDPATH"))])), lit "-le", lit "1"]) "CDPATH must be absent or one empty scalar",
        require (builtin "test" [val (capture (builtin "string" [lit "join", lit "", lit "--", val (ExprVariable (VarAll "CDPATH"))])), lit "=", lit ""]) "CDPATH must be empty",
        require (builtin "set" [lit "--query", lit "--global", lit "--export", lit "PWD"]) "PWD must be an exported global",
        require (builtin "test" [val (capture (builtin "count" [val (ExprVariable (VarAll "PWD"))])), lit "=", lit "1"]) "PWD must be scalar"
      ]
        <> concatMap ordinary ["PWD", "OLDPWD", "dirstack"]
        <> [ choose (builtin "set" [lit "--query", lit "OLDPWD"]) [require (builtin "test" [val (capture (builtin "count" [val (ExprVariable (VarAll "OLDPWD"))])), lit "=", lit "1"]) "OLDPWD must be scalar"] [],
             choose (builtin "set" [lit "--query", lit "--export", lit "dirstack"]) (failure "dirstack must be unexported") []
           ]
        <> [require (Stmt (Pipeline (MkFishJobPipeline False [] (builtin "printf" [lit "%s\\0", val (scalar "PWD"), val (ExprVariable (VarAll "dirstack"))]) [PipeTo [] (Stmt (Command (compilerCommandName runtime) (map lit ["--abi", "2", "directory-validate"])))]))) "PWD or stack paths are not valid ordinary absolute directories"]
        <> [setList [SetGlobal, SetUnexport] "dirstack" (ExprListLiteral []) | entryMode cfg == Standalone]
        <> [choose initialOldpwd [] (Binding.eraseBinding oldpwdBinding <> Binding.declareExport oldpwdBinding P.Global Nothing) | entryMode cfg == Standalone]
  where
    runtime = runtimePrefix <> "native"
    oldpwdBinding = Binding.bindingRuntime runtimePrefix "OLDPWD"
    initialOldpwd = Stmt (Pipeline (MkFishJobPipeline False [] (builtin "printf" [lit "%s\\0", val (scalar "OLDPWD")]) [PipeTo [] (Stmt (Command (compilerCommandName runtime) (map lit ["--abi", "2", "directory-initial-oldpwd"])))]))
    failure message = [builtin "printf" [lit "%s\n", lit ("monk: stable directory contract failed: " <> message), toError], if entryMode cfg == Sourceable then Stmt (ReturnScalar (ExprLiteral "125")) else builtin "exit" [lit "125"]]
    require predicate message = choose predicate [] (failure message)
    ordinary name = [choose (builtin "set" (map lit ["--query", flag, name])) (failure ("unsupported " <> flag <> " binding " <> name)) [] | flag <- ["--universal", "--path", "--local"]]

val :: (Typeable t) => FishExpr t -> ExprOrRedirect
val = ExprVal

lit :: Text -> ExprOrRedirect
lit = val . ExprLiteral

scalar :: Text -> FishExpr TStr
scalar = ExprQuotedVariable . VarScalar . compilerIdentifier

index :: Text -> Int -> FishExpr TStr
index name n = ExprQuotedVariable (VarIndex (compilerIdentifier name) (IndexSingle (ExprNumLiteral n)))

builtin :: Text -> [ExprOrRedirect] -> FishStatement
builtin name = Stmt . Decorated DecBuiltin . Command (compilerCommandName name)

set :: [SetFlag] -> Text -> FishExpr TStr -> FishStatement
set flags name value = setList flags name (ExprListLiteral [value])

setList :: [SetFlag] -> Text -> FishExpr (TList TStr) -> FishStatement
setList flags name = Stmt . Decorated DecBuiltin . Set flags (compilerIdentifier name)

toError :: ExprOrRedirect
toError = RedirectVal (DuplicateRedirect 1 WriteTo 2)

capture :: FishStatement -> FishExpr TStr
capture statement = ExprQuotedCommandSubst (statement :| [])

choose :: FishStatement -> [FishStatement] -> [FishStatement] -> FishStatement
choose predicate yes no = Stmt (If (MkFishJobList (MkFishJobConjunction Nothing (MkFishJobPipeline False [] predicate []) [] :| [])) (fromMaybe (builtin "true" [] :| []) (nonEmpty yes)) no [])

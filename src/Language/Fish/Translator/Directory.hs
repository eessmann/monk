{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | Directory effects execute in the parent Fish process. Only actual cd
-- diagnostics and physical getcwd/stack display cross the native boundary.
module Language.Fish.Translator.Directory (directoryStatements, directorySetup) where

import Language.Bash.Plan qualified as P
import Language.Bash.Plan.Directory
import Language.Fish.DSL.Internal
import Language.Fish.Translator.Binding qualified as Binding
import Monk.Translation.Types

-- | Unique temporary prefix, immutable runtime function, semantic status slot,
-- source origin/line and the admitted operation.
directoryStatements :: EntryMode -> Text -> Text -> Text -> Text -> Text -> DirectoryOperation -> [FishStatement]
directoryStatements mode prefix runtimePrefix status origin line operation = case operation of
  PrintDirectory False -> [builtin "printf" [lit "%s\n", val (scalar "PWD")], saveStatus]
  PrintDirectory True -> [native "directory-physical" [], saveStatus]
  ChangeDirectory path -> change "cd" (ExprLiteral path) []
  ChangePreviousDirectory -> change "cd" (scalar "OLDPWD") [builtin "printf" [lit "%s\n", val (scalar "PWD")]]
  PushDirectory path -> change "pushd" (ExprLiteral path) [setList [SetGlobal] "dirstack" (ExprListConcat (ExprListLiteral [scalar previous]) (ExprVariable (VarAll "dirstack"))), displayStack]
  PopDirectory ->
    [ choose
        (builtin "set" [lit "--query", lit "dirstack[1]"])
        (change "popd" (index "dirstack" 1) [setList [SetGlobal] "dirstack" (ExprVariable (VarIndex "dirstack" (IndexRange (Just (ExprNumLiteral 2)) Nothing))), displayStack])
        [builtin "printf" [lit "%s\n", lit (origin <> ": line " <> line <> ": popd: directory stack empty"), toError], set [] status (ExprLiteral "1")]
    ]
  where
    runtime = runtimePrefix <> "native"
    previous = prefix <> "previous"
    statuses = prefix <> "statuses"
    target = prefix <> "target"
    saveStatus = set [] status (scalar "status")
    native name arguments = Stmt (Command runtime (map lit ["--abi", "1", name] <> arguments))
    displayStack =
      Stmt (Pipeline (MkFishJobPipeline False [] (builtin "printf" [lit "%s\\0", val (scalar "PWD"), val (scalar "HOME"), val (ExprVariable (VarAll "dirstack"))]) [PipeTo [] (native "directory-stack" [])] False))
    pathBound = Stmt (Pipeline (MkFishJobPipeline False [] (builtin "printf" [lit "%s\\0", val (scalar "PWD"), val (scalar target)]) [PipeTo [] (native "directory-path-bound" [])] False))
    boundFailure = [builtin "printf" [lit "%s\n", lit "monk: stable directory contract failed: resolved logical directory path exceeds 4095 bytes", toError], builtin (if mode == Sourceable then "return" else "exit") [lit "125"]]
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
                      [PipeErrorTo [] (native "directory-diagnostic" [])]
                      False
                  )
              ),
            setList [SetLocal] statuses (ExprVariable (VarAll "pipestatus")),
            set [] status (index statuses 1),
            choose (builtin "test" [val (index statuses 2), lit "=", lit "0"]) [] [set [] status (ExprLiteral "125")],
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
        <> [require (Stmt (Pipeline (MkFishJobPipeline False [] (builtin "printf" [lit "%s\\0", val (scalar "PWD"), val (ExprVariable (VarAll "dirstack"))]) [PipeTo [] (Stmt (Command runtime (map lit ["--abi", "1", "directory-validate"])))] False))) "PWD or stack paths are not valid ordinary absolute directories"]
        <> [setList [SetGlobal, SetUnexport] "dirstack" (ExprListLiteral []) | entryMode cfg == Standalone]
        <> [choose initialOldpwd [] (Binding.eraseBinding oldpwdBinding <> Binding.declareExport oldpwdBinding P.Global Nothing) | entryMode cfg == Standalone]
  where
    runtime = runtimePrefix <> "native"
    oldpwdBinding = Binding.bindingRuntime runtimePrefix "OLDPWD"
    initialOldpwd = Stmt (Pipeline (MkFishJobPipeline False [] (builtin "printf" [lit "%s\\0", val (scalar "OLDPWD")]) [PipeTo [] (Stmt (Command runtime (map lit ["--abi", "1", "directory-initial-oldpwd"])))] False))
    failure message = [builtin "printf" [lit "%s\n", lit ("monk: stable directory contract failed: " <> message), toError], builtin (if entryMode cfg == Sourceable then "return" else "exit") [lit "125"]]
    require predicate message = choose predicate [] (failure message)
    ordinary name = [choose (builtin "set" (map lit ["--query", flag, name])) (failure ("unsupported " <> flag <> " binding " <> name)) [] | flag <- ["--universal", "--path", "--local"]]

val :: (Typeable t) => FishExpr t -> ExprOrRedirect
val = ExprVal

lit :: Text -> ExprOrRedirect
lit = val . ExprLiteral

scalar :: Text -> FishExpr TStr
scalar = ExprQuotedVariable . VarScalar

index :: Text -> Int -> FishExpr TStr
index name n = ExprQuotedVariable (VarIndex name (IndexSingle (ExprNumLiteral n)))

builtin :: Text -> [ExprOrRedirect] -> FishStatement
builtin name = Stmt . Decorated DecBuiltin . Command name

set :: [SetFlag] -> Text -> FishExpr TStr -> FishStatement
set flags name value = setList flags name (ExprListLiteral [value])

setList :: [SetFlag] -> Text -> FishExpr (TList TStr) -> FishStatement
setList flags name = Stmt . Decorated DecBuiltin . Set flags name

toError :: ExprOrRedirect
toError = RedirectVal (MkRedirect RedirectStdout RedirectOut (RedirectTargetFD 2))

capture :: FishStatement -> FishExpr TStr
capture statement = ExprQuotedCommandSubst (statement :| [])

choose :: FishStatement -> [FishStatement] -> [FishStatement] -> FishStatement
choose predicate yes no = Stmt (If (MkFishJobList (MkFishJobConjunction Nothing (MkFishJobPipeline False [] predicate [] False) [] :| [])) (fromMaybe (builtin "true" [] :| []) (nonEmpty yes)) no [])

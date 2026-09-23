{-# LANGUAGE DataKinds #-}

-- | Structural entry guards for standalone and sourceable artifacts.
module Language.Fish.Translator.Boundary
  ( sourceableEntry,
    helperNames,
    boundaryFailure,
    guardFailure,
    guardWhen,
    guardUnless,
    privateGuardsWithCaptured,
    standaloneGuards,
    standaloneFailure,
    standaloneBindingGuards,
    sessionEnvironmentGuards,
    standaloneArrayGuards,
    callerGuards,
  )
where

import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Language.Fish.DSL.Internal
import Language.Fish.Translator.Directory qualified as Directory
import Language.Fish.Translator.Identifier (compilerCommandName, compilerIdentifier)
import Language.Fish.Translator.NativeRuntime qualified as NativeRuntime
import Language.Fish.Translator.Statement
import Monk.Translation.Types
import Prelude hiding (first)

sourceableEntry :: TranslateConfig -> S.Set NativeOperation -> Text -> Text -> FishExpr TStr -> Text -> [Text] -> [FishStatement] -> [FishStatement] -> [FishStatement]
sourceableEntry cfg nativeOperations prefix identityTag incomingValue wrapper moduleFunctions helpers body =
  [assign [SetLocal] (compilerIdentifier incoming) incomingValue]
    <> privateGuardsWithCaptured guardFailure captured prefix incoming markerGuards
  where
    markerGuards = foldr (\name next -> guardWhen (builtin "set" [arg (ExprLiteral "--query"), arg (ExprLiteral name)]) ("reserved launcher metadata " <> name) next) (callerGuards (callerContract cfg) execution) ["MONK_LAUNCH_ORIGINAL", "MONK_LAUNCH_WRAPPER"]
    captured = [NativeRuntime.runtimePathName prefix | wrapper /= prefix <> "entry" && not (S.null nativeOperations)]
    incoming = prefix <> "incoming"
    result = prefix <> "result"
    finish = prefix <> "finish"
    execution =
      [ Stmt (Function (MkFishFunction wrapper [FuncUnknownFlag "--no-scope-shadowing"] [] (bodyNE bodyExecution))),
        Stmt (Command (compilerCommandName wrapper) [arg (ExprVariable (VarAll "argv"))]),
        assign [SetLocal] (compilerIdentifier result) (scalarVar "status"),
        builtin "functions" (map (arg . ExprLiteral) ("--erase" : wrapper : (helperNames helpers <> moduleFunctions))),
        Stmt
          ( Function
              ( MkFishFunction
                  finish
                  []
                  []
                  ( builtin "functions" [arg (ExprLiteral "--erase"), arg (ExprLiteral finish)]
                      :| [Stmt (ReturnScalar (ExprQuotedVariable (VarIndex "argv" (IndexSingle (ExprNumLiteral 1)))))]
                  )
              )
          ),
        Stmt (Command (compilerCommandName finish) [arg (scalarVar (compilerIdentifier result))])
      ]
    bodyExecution =
      NativeRuntime.nativeRuntimeSetup cfg prefix nativeOperations
        <> [ assign [SetLocal] (compilerIdentifier (prefix <> "active")) (ExprLiteral identityTag),
             assign [SetLocal] (compilerIdentifier (prefix <> "status")) (scalarVar (compilerIdentifier incoming)),
             assign [SetLocal] (compilerIdentifier (prefix <> "errexit")) (ExprLiteral "0"),
             assign [SetLocal] (compilerIdentifier (prefix <> "pipefail")) (ExprLiteral "0"),
             assign [SetLocal] (compilerIdentifier (prefix <> "suppress")) (ExprLiteral "0"),
             assign [SetLocal] (compilerIdentifier (prefix <> "ifs")) (ExprLiteral " \t\n"),
             assign [SetLocal] (compilerIdentifier (prefix <> "substitution_executed")) (ExprLiteral "0"),
             assign [SetLocal] (compilerIdentifier (prefix <> "substitution_status")) (ExprLiteral "0")
           ]
        <> helpers
        <> (if S.member NativeDirectory nativeOperations then Directory.directorySetup cfg prefix else [])
        <> body
        <> [Stmt (ReturnScalar (if null body then ExprLiteral "0" else scalarVar (compilerIdentifier (prefix <> "status"))))]

helperNames :: [FishStatement] -> [Text]
helperNames statements = [funcName function | Stmt (Function function) <- statements]

boundaryFailure :: Text -> [FishStatement]
boundaryFailure message =
  [ builtin "printf" [arg (ExprLiteral "%s\n"), arg (ExprLiteral ("monk: caller contract failed: " <> message)), RedirectVal (DuplicateRedirect 1 WriteTo 2)],
    Stmt (ReturnScalar (ExprLiteral "125"))
  ]

-- A failing source guard reaches EOF with its status. Native `return` here
-- would escape a Fish function which happened to source this file.
guardFailure :: Text -> [FishStatement]
guardFailure message =
  take 1 (boundaryFailure message)
    <> [external "fish" [arg (ExprLiteral "--no-config"), arg (ExprLiteral "-c"), arg (ExprLiteral "exit 125")]]

guardWhen :: FishStatement -> Text -> [FishStatement] -> [FishStatement]
guardWhen predicate message next = [ifStatements [predicate] (guardFailure message) next]

guardUnless :: FishStatement -> Text -> [FishStatement] -> [FishStatement]
guardUnless predicate message next = [ifStatements [predicate] next (guardFailure message)]

privateGuardsWithCaptured :: (Text -> [FishStatement]) -> [Text] -> Text -> Text -> [FishStatement] -> [FishStatement]
privateGuardsWithCaptured failure captured prefix incoming next =
  foldr (\(predicate, message) continuation -> [ifStatements [predicate] (failure message) continuation]) next (privateGuardChecks captured prefix incoming)

privateGuardChecks :: [Text] -> Text -> Text -> [(FishStatement, Text)]
privateGuardChecks captured prefix incoming =
  [ (namesMatch (builtin "functions" [arg (ExprLiteral "--all"), arg (ExprLiteral "--names")]) [], "private function namespace is occupied"),
    ( namesMatch
        (builtin "set" [arg (ExprLiteral "--names")])
        [PipeTo [] (builtin "string" [arg (ExprLiteral "match"), arg (ExprLiteral "--invert"), arg (ExprLiteral "--regex"), arg (ExprLiteral "--"), arg (ExprLiteral ("^(" <> T.intercalate "|" excluded <> ")$"))]) | not (null excluded)],
      "private variable namespace is occupied"
    )
  ]
  where
    excluded = filter (not . T.null) (incoming : captured)
    namesMatch producer middle =
      Stmt
        ( Pipeline
            ( MkFishJobPipeline
                False
                []
                producer
                (middle <> [PipeTo [] (builtin "string" [arg (ExprLiteral "match"), arg (ExprLiteral "--quiet"), arg (ExprLiteral "--"), arg (ExprLiteral (prefix <> "*"))])])
            )
        )

-- Standalone execution admits only represented environment scalars for
-- relevant source bindings. Unrelated interactive state is immaterial.
standaloneGuards :: Text -> S.Set Text -> [FishStatement] -> [FishStatement]
standaloneGuards prefix bindings next =
  [ifStatements [predicate] (standaloneFailure message) [] | (predicate, message) <- privateGuardChecks [] prefix ""]
    <> standaloneBindingGuards bindings
    <> next

standaloneFailure :: Text -> [FishStatement]
standaloneFailure message =
  [ builtin "printf" [arg (ExprLiteral "%s\n"), arg (ExprLiteral ("monk: runtime contract failed: " <> message)), RedirectVal (DuplicateRedirect 1 WriteTo 2)],
    builtin "exit" [arg (ExprLiteral "125")]
  ]

-- These checks can terminate their owned standalone process directly. Keeping
-- their continuation flat makes each boundary obligation visible only once.
standaloneBindingGuards :: S.Set Text -> [FishStatement]
standaloneBindingGuards bindings = concatMap bindingGuard (S.toAscList (S.delete "IFS" bindings))
  where
    rejectWhen predicate message = ifStatements [predicate] (standaloneFailure message) []
    require predicate message = ifStatements [predicate] [] (standaloneFailure message)
    query flags name = builtin "set" (map (arg . ExprLiteral) ("--query" : flags <> [name]))
    bindingGuard name =
      [ rejectWhen (query ["--universal"] name) ("universal binding " <> name),
        rejectWhen (query ["--path"] name) ("path binding " <> name),
        ifStatements [query [] name] (shape name) []
      ]
    shape name =
      let count = ExprQuotedCommandSubst (builtin "count" [arg (ExprVariable (VarAll (compilerIdentifier name)))] :| [])
       in [ require (testEquals count "1") ("non-scalar binding " <> name),
            rejectWhen (query ["--local"] name) ("local binding " <> name),
            require (query ["--export"] name) ("non-environment binding " <> name)
          ]

sessionEnvironmentGuards :: [FishStatement]
sessionEnvironmentGuards =
  [ ifStatements [builtin "set" [arg (ExprLiteral "--query"), arg (ExprLiteral name)]] (standaloneFailure ("private session binding " <> name)) []
  | name <- ["MONK_SESSION_SOCKET", "MONK_SESSION_TOKEN", "MONK_SESSION_REPLY", "MONK_SESSION_FDS"]
  ]

-- Arrays are owned by the translated program. An ambient scalar with the same
-- name carries export attributes which are outside this initial contract.
standaloneArrayGuards :: S.Set Text -> [FishStatement]
standaloneArrayGuards = concatMap checkArray . S.toAscList
  where
    checkArray name = [ifStatements [builtin "set" [arg (ExprLiteral "--query"), arg (ExprLiteral name)]] (standaloneFailure ("preexisting array binding " <> name)) []]

-- Each check has a bounded body and succeeds with status zero. The conjunction
-- stops at the first failed check, preserving its status, and owns one copy of
-- the admitted continuation. Nesting the continuation in optional-binding
-- branches would duplicate the entire remaining program for every binding.
guardSequence :: [[FishStatement]] -> [FishStatement] -> [FishStatement]
guardSequence [] next = next
guardSequence (first : rest) next =
  [ Stmt
      ( JobConj
          ( MkFishJobConjunction
              Nothing
              (jobOf (asCommand first))
              [JCAnd (jobOf (asCommand statements)) | statements <- rest <> [next]]
          )
      )
  ]

callerGuards :: CallerContract -> [FishStatement] -> [FishStatement]
callerGuards contract =
  guardSequence
    ( [variableGuard binding [builtin "true" []] | binding <- M.toList (callerVariables contract)]
        <> [functionGuard imported [builtin "true" []] | imported <- M.elems (callerFunctions contract)]
    )
  where
    query flags name = builtin "set" (map (arg . ExprLiteral) ("--query" : flags <> [name]))
    functionGuard imported =
      guardUnless
        (builtin "functions" [arg (ExprLiteral "--query"), arg (ExprLiteral (functionTarget imported))])
        ("missing imported function " <> functionTarget imported)
    variableGuard (name, ScalarBinding access scope exported) remaining =
      guardWhen (query ["--universal"] name) ("universal binding " <> name) $
        (if scope == GlobalBinding then guardWhen (query ["--local"] name) ("local shadow of global " <> name) else id)
          [ ifStatements
              [query scopeFlags name]
              shape
              (if access == OutputBinding && scope == GlobalBinding && exported == UnexportedBinding then remaining else guardFailure ("missing scalar " <> name))
          ]
      where
        scopeFlags = ["--global" | scope == GlobalBinding]
        cardinality = ExprQuotedCommandSubst (builtin "count" [arg (ExprVariable (VarAll (compilerIdentifier name)))] :| [])
        shape =
          guardUnless (testEquals cardinality "1") ("non-scalar binding " <> name) $
            guardWhen (query (scopeFlags <> ["--path"]) name) ("path binding " <> name) $
              (if exported == ExportedBinding then guardUnless else guardWhen)
                (query (scopeFlags <> ["--export"]) name)
                ("export attribute mismatch for " <> name)
                remaining

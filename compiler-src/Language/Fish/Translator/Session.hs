{-# LANGUAGE DataKinds #-}

-- | Structural control requests to the native session owner. User descriptors
-- are copied before the metadata pipe; replies use a private per-evaluator file.
module Language.Fish.Translator.Session
  ( requestHelperName,
    requestDefinition,
    request,
    endpointPath,
    endpointLease,
    launchSession,
  )
where

import Data.Bits (testBit)
import Language.Fish.DSL.Argument (argumentExpression)
import Language.Fish.DSL.Internal
import Language.Fish.Translator.Identifier (compilerCommandName, compilerIdentifier)
import Language.Fish.Translator.NativeRuntime qualified as NativeRuntime
import Language.Fish.Translator.Session.Request qualified as Request

requestHelperName :: Text -> Text
requestHelperName prefix = prefix <> "session_request"

request :: Text -> Request.Request operation -> FishStatement
request prefix operation = Stmt (Command (compilerCommandName (requestHelperName prefix)) (map argumentExpression (toList (Request.requestArguments operation))))

endpointPath :: Text -> FishExpr TStr
endpointPath prefix = member (prefix <> "session_endpoint") 1

endpointLease :: Text -> FishExpr TStr
endpointLease prefix = member (prefix <> "session_endpoint") 2

requestDefinition :: Text -> FishStatement
requestDefinition prefix =
  Stmt (Function (MkFishFunction (requestHelperName prefix) [FuncUnknownFlag "--no-scope-shadowing"] [] (nonempty body)))
  where
    mask = prefix <> "session_mask"
    packet = prefix <> "session_packet"
    probe = Stmt (Command (compilerCommandName (NativeRuntime.runtimeHelperName prefix)) (map literal ["--abi", "2", "descriptor-state"]))
    input = builtin "printf" [literal "%s\\0", ExprVal (member "argv" 1), ExprVal (variable mask), ExprVal (ExprVariable (VarIndex "argv" (IndexRange (Just (ExprNumLiteral 2)) Nothing)))]
    client = Stmt (Decorated DecCommand (CommandExpr (variableExecutable (compilerIdentifier (NativeRuntime.runtimePathName prefix))) (map literal ["--abi", "2", "session-client", "--reply"])))
    pipeline = Stmt (Pipeline (MkFishJobPipeline False [] input [PipeTo [] client]))
    arm bits = MkCaseItem (ExprLiteral (show bits) :| []) (Stmt (Begin (pipeline :| [choose (builtin "test" [ExprVal (member "pipestatus" (-1)), literal "=", literal "0"]) [] failure]) [DuplicateRedirect (fromIntegral (fd + 3)) (if fd == 0 then ReadFrom else WriteTo) (fromIntegral fd) | fd <- [0 .. 2], testBit bits fd]) :| [])
    capture = ExprCommandSubst (builtin "string" [literal "split0", RedirectVal (FileRedirect 0 InputFile (variable "MONK_SESSION_REPLY"))] :| [])
    body =
      [ probe,
        set [SetLocal] mask (variable "status"),
        Stmt (Switch (variable mask) (arm (0 :: Int) :| map arm [1 .. 7]) []),
        set [SetLocal] "fish_read_limit" (ExprLiteral "0"),
        Stmt (Decorated DecBuiltin (Set [SetLocal] (compilerIdentifier packet) capture)),
        choose (builtin "test" [ExprVal (ExprQuotedCommandSubst (builtin "count" [ExprVal (ExprVariable (VarAll (compilerIdentifier packet)))] :| [])), literal "-ge", literal "2"]) [] failure,
        choose
          (builtin "test" [ExprVal (member "argv" 1), literal "=", literal "read"])
          [Stmt (Decorated DecBuiltin (Set [SetGlobal] (compilerIdentifier (prefix <> "session_fields")) (ExprVariable (VarIndex (compilerIdentifier packet) (IndexRange (Just (ExprNumLiteral 3)) Nothing)))))]
          [ choose
              (builtin "test" [ExprVal (member "argv" 1), literal "=", literal "substitution"])
              [ choose (builtin "test" [ExprVal (ExprQuotedCommandSubst (builtin "count" [ExprVal (ExprVariable (VarAll (compilerIdentifier packet)))] :| [])), literal "=", literal "5"]) [] failure,
                Stmt (Decorated DecBuiltin (Set [SetGlobal] (compilerIdentifier (prefix <> "session_endpoint")) (ExprVariable (VarIndex (compilerIdentifier packet) (IndexRange (Just (ExprNumLiteral 4)) Nothing)))))
              ]
              [choose (builtin "test" [ExprVal (ExprQuotedCommandSubst (builtin "count" [ExprVal (ExprVariable (VarAll (compilerIdentifier packet)))] :| [])), literal "=", literal "3"]) [] failure]
          ],
        choose (builtin "test" [ExprVal (member packet 1), literal "=", literal "ok"]) [] failure,
        choose (builtin "contains" [literal "--", ExprVal (member "argv" 1), literal "spawn", literal "substitution"]) [set [SetGlobal] (prefix <> "last_pid") (member packet 3)] [],
        Stmt (ReturnScalar (member packet 2))
      ]

-- | The launcher transfers compiled Fish bytes, then becomes the native owner.
-- The first evaluator argument pins the already validated executable capability.
launchSession :: Text -> [FishStatement] -> [FishStatement]
launchSession prefix body =
  [ set [SetLocal] "fish_read_limit" (ExprLiteral "0"),
    Stmt (Decorated DecBuiltin (Set [SetLocal] (compilerIdentifier capsule) captured)),
    choose (builtin "test" [ExprVal (ExprQuotedCommandSubst (builtin "count" [ExprVal (ExprVariable (VarAll (compilerIdentifier capsule)))] :| [])), literal "=", literal "4"]) [] failure,
    choose (builtin "test" [ExprVal (member capsule 1), literal "=", literal "ok"]) [] failure,
    Stmt (Exec (variableExecutable (compilerIdentifier path)) (map literal ["--abi", "2", "session-run", "--capsule"] <> [ExprVal (member capsule 3), ExprVal (member capsule 4), ExprVal (variable path), ExprVal (ExprVariable (VarAll "argv"))]))
  ]
  where
    path = NativeRuntime.runtimePathName prefix
    capsule = prefix <> "session_capsule"
    evaluator = MkScript ([set [SetGlobal] path (member "argv" 1), Stmt (Decorated DecBuiltin (Set [] "argv" (ExprVariable (VarIndex "argv" (IndexRange (Just (ExprNumLiteral 2)) Nothing)))))] <> body)
    writer = builtin "printf" [literal "%s\\0", ExprVal (ExprEmbeddedScript evaluator)]
    prepare = Stmt (Decorated DecCommand (CommandExpr (variableExecutable (compilerIdentifier path)) (map literal ["--abi", "2", "session-prepare"])))
    captured = ExprCommandSubst (Stmt (Pipeline (MkFishJobPipeline False [] writer [PipeTo [] prepare, PipeTo [] (builtin "string" [literal "split0"])])) :| [])

failure :: [FishStatement]
failure = [builtin "printf" [literal "%s\n", literal "monk.runtime: session control failed", RedirectVal (DuplicateRedirect 1 WriteTo 2)], builtin "exit" [literal "125"]]

literal :: Text -> ExprOrRedirect
literal = ExprVal . ExprLiteral

variable :: Text -> FishExpr TStr
variable = ExprQuotedVariable . VarScalar . compilerIdentifier

member :: Text -> Int -> FishExpr TStr
member name index = ExprQuotedVariable (VarIndex (compilerIdentifier name) (IndexSingle (ExprNumLiteral index)))

builtin :: Text -> [ExprOrRedirect] -> FishStatement
builtin name = Stmt . Decorated DecBuiltin . Command (compilerCommandName name)

set :: [SetFlag] -> Text -> FishExpr TStr -> FishStatement
set flags name value = Stmt (Decorated DecBuiltin (Set flags (compilerIdentifier name) (ExprListLiteral [value])))

nonempty :: [FishStatement] -> NonEmpty FishStatement
nonempty = fromMaybe (builtin "true" [] :| []) . nonEmpty

choose :: FishStatement -> [FishStatement] -> [FishStatement] -> FishStatement
choose predicate yes no = Stmt (If (MkFishJobList (MkFishJobConjunction Nothing (MkFishJobPipeline False [] predicate []) [] :| [])) (nonempty yes) no [])

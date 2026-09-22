{-# LANGUAGE DataKinds #-}

-- | Compiled standalone callbacks. The evaluator retains live bindings and
-- status ownership; no handler source is interpreted at runtime.
module Language.Fish.Translator.Traps
  ( definitions,
    initialize,
    functionEntry,
    install,
    exitWithStatus,
    exitWithStatusAt,
    errorHook,
  )
where

import Language.Bash.Plan qualified as P
import Language.Fish.DSL.Internal
import Language.Fish.Translator.Session qualified as Session

initialize :: Text -> [FishStatement]
initialize prefix = [set [SetGlobal] (prefix <> name) (ExprLiteral value) | (name, value) <- [("trap_exit", ""), ("trap_err", ""), ("trap_exit_active", "0"), ("trap_err_active", "0"), ("trap_err_enabled", "1"), ("pending_signal", "0")]]

functionEntry :: Text -> [FishStatement]
functionEntry prefix = [set [SetLocal] (prefix <> "trap_err_saved") (variable (prefix <> "trap_err")), set [SetLocal] (prefix <> "trap_err_enabled") (ExprLiteral "0")]

install :: Text -> P.TrapKind -> Maybe Text -> [FishStatement]
install prefix kind handler = case (kind, handler) of
  (P.ExitTrap, _) -> [set [SetGlobal] (prefix <> "trap_exit") (ExprLiteral (fromMaybe "" handler))]
  (P.ErrTrap, Just name) -> [set [SetGlobal] (prefix <> "trap_err") (ExprLiteral name), set [] (prefix <> "trap_err_enabled") (ExprLiteral "1")]
  (P.ErrTrap, Nothing) ->
    [ choose
        (builtin "set" [literal "--query", literal (prefix <> "trap_err_saved")])
        [set [SetGlobal] (prefix <> "trap_err") (variable (prefix <> "trap_err_saved"))]
        [set [SetGlobal] (prefix <> "trap_err") (ExprLiteral "")],
      set [] (prefix <> "trap_err_enabled") (ExprLiteral "0")
    ]

exitWithStatus :: Text -> FishExpr TStr -> FishStatement
exitWithStatus prefix = exitWithStatusAt prefix (variable (prefix <> "source_origin"))

exitWithStatusAt :: Text -> FishExpr TStr -> FishExpr TStr -> FishStatement
exitWithStatusAt prefix origin status = Stmt (Command (prefix <> "trap_exit_run") [ExprVal status, ExprVal origin, ExprVal (ExprVariable (VarAll "argv"))])

errorHook :: Text -> [ExprOrRedirect] -> FishStatement
errorHook prefix location = Stmt (Command (prefix <> "trap_err_run") (location <> [ExprVal (ExprVariable (VarAll "argv"))]))

definitions :: Text -> [FishStatement]
definitions prefix = [function "trap_exit_run" exitBody, function "trap_err_run" errorBody]
  where
    name role = prefix <> role
    sourceStatus = name "status"
    invoke role = Stmt (CommandExpr (variable (name role)) [ExprVal (ExprVariable (VarAll "argv"))])
    function role body = Stmt (Function (MkFishFunction (name role) [FuncUnknownFlag "--no-scope-shadowing"] [] (nonempty body)))
    equal role expected = builtin "test" [ExprVal (variable (name role)), literal "=", literal expected]
    present role = builtin "test" [literal "-n", ExprVal (variable (name role))]
    exitBody =
      [ set [SetLocal] (name "trap_exit_status") (ExprQuotedVariable (VarIndex "argv" (IndexSingle (ExprNumLiteral 1)))),
        set [SetLocal] (name "callback_origin") (ExprQuotedVariable (VarIndex "argv" (IndexSingle (ExprNumLiteral 2)))),
        set [SetLocal] (name "callback_line") (ExprLiteral "1"),
        Stmt (Decorated DecBuiltin (Set [] "argv" (ExprVariable (VarIndex "argv" (IndexRange (Just (ExprNumLiteral 3)) Nothing))))),
        choose
          (equal "trap_exit_active" "0")
          [ choose
              (present "trap_exit")
              [set [SetGlobal] (name "trap_exit_active") (ExprLiteral "1"), set [] sourceStatus (variable (name "trap_exit_status")), invoke "trap_exit"]
              []
          ]
          [],
        choose (equal "pending_signal" "0") [] [Session.request prefix "finish-signal" [ExprVal (variable (name "pending_signal"))]],
        builtin "exit" [ExprVal (variable (name "trap_exit_status"))]
      ]
    errorBody =
      [ set [SetLocal] (name "trap_err_status") (variable sourceStatus),
        set [SetLocal] (name "callback_origin") (ExprQuotedVariable (VarIndex "argv" (IndexSingle (ExprNumLiteral 1)))),
        set [SetLocal] (name "callback_line") (ExprQuotedVariable (VarIndex "argv" (IndexSingle (ExprNumLiteral 2)))),
        Stmt (Decorated DecBuiltin (Set [] "argv" (ExprVariable (VarIndex "argv" (IndexRange (Just (ExprNumLiteral 3)) Nothing))))),
        choose
          (equal "trap_err_enabled" "1")
          [ choose
              (equal "trap_err_active" "0")
              [ choose
                  (present "trap_err")
                  [set [SetLocal] (name "trap_err_active") (ExprLiteral "1"), invoke "trap_err", set [] sourceStatus (variable (name "trap_err_status"))]
                  []
              ]
              []
          ]
          [],
        builtin "return" [ExprVal (variable (name "trap_err_status"))]
      ]

literal :: Text -> ExprOrRedirect
literal = ExprVal . ExprLiteral

variable :: Text -> FishExpr TStr
variable = ExprQuotedVariable . VarScalar

builtin :: Text -> [ExprOrRedirect] -> FishStatement
builtin name = Stmt . Decorated DecBuiltin . Command name

set :: [SetFlag] -> Text -> FishExpr TStr -> FishStatement
set flags name value = Stmt (Decorated DecBuiltin (Set flags name (ExprListLiteral [value])))

nonempty :: [FishStatement] -> NonEmpty FishStatement
nonempty = fromMaybe (builtin "true" [] :| []) . nonEmpty

choose :: FishStatement -> [FishStatement] -> [FishStatement] -> FishStatement
choose predicate yes no = Stmt (If (MkFishJobList (MkFishJobConjunction Nothing (MkFishJobPipeline False [] predicate [] False) [] :| [])) (nonempty yes) no [])

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | One owner for scalar slots, logical export state, and the environment
-- value inherited by a declared-but-unset Bash local. Scalar reads continue to
-- observe the zero-element slot; external commands consume the separate value.
module Language.Fish.Translator.Binding
  ( BindingRuntime,
    bindingRuntime,
    bindingRuntimeNames,
    writeBinding,
    eraseBinding,
    declareLocal,
    declareExport,
    environmentShadows,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Set qualified as S
import Language.Bash.Plan qualified as P
import Language.Fish.DSL.Internal

data BindingRuntime = BindingRuntime
  { scalarName :: Text,
    exportName :: Text,
    fallbackName :: Text
  }
  deriving stock (Eq, Show)

bindingRuntime :: Text -> Text -> BindingRuntime
bindingRuntime prefix name = BindingRuntime name (prefix <> "binding_export_" <> name) (prefix <> "binding_environment_" <> name)

bindingRuntimeNames :: BindingRuntime -> Set Text
bindingRuntimeNames binding = S.fromList [exportName binding, fallbackName binding]

writeBinding :: BindingRuntime -> P.Storage -> FishExpr TStr -> [FishStatement]
writeBinding binding storage value =
  let global = globalStorage storage
      scope = [literal "--global" | global]
      marker = query (scope <> [literal (exportName binding)])
      ordinary = conditional (query (scope <> [literal (scalarName binding)])) [builtin "set" (scope <> [literal (scalarName binding), arg value])] [builtin "set" (scope <> [literal "--unpath", literal (scalarName binding), arg value])]
      marked = builtin "set" (scope <> [arg (scalar (exportName binding)), literal (scalarName binding), arg value])
      clear = builtin "set" (scope <> [literal (fallbackName binding)])
      write = conditional marker [marked, clear] [ordinary]
   in if global || storage == P.Local
        then [write]
        else [conditional (query [literal (scalarName binding)]) [write] [builtin "set" [literal "--global", literal "--unpath", literal (scalarName binding), arg value]]]

eraseBinding :: BindingRuntime -> [FishStatement]
eraseBinding binding = [builtin "set" [literal "--erase", literal name] | name <- [scalarName binding, exportName binding, fallbackName binding]]

-- | The caller has frozen every declaration operand before these actions.
-- Temporary names are allocated by the materializer, not derived from input.
declareLocal :: Text -> BindingRuntime -> Bool -> Maybe (FishExpr TStr) -> [FishStatement]
declareLocal temporary binding fresh value
  | not fresh = maybe [] (writeBinding binding P.Local) value
  | otherwise =
      let savedFlag = temporary <> "_export"
          savedEnvironment = temporary <> "_environment"
          captureFlag =
            conditional
              (query [literal (scalarName binding <> "[1]")])
              [conditional (query [literal "--export", literal (scalarName binding)]) [set [] savedFlag (ExprLiteral "--export")] []]
              [conditional (query [literal (exportName binding)]) [set [] savedFlag (scalar (exportName binding))] []]
          captureEnvironment =
            conditional
              (query [literal (scalarName binding <> "[1]")])
              [conditional (query [literal "--export", literal (scalarName binding)]) [setList [] savedEnvironment (allValues (scalarName binding))] []]
              [conditional (query [literal (fallbackName binding <> "[1]")]) [setList [] savedEnvironment (allValues (fallbackName binding))] []]
          actualFlag = maybe (ExprLiteral "--unexport") (const (scalar savedFlag)) value
          actualValues = maybe [] ((: []) . arg) value
       in [ set [SetLocal] savedFlag (ExprLiteral "--unexport"),
            setList [SetLocal] savedEnvironment (ExprListLiteral []),
            captureFlag,
            captureEnvironment,
            set [SetLocal] (exportName binding) (scalar savedFlag),
            setList [SetLocal] (fallbackName binding) (maybe (allValues savedEnvironment) (const (ExprListLiteral [])) value),
            builtin "set" ([literal "--local", literal "--unpath", arg actualFlag, literal (scalarName binding)] <> actualValues)
          ]

declareExport :: BindingRuntime -> P.Storage -> Maybe (FishExpr TStr) -> [FishStatement]
declareExport binding storage value =
  let global = globalStorage storage
      scope = [literal "--global" | global]
      marker = query (scope <> [literal (exportName binding)])
      initialized = query (scope <> [literal (scalarName binding <> "[1]")])
      exportValue = builtin "set" (scope <> [literal "--export", literal (scalarName binding), arg (allValues (scalarName binding))])
      missing = [builtin "set" [literal "--global", literal "--unexport", literal "--unpath", literal (scalarName binding)], set [SetGlobal] (exportName binding) (ExprLiteral "--export"), setList [SetGlobal] (fallbackName binding) (ExprListLiteral [])]
      existingUnset = [builtin "set" (scope <> [literal "--unexport", literal (scalarName binding)]), conditional marker [set [SetGlobal | global] (exportName binding) (ExprLiteral "--export")] [set [SetGlobal] (exportName binding) (ExprLiteral "--export"), setList [SetGlobal] (fallbackName binding) (ExprListLiteral [])]]
      unset = if global then existingUnset else [conditional (query [literal (scalarName binding)]) existingUnset missing]
      updateFlag = conditional marker [builtin "set" (scope <> [literal (exportName binding), literal "--export"])] []
   in maybe [] (writeBinding binding storage) value <> [updateFlag, conditional initialized [exportValue] unset]

-- | Execute these actions directly in an owned scope-sharing external wrapper.
-- Cloning visible slots before conditionals makes their lifetime the wrapper's;
-- a local introduced inside an if block would disappear before exec.
environmentShadows :: Text -> [BindingRuntime] -> [FishStatement]
environmentShadows temporary bindings = concat (zipWith shadow [0 :: Int ..] bindings)
  where
    shadow index binding =
      let savedFlag = temporary <> "_export_" <> show index
          replace =
            conditional
              (query [literal (scalarName binding <> "[1]")])
              []
              [ conditional
                  (equals (scalar (exportName binding)) "--export")
                  [conditional (query [literal (fallbackName binding <> "[1]")]) [builtin "set" [literal "--export", literal (scalarName binding), arg (allValues (fallbackName binding))]] []]
                  []
              ]
       in [ set [SetLocal] savedFlag (ExprLiteral "--unexport"),
            conditional (query [literal "--export", literal (scalarName binding)]) [set [] savedFlag (ExprLiteral "--export")] [],
            builtin "set" [literal "--local", literal "--unpath", arg (scalar savedFlag), literal (scalarName binding), arg (allValues (scalarName binding))],
            replace
          ]

globalStorage :: P.Storage -> Bool
globalStorage = \case
  P.Global -> True
  P.CallerGlobal _ -> True
  P.Visible -> False
  P.Local -> False
  P.CallerVisible _ -> False

arg :: (Typeable value) => FishExpr value -> ExprOrRedirect
arg = ExprVal

literal :: Text -> ExprOrRedirect
literal = arg . ExprLiteral

scalar :: Text -> FishExpr TStr
scalar = ExprQuotedVariable . VarScalar

allValues :: Text -> FishExpr (TList TStr)
allValues = ExprVariable . VarAll

builtin :: Text -> [ExprOrRedirect] -> FishStatement
builtin name = Stmt . Decorated DecBuiltin . Command name

query :: [ExprOrRedirect] -> FishCommand TStatus
query values = Decorated DecBuiltin (Command "set" (literal "--query" : values))

equals :: FishExpr TStr -> Text -> FishCommand TStatus
equals value expected = Decorated DecBuiltin (Command "test" [arg value, literal "=", literal expected])

set :: [SetFlag] -> Text -> FishExpr TStr -> FishStatement
set flags name value = setList flags name (ExprListLiteral [value])

setList :: [SetFlag] -> Text -> FishExpr (TList TStr) -> FishStatement
setList flags name values = Stmt (Decorated DecBuiltin (Set (SetUnpath : flags) name values))

conditional :: FishCommand TStatus -> [FishStatement] -> [FishStatement] -> FishStatement
conditional condition yes no =
  let jobs = MkFishJobList (MkFishJobConjunction Nothing (MkFishJobPipeline False [] (Stmt condition) [] False) [] NE.:| [])
      body = fromMaybe (builtin "true" [] NE.:| []) (NE.nonEmpty yes)
   in Stmt (If jobs body no [])

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | Structural child programs and bounded byte/status transport. Child scripts
-- contain only already-lowered plans; helpers never interpret Bash source.
module Language.Fish.Translator.Child
  ( ChildMode (..),
    ChildRuntime (..),
    ChildInvocation,
    childPrelude,
    childCommand,
    childRequirements,
    childStatistics,
    ChildCapture (..),
    ChildPipeline (..),
    materializeChild,
    materializeCapture,
    materializePipeline,
  )
where

import Data.Char (isAlpha, isAlphaNum)
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as S
import Data.Text qualified as T
import Language.Fish.DSL.Internal
import Language.Fish.Translator.Statistics (materializationStatistics)
import Monk.Translation.Types
  ( FishFeature (FunctionScopeSharing, NulDelimitedCapture),
    NativeOperation (NativeChildCapture, NativeChildRun, NativeDescriptorState),
    PlatformCapability (Linux64DescriptorFilesystem),
    RequirementUse (..),
    RuntimeProgram (RequiresCommand, RequiresFishFeature, RequiresPlatformCapability),
    RuntimeRequirement (..),
    TranslationStatistics,
    nativeRuntimeRequirement,
  )

data ChildMode = SubstitutionChild | IsolatedChild
  deriving stock (Eq, Show)

data ChildRuntime = MkChildRuntime
  { childOwnedPrefix :: Text,
    childStatusName :: Text,
    childErrexitName :: Text,
    childSuppressionName :: Text,
    childRuntimeNames :: Set Text,
    childNativeRuntimeName :: Text,
    childLexicallySuppressed :: Bool
  }
  deriving stock (Eq, Show)

data ChildInvocation = MkChildInvocation
  { childPrelude :: [FishStatement],
    childCommand :: FishStatement,
    childRequirements :: [RuntimeRequirement],
    childStatistics :: TranslationStatistics,
    invocationFrames :: [ExprOrRedirect],
    invocationShadows :: [FishStatement],
    invocationRuntimeName :: Text
  }
  deriving stock (Eq, Show)

data ChildCapture = MkChildCapture
  { captureStatements :: [FishStatement],
    captureValue :: FishExpr TStr,
    captureStatus :: FishExpr TStr,
    captureError :: FishExpr TStr,
    captureRequirements :: [RuntimeRequirement]
  }
  deriving stock (Eq, Show)

data ChildPipeline = MkChildPipeline
  { pipelineStatements :: [FishStatement],
    pipelineStatus :: FishExpr TStr,
    pipelineRequirements :: [RuntimeRequirement]
  }
  deriving stock (Eq, Show)

-- | Prefix, execution mode, runtime names, actual visible Fish scalar names,
-- owned helper/function definitions, then the lowered child statements.
materializeChild :: Text -> ChildMode -> ChildRuntime -> Set Text -> [FishStatement] -> [FishStatement] -> Either Text ChildInvocation
materializeChild prefix mode runtime bindings definitions body = do
  let names = S.toAscList ((bindings <> childRuntimeNames runtime <> S.fromList [childStatusName runtime, childErrexitName runtime, childSuppressionName runtime]) S.\\ S.fromList ["SHLVL", "PWD"])
  unless (all validName (childNativeRuntimeName runtime : prefix : names)) (Left "Child snapshot requires concrete scalar Fish binding names")
  let snapshots = zipWith snapshot [0 :: Int ..] names
      arguments = concatMap (\(_, values, _) -> values) snapshots
      restore = concatMap (\(_, _, statements) -> statements) snapshots
      finalStatus = if null body then ExprLiteral "0" else variable (childStatusName runtime)
      wrapper = prefix <> "_body"
      loadState = ExprCommandSubst (builtin "string" [value (ExprLiteral "split0"), RedirectVal (MkRedirect RedirectStdin RedirectIn (RedirectFile (argument 1)))] NE.:| [])
      script =
        MkScript
          ( [set [SetGlobal] "fish_read_limit" (ExprLiteral "0"), setList [SetGlobal] "argv" loadState]
              <> definitions
              <> restore
              <> [setList [] "argv" (ExprVariable (VarIndex "argv" (IndexRange (Just (ExprNumLiteral (length names * 4 + 1))) Nothing)))]
              <> [set [] (childErrexitName runtime) (ExprLiteral "0") | mode == SubstitutionChild]
              <> [set [] (childSuppressionName runtime) (ExprLiteral "1") | childLexicallySuppressed runtime]
              <> [ Stmt (Function (MkFishFunction wrapper [FuncUnknownFlag "--no-scope-shadowing"] [] (bodyNE (body <> [builtin "return" [value finalStatus]])))),
                   Stmt (Command wrapper [value (ExprVariable (VarAll "argv"))]),
                   builtin "exit" [value (variable "status")]
                 ]
          )
      frames = [value (ExprEmbeddedScript script), value (variable "SHLVL")] <> arguments <> [value (ExprVariable (VarAll "argv"))]
      shadows = [setList [SetLocal, SetUnexport, SetUnpath] name (ExprVariable (VarAll name)) | name <- names]
      launcher = prefix <> "_launch"
      launch = Stmt (Function (MkFishFunction launcher [FuncUnknownFlag "--no-scope-shadowing"] [] (transportCommand (childNativeRuntimeName runtime) "child-run" "" frames shadows NE.:| [])))
      prelude = concatMap (\(statements, _, _) -> statements) snapshots <> [launch]
  pure (MkChildInvocation prelude (Stmt (Command launcher [value (ExprVariable (VarAll "argv"))])) [require "fish", nativeRuntimeRequirement NativeDescriptorState "Observe original child stream descriptors", nativeRuntimeRequirement NativeChildRun "Owned child execution and byte transport", feature FunctionScopeSharing, feature NulDelimitedCapture, platformRequirement] (materializationStatistics (childOwnedPrefix runtime) (childNativeRuntimeName runtime) script) frames shadows (childNativeRuntimeName runtime))
  where
    snapshot index name =
      let present = prefix <> "_present_" <> show index
          exported = prefix <> "_exported_" <> show index
          count = prefix <> "_count_" <> show index
          saved = prefix <> "_value_" <> show index
          ordinal = index * 4 + 1
          query flags = builtinCommand "set" (map (value . ExprLiteral) ("--query" : flags <> [name]))
          setup =
            [ set [SetLocal] present (ExprLiteral "0"),
              set [SetLocal] exported (ExprLiteral "0"),
              set [SetLocal] count (ExprLiteral "0"),
              conditional (query []) [set [] present (ExprLiteral "1")] [],
              conditional (query ["--export"]) [set [] exported (ExprLiteral "1")] [],
              conditional (builtinCommand "set" [value (ExprLiteral "--query"), value (ExprLiteral (name <> "[1]"))]) [set [] count (ExprLiteral "1")] [],
              set [SetLocal] saved (snapshotValue name)
            ]
          restoreValue flags = conditional (equals (argument (ordinal + 2)) "1") [restoreSnapshot (SetUnpath : flags) name (argument (ordinal + 3))] [setList (SetUnpath : flags) name (ExprListLiteral [])]
          restore =
            [ conditional
                (equals (argument ordinal) "1")
                [conditional (equals (argument (ordinal + 1)) "1") [restoreValue [SetGlobal, SetExport]] [restoreValue [SetGlobal, SetUnexport]]]
                [builtin "set" [value (ExprLiteral "--erase"), value (ExprLiteral name)]]
            ]
       in (setup, map (value . variable) [present, exported, count, saved], restore)
    argument index = ExprQuotedVariable (VarIndex "argv" (IndexSingle (ExprNumLiteral index)))
    -- Only the directory subsystem owns a list binding. Its variable-style
    -- byte encoding contains no slash or newline, so one scalar snapshot frame
    -- preserves the stack without widening ordinary scalar admission.
    snapshotValue "dirstack" =
      ExprQuotedCommandSubst
        (builtin "string" [value (ExprLiteral "join"), value (ExprLiteral "/"), value (ExprLiteral "--"), value (ExprCommandSubst (builtin "string" [value (ExprLiteral "escape"), value (ExprLiteral "--style=var"), value (ExprLiteral "--"), value (ExprVariable (VarAll "dirstack"))] NE.:| []))] NE.:| [])
    snapshotValue name = variable name
    restoreSnapshot flags "dirstack" encoded =
      setList
        flags
        "dirstack"
        (ExprCommandSubst (builtin "string" [value (ExprLiteral "unescape"), value (ExprLiteral "--style=var"), value (ExprLiteral "--"), value (ExprCommandSubst (builtin "string" [value (ExprLiteral "split"), value (ExprLiteral "/"), value (ExprLiteral "--"), value encoded] NE.:| []))] NE.:| []))
    restoreSnapshot flags name encoded = set flags name encoded

materializeCapture :: Text -> Maybe SourceRange -> ChildInvocation -> Either Text ChildCapture
materializeCapture prefix range invocation = do
  unless (validName prefix) (Left "Capture temporary prefix is not a scalar Fish name")
  source <- maybe (Left "Command substitution byte diagnostics need the original source occurrence") (pure . rangeStart) range
  let packet = prefix <> "_packet"
      result = prefix <> "_result"
      status = prefix <> "_status"
      failure = prefix <> "_error"
      warning = srcFile source <> ": line " <> show (srcLine source) <> ": warning: command substitution: ignored null byte in input\n"
      producer = transportCommand (invocationRuntimeName invocation) "child-capture" warning (invocationFrames invocation) (invocationShadows invocation)
      capture = ExprCommandSubst (Stmt (Pipeline (MkFishJobPipeline False [] producer [PipeTo [] (builtin "string" [value (ExprLiteral "split0")])] False)) NE.:| [])
      member index = ExprQuotedVariable (VarIndex packet (IndexSingle (ExprNumLiteral index)))
      validPacket = builtinCommand "test" [value (ExprCommandSubst (builtin "count" [value (ExprVariable (VarAll packet))] NE.:| [])), value (ExprLiteral "="), value (ExprLiteral "3")]
      accepted = [set [] status (member 2), set [] result (member 3), set [] failure (ExprLiteral "")]
      check = conditional validPacket [conditional (equals (member 1) "ok") accepted []] []
  pure
    MkChildCapture
      { captureStatements = childPrelude invocation <> [set [SetLocal] result (ExprLiteral ""), set [SetLocal] status (ExprLiteral "125"), set [SetLocal] failure (ExprLiteral "child-transport-failure"), setList [SetLocal] packet (ExprListLiteral []), Stmt (Begin (set [SetLocal] "fish_read_limit" (ExprLiteral "0") NE.:| [setList [] packet capture]) []), check],
        captureValue = variable result,
        captureStatus = variable status,
        captureError = variable failure,
        captureRequirements = nativeRuntimeRequirement NativeChildCapture "Command substitution byte transport" : childRequirements invocation
      }

materializePipeline :: Text -> FishExpr TStr -> NonEmpty ChildInvocation -> ChildPipeline
materializePipeline prefix pipefail invocations =
  let statuses = prefix <> "_statuses"
      selected = prefix <> "_status"
      current = prefix <> "_stage_status"
      firstInvocation NE.:| remaining = invocations
      pipeline = Stmt (Pipeline (MkFishJobPipeline False [] (childCommand firstInvocation) (map (PipeTo [] . childCommand) remaining) False))
      choose = Stmt (For current (ExprVariable (VarAll statuses)) (conditional (equals (variable current) "0") [] [set [] selected (variable current)] NE.:| []) [])
      statements =
        concatMap childPrelude (toList invocations)
          <> [ pipeline,
               setList [SetLocal] statuses (ExprVariable (VarAll "pipestatus")),
               set [SetLocal] selected (ExprQuotedVariable (VarIndex statuses (IndexSingle (ExprNumLiteral (-1))))),
               conditional (equals pipefail "1") [choose] []
             ]
   in MkChildPipeline statements (variable selected) (concatMap childRequirements (toList invocations))

validName :: Text -> Bool
validName name = case T.uncons name of
  Just (leading, rest) -> (isAlpha leading || leading == '_') && T.all (\c -> isAlphaNum c || c == '_') rest
  Nothing -> False

value :: (Typeable a) => FishExpr a -> ExprOrRedirect
value = ExprVal

variable :: Text -> FishExpr TStr
variable = ExprQuotedVariable . VarScalar

builtinCommand :: Text -> [ExprOrRedirect] -> FishCommand TStatus
builtinCommand name = Decorated DecBuiltin . Command name

builtin :: Text -> [ExprOrRedirect] -> FishStatement
builtin name = Stmt . builtinCommand name

set :: [SetFlag] -> Text -> FishExpr TStr -> FishStatement
set flags name scalar = setList flags name (ExprListLiteral [scalar])

setList :: [SetFlag] -> Text -> FishExpr (TList TStr) -> FishStatement
setList flags name values = Stmt (Decorated DecBuiltin (Set flags name values))

equals :: FishExpr TStr -> Text -> FishCommand TStatus
equals actual expected = builtinCommand "test" [value actual, value (ExprLiteral "="), value (ExprLiteral expected)]

bodyNE :: [FishStatement] -> NonEmpty FishStatement
bodyNE = fromMaybe (builtin "true" [] NE.:| []) . NE.nonEmpty

conditional :: FishCommand TStatus -> [FishStatement] -> [FishStatement] -> FishStatement
conditional condition yes no =
  Stmt (If (MkFishJobList (MkFishJobConjunction Nothing (MkFishJobPipeline False [] (Stmt condition) [] False) [] NE.:| [])) (bodyNE yes) no [])

require :: Text -> RuntimeRequirement
require command = MkRuntimeRequirement (RequiresCommand command) (MkRequirementUse "Owned child execution and byte transport" Nothing NE.:| [])

feature :: FishFeature -> RuntimeRequirement
feature capability = MkRuntimeRequirement (RequiresFishFeature capability) (MkRequirementUse "Owned child snapshots and launch scope" Nothing NE.:| [])

platformRequirement :: RuntimeRequirement
platformRequirement = MkRuntimeRequirement (RequiresPlatformCapability Linux64DescriptorFilesystem) (MkRequirementUse "Owned anonymous script and state descriptors" Nothing NE.:| [])

-- | Duplicate the original stdin before connecting the metadata pipe. Every
-- payload crosses a builtin byte writer, never an executable argument boundary.
transportCommand :: Text -> Text -> Text -> [ExprOrRedirect] -> [FishStatement] -> FishStatement
transportCommand runtimeName mode warning frames shadows =
  let descriptorMask = runtimeName <> "_child_descriptors"
      writer = builtin "printf" (value (ExprLiteral "%s\\0") : value (ExprLiteral warning) : value (variable descriptorMask) : frames)
      driver = Stmt (Command runtimeName (map (value . ExprLiteral) ["--abi", "1", mode]))
      pipe = Stmt (Pipeline (MkFishJobPipeline False [] writer [PipeTo [] driver] False))
      withInput = Stmt (Begin (pipe NE.:| []) [RedirectVal (MkRedirect (RedirectFD 3) RedirectIn (RedirectTargetFD 0))])
      -- Odd masks have an open stdin. A closed stdin must never be duplicated:
      -- Fish would fail the native exec before it could consume its metadata.
      launch = foldr (\mask fallback -> [conditional (equals (variable descriptorMask) mask) [withInput] fallback]) [pipe] ["1", "3", "5", "7"]
      probe = Stmt (Command runtimeName (map (value . ExprLiteral) ["--abi", "1", "descriptor-state"]))
   in Stmt (Begin (bodyNE (shadows <> [probe, set [SetLocal] descriptorMask (variable "status")] <> launch)) [])

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

-- | Complete structural ABI-2 templates. Required fields are scalar, variadic
-- arguments retain their cardinality, and executable bodies remain inspectable.
module Language.Fish.Translator.Session.Request
  ( Request (..),
    ReplyShape,
    requestArguments,
    stageArguments,
    replyExpression,
    Site (..),
    OpenMode (..),
    ReadDestination (..),
    Direction (..),
    Writer (..),
    DirectoryWriter (..),
    OutputDescriptor (..),
    Stage (..),
    SomeStage (..),
    Body,
    singleBody,
    pipelineBody,
    PreparedStage,
    prepareStage,
  )
where

import Language.Fish.DSL.Argument
import Language.Fish.DSL.Internal (FishExpr (..), FishIndex (..), FishType (..), FishVarRef (..), Identifier, Script, identifierText)
import Language.Fish.Translator.Emission (Emission, emit)
import Language.Fish.Translator.Identifier (compilerIdentifier)
import Language.Fish.Translator.Statement (arg, builtin)
import Monk.Runtime.Abi2

-- Diagnostic spelling is an evaluated scalar, including source resumption.
data Site = Site (FishExpr TStr) (FishExpr TStr)
  deriving stock (Show, Eq)

data OpenMode = ReadFile | WriteFile | AppendFile | ReadWriteFile
  deriving stock (Show, Eq)

data ReadDestination = ReplyVariable | ScalarVariables (NonEmpty Text) | ArrayVariable
  deriving stock (Show, Eq)

data Direction = Input | Output
  deriving stock (Show, Eq)

data Writer = Printf | Echo
  deriving stock (Show, Eq)

data DirectoryWriter = Pwd | Cd | Pushd | Popd
  deriving stock (Show, Eq)

data OutputDescriptor = StandardOutput | StandardError
  deriving stock (Show, Eq)

type role Stage nominal

data Stage (kind :: BodyOpcode) where
  ExternalStage :: FishExpr TStr -> [SomeArgument] -> Stage BodyExternal
  ExternalSiteStage :: Site -> FishExpr TStr -> [SomeArgument] -> Stage BodyExternalSite
  WriterStage :: Site -> Writer -> [SomeArgument] -> Stage BodyBuiltin
  DirectoryStage :: Site -> DirectoryWriter -> OutputDescriptor -> FishExpr TStr -> Stage BodyDirectoryOutput
  BodyStage :: Script -> [SomeArgument] -> Stage BodyBody
  SnapshotStage :: Script -> FishExpr TStr -> [SomeArgument] -> Stage BodySnapshot

deriving stock instance Show (Stage kind)

deriving stock instance Eq (Stage kind)

data SomeStage where
  SomeStage :: Stage kind -> SomeStage

-- These fields are minted only by capturing a complete stage's expansion.
data PreparedStage = PreparedStage (FishExpr TStr) (FishExpr TStr) (FishExpr (TList TStr))

data Body = Single SomeStage | PipelineBody (FishExpr TStr) (NonEmpty PreparedStage)

singleBody :: Stage kind -> Body
singleBody = Single . SomeStage

pipelineBody :: FishExpr TStr -> NonEmpty PreparedStage -> Body
pipelineBody = PipelineBody

-- | Capture each stage immediately after its own operand effects. A later
-- stage cannot change its argv or status reads before the request is emitted.
prepareStage :: Identifier -> Emission SomeStage -> Emission PreparedStage
prepareStage name emission = do
  SomeStage stage <- emission
  let fields = stageArguments stage
      remaining = ExprVariable (VarIndex name (IndexRange (Just (ExprNumLiteral 2)) Nothing))
      kind = ExprQuotedVariable (VarIndex name (IndexSingle (ExprNumLiteral 1)))
      count = ExprQuotedCommandSubst (builtin "count" [arg remaining] :| [])
  emit [builtin "set" (map (arg . ExprLiteral) ["--local", "--unexport", "--unpath", identifierText name] <> map argumentExpression (toList fields))]
  pure (PreparedStage kind count remaining)

type role Request nominal

data Request (operation :: SessionOpcode) where
  Capture :: FishExpr TStr -> Stage BodySnapshot -> Request SessionCapture
  FdClose :: Natural -> Request SessionFdClose
  FdData :: Natural -> FishExpr TStr -> Request SessionFdData
  FdDup :: Site -> Natural -> Natural -> Request SessionFdDup
  FdEndpoint :: Site -> Natural -> FishExpr TStr -> Request SessionFdEndpoint
  FdOpen :: Site -> Natural -> OpenMode -> FishExpr TStr -> Request SessionFdOpen
  FdPop :: Request SessionFdPop
  FdPush :: Request SessionFdPush
  FdReset :: Request SessionFdReset
  FinishBrokenPipe :: Request SessionFinishSignal
  Ping :: Request SessionPing
  Read :: Site -> Natural -> Bool -> Text -> Maybe Natural -> FishExpr TStr -> ReadDestination -> Request SessionRead
  Run :: Body -> Request SessionRun
  Spawn :: Body -> Request SessionSpawn
  Substitution :: Direction -> Body -> Request SessionSubstitution
  ReleaseSubstitutions :: Request SessionSubstitutionRelease
  Wait :: Site -> [SomeArgument] -> Request SessionWait

-- The response shape follows the operation, independently of source size.
type family ReplyShape (operation :: SessionOpcode) :: FishType where
  ReplyShape SessionRead = TList TStr
  ReplyShape SessionSubstitution = TList TStr
  ReplyShape operation = TStr

requestArguments :: Request operation -> NonEmpty SomeArgument
requestArguments request = case request of
  Capture warning stage -> opcode SessionCapture (scalar warning : toList (stageArguments stage))
  FdClose descriptor -> opcode SessionFdClose [literal (show descriptor)]
  FdData descriptor bytes -> opcode SessionFdData [literal (show descriptor), scalar bytes]
  FdDup site target source -> opcode SessionFdDup (siteArguments site <> map (literal . show) [target, source])
  FdEndpoint site target lease -> opcode SessionFdEndpoint (siteArguments site <> [literal (show target), scalar lease])
  FdOpen site descriptor mode path -> opcode SessionFdOpen (siteArguments site <> [literal (show descriptor), literal (openMode mode), scalar path])
  FdPop -> opcode SessionFdPop []
  FdPush -> opcode SessionFdPush []
  FdReset -> opcode SessionFdReset []
  FinishBrokenPipe -> opcode SessionFinishSignal [literal "13"]
  Ping -> opcode SessionPing []
  Read site descriptor raw delimiter limit ifs destination ->
    let (mode, count) = case destination of
          ReplyVariable -> ("reply", 1 :: Int)
          ScalarVariables names -> ("scalar", length names)
          ArrayVariable -> ("array", 0)
     in opcode SessionRead (siteArguments site <> [literal (show descriptor), literal (if raw then "1" else "0"), literal delimiter, literal (maybe "-1" show limit), scalar ifs, literal mode, literal (show count)])
  Run body -> opcode SessionRun (bodyArguments body)
  Spawn body -> opcode SessionSpawn (bodyArguments body)
  Substitution direction body -> opcode SessionSubstitution (literal (case direction of Input -> "input"; Output -> "output") : bodyArguments body)
  ReleaseSubstitutions -> opcode SessionSubstitutionRelease []
  Wait site arguments -> opcode SessionWait (siteArguments site <> arguments)
  where
    opcode operation = (literal (toText (sessionOpcodeName operation)) :|)

replyExpression :: Request operation -> Text -> FishExpr (ReplyShape operation)
replyExpression request prefix = case request of
  Read {} -> ExprVariable (VarIndex (compilerIdentifier (prefix <> "session_fields")) (IndexRange (Just (ExprNumLiteral 2)) Nothing))
  Substitution {} -> ExprVariable (VarAll (compilerIdentifier (prefix <> "session_endpoint")))
  Spawn {} -> ExprQuotedVariable (VarScalar (compilerIdentifier (prefix <> "last_pid")))
  Capture {} -> status
  FdClose {} -> status
  FdData {} -> status
  FdDup {} -> status
  FdEndpoint {} -> status
  FdOpen {} -> status
  FdPop -> status
  FdPush -> status
  FdReset -> status
  FinishBrokenPipe -> status
  Ping -> status
  Run {} -> status
  ReleaseSubstitutions -> status
  Wait {} -> status
  where
    status = ExprQuotedVariable (VarScalar "status")

bodyArguments :: Body -> [SomeArgument]
bodyArguments (Single (SomeStage stage)) = toList (stageArguments stage)
bodyArguments (PipelineBody policy stages) =
  literal (toText (bodyOpcodeName BodyPipeline)) : scalar policy : literal (show (length stages)) : foldMap stageFields stages
  where
    stageFields (PreparedStage kind count values) = [scalar kind, scalar count, SomeArgument (ListArgument values)]

stageArguments :: Stage kind -> NonEmpty SomeArgument
stageArguments stage = case stage of
  ExternalStage executable arguments -> opcode BodyExternal (scalar executable : arguments)
  ExternalSiteStage site executable arguments -> opcode BodyExternalSite (siteArguments site <> [scalar executable] <> arguments)
  WriterStage site writer arguments -> opcode BodyBuiltin (siteArguments site <> [literal (case writer of Printf -> "printf"; Echo -> "echo")] <> arguments)
  DirectoryStage site writer descriptor bytes -> opcode BodyDirectoryOutput (siteArguments site <> [literal (case writer of Pwd -> "pwd"; Cd -> "cd"; Pushd -> "pushd"; Popd -> "popd"), literal (case descriptor of StandardOutput -> "1"; StandardError -> "2"), scalar bytes])
  BodyStage script arguments -> opcode BodyBody (scalar (ExprEmbeddedScript script) : arguments)
  SnapshotStage script level fields -> opcode BodySnapshot (scalar (ExprEmbeddedScript script) : scalar level : fields)
  where
    opcode operation = (literal (toText (bodyOpcodeName operation)) :|)

siteArguments :: Site -> [SomeArgument]
siteArguments (Site origin line) = [scalar origin, scalar line]

openMode :: OpenMode -> Text
openMode ReadFile = "read"
openMode WriteFile = "write"
openMode AppendFile = "append"
openMode ReadWriteFile = "read-write"

scalar :: FishExpr TStr -> SomeArgument
scalar = SomeArgument . ScalarArgument

literal :: Text -> SomeArgument
literal = scalar . ExprLiteral

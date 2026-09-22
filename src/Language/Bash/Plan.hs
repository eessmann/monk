{-# LANGUAGE DerivingStrategies #-}

-- | Private source semantics. No node contains executable ShellCheck syntax.
module Language.Bash.Plan
  ( SourcePlan (..),
    Statement (..),
    StatementNode (..),
    Declaration (..),
    Redirection (..),
    DescriptorMode (..),
    ReadOptions (..),
    ReadTarget (..),
    TrapKind (..),
    ProcessDirection (..),
    Word (..),
    ExpansionPart (..),
    guaranteesField,
    Scalar (..),
    Pattern (..),
    PatternPart (..),
    CallTarget (..),
    Storage (..),
    CaseArm (..),
    CaseEnd (..),
    Option (..),
    SourceRequest (..),
    SourceContext (..),
    SourceEntryContext (..),
    ChildRegion (..),
  )
where

import Language.Bash.Arithmetic.Plan (ArithmeticExpr)
import Language.Bash.Arithmetic.Source (ArithmeticSite)
import Language.Bash.Plan.Directory (DirectoryFacts, DirectoryOperation)
import Language.Fish.DSL (SourceRange)
import Monk.Translation.Types (Approximation, BindingExport, TranslateConfig)
import Prelude hiding (Word)

data SourcePlan = SourcePlan TranslateConfig [Statement] (Set Text)
  deriving stock (Show, Eq)

data Statement = Statement (Maybe SourceRange) StatementNode
  deriving stock (Show, Eq)

data StatementNode
  = Sequence [Statement]
  | AssignmentCommand Bool [Statement]
  | DeclarationCommand [Declaration]
  | Redirected [Redirection] Statement
  | Invoke CallTarget [Word]
  | PrefixedInvoke [(Storage, Text, Scalar)] CallTarget [Word]
  | Assign Storage Text Scalar
  | AssignArray Storage Text [Word]
  | AppendArray Storage Text [Word]
  | AssignArrayElement Storage Text Int Scalar
  | Erase Text
  | SetArguments [Word]
  | DirectoryOperation DirectoryOperation
  | SetOption Option Bool
  | And Statement Statement
  | Or Statement Statement
  | Negate Statement
  | Conditional [Statement] [Statement] [Statement]
  | WhileLoop Bool [Statement] [Statement]
  | ForLoop Storage Text [Word] [Statement]
  | ArithmeticFor Statement Statement Statement [Statement]
  | ShiftArguments Int
  | Case Scalar [CaseArm]
  | PatternCondition Bool Scalar Pattern
  | NumericCondition Text Scalar Scalar
  | Approximate Approximation [Statement]
  | DefineFunction Text [Statement]
  | SourceBody SourceRequest [Statement]
  | Subshell ChildRegion
  | Pipeline (NonEmpty ChildRegion)
  | SupervisedPipeline (NonEmpty ChildRegion)
  | Background ChildRegion
  | Wait [Word]
  | Read ReadOptions ReadTarget
  | PrefixedRead [(Storage, Text, Scalar)] ReadOptions ReadTarget
  | SetTrap TrapKind (Maybe [Statement])
  | ArithmeticCommand ArithmeticSite ArithmeticExpr (Map Text Storage)
  | Return (Maybe Scalar)
  | Exit (Maybe Scalar)
  | Break
  | Continue
  deriving stock (Show, Eq)

-- Declaration operands expand before bindings are installed. The Boolean
-- records whether this declaration introduces a fresh function-local slot.
data Declaration
  = DeclareLocal Bool Text (Maybe Scalar)
  | DeclareExport Storage Text (Maybe Scalar)
  deriving stock (Show, Eq)

data Redirection
  = DuplicateDescriptor Int Int Bool
  | CloseDescriptor Int Bool
  | NullDescriptor Int Bool
  | OpenDescriptor Int DescriptorMode Scalar
  | InputDescriptor Int Scalar Bool
  deriving stock (Show, Eq)

data DescriptorMode = ReadFile | WriteFile | AppendFile | ReadWriteFile
  deriving stock (Show, Eq)

data ProcessDirection = ProcessInput | ProcessOutput
  deriving stock (Show, Eq)

data TrapKind = ExitTrap | ErrTrap
  deriving stock (Show, Eq)

data ReadOptions = ReadOptions
  { readRaw :: Bool,
    readDelimiter :: Text,
    readCount :: Maybe Int,
    readDescriptor :: Int
  }
  deriving stock (Show, Eq)

data ReadTarget = ReadReply Storage | ReadScalars [(Storage, Text)] | ReadArray Storage Text
  deriving stock (Show, Eq)

data Storage = Global | Visible | Local | CallerGlobal BindingExport | CallerVisible BindingExport
  deriving stock (Show, Eq)

data Option = Errexit | Pipefail
  deriving stock (Show, Eq)

data CallTarget = Builtin Text | External Text | Function Text
  deriving stock (Show, Eq)

data Word
  = OneField Scalar
  | SplitFields Scalar
  | PathnameFields Pattern
  | QuotedArguments Scalar Scalar Bool
  | QuotedArray Text Scalar Scalar Bool
  | ExpandedWord [ExpansionPart]
  deriving stock (Show, Eq)

data ExpansionPart = QuotedExpansion Scalar | LiteralExpansion Scalar | SplitExpansion Scalar
  deriving stock (Show, Eq)

-- | The admitted glob envelope has a literal no-match fallback. Other list
-- words may vanish; a quoted forced-empty word still owns one argument.
guaranteesField :: Word -> Bool
guaranteesField = \case
  OneField _ -> True
  PathnameFields _ -> True
  QuotedArguments _ _ True -> True
  QuotedArray _ _ _ True -> True
  ExpandedWord parts -> any (\case QuotedExpansion _ -> True; LiteralExpansion (Literal text) -> text /= ""; _ -> False) parts
  _ -> False

data Scalar
  = Literal Text
  | Variable Text
  | ArrayElement Text Int
  | ArrayLength Text
  | Positional Int
  | PositionalDefault Int Bool Scalar
  | PositionalAlternate Int Bool Scalar
  | ArgumentCount
  | LastStatus
  | LastBackgroundPid
  | Concat [Scalar]
  | Substitute ChildRegion
  | ProcessSubstitution ProcessDirection ChildRegion
  | DefaultValue Storage Text Bool Bool Scalar
  | AlternateValue Text Bool Scalar
  | ParameterTransform Text Scalar Text Text
  | ParameterPatternTransform Text Scalar Pattern
  | AppendValue Text Scalar
  | ByteLiteral ByteString
  | PlatformBytes ByteString ByteString
  | ArithmeticValue ArithmeticSite ArithmeticExpr (Map Text Storage)
  deriving stock (Show, Eq)

newtype Pattern = MkPattern [PatternPart]
  deriving stock (Show, Eq)

data PatternPart = LiteralPattern Scalar | ActivePattern Scalar
  deriving stock (Show, Eq)

data CaseArm = CaseArm [Pattern] [Statement] CaseEnd
  deriving stock (Show, Eq)

-- | An isolated execution region owns its finite normalized function closure
-- and the scalar names whose presence, attributes and values must be copied.
data ChildRegion = MkChildRegion
  { childRange :: Maybe SourceRange,
    childStatements :: [Statement],
    childFunctions :: Map Text [Statement],
    childVariables :: Set Text,
    childArrays :: Set Text,
    childNeedsEnvironment :: Bool
  }
  deriving stock (Show, Eq)

data CaseEnd = StopCase | FallThrough | Retest
  deriving stock (Show, Eq)

data SourceContext = SharedSource | ChildSource | DeferredSource
  deriving stock (Show, Eq)

data SourceRequest = SourceRequest
  { sourceRequestId :: Int,
    sourceRequestRange :: Maybe SourceRange,
    sourceRequestTarget :: Text,
    sourceRequestArguments :: [Word],
    sourceRequestContext :: SourceContext,
    sourceRequestStack :: [Text],
    sourceRequestEntryContext :: SourceEntryContext,
    sourceRequestWorkingDirectory :: Maybe Text
  }
  deriving stock (Show, Eq)

data SourceEntryContext = SourceEntryContext
  { sourceEntryDefinitions :: Map Text (Text, Int),
    sourceEntryVariables :: Set Text,
    sourceEntryConstants :: Map Text Text,
    sourceEntryArrays :: Map Text Int,
    sourceEntryLocals :: Set Text,
    sourceEntryNumericVariables :: Set Text,
    sourceEntryDirectoryFacts :: DirectoryFacts
  }
  deriving stock (Show, Eq, Ord)

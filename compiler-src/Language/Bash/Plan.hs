{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Private source semantics. No node contains executable ShellCheck syntax.
module Language.Bash.Plan
  ( SourcePlan,
    OwnedPlan,
    EntryBody,
    entryBody,
    sealSourcePlan,
    ownSourcePlan,
    withSourcePlan,
    sourcePlanConfig,
    rebindSourcePlan,
    normalizedContext,
    normalizedBody,
    normalizedReserved,
    Statement (..),
    ScopedBody,
    scopedBody,
    withScopedBody,
    ForBody,
    forBody,
    withForBody,
    WhileBody,
    whileBody,
    withWhileBody,
    ArithmeticBody,
    arithmeticBody,
    withArithmeticBody,
    sameStatement,
    bodyEffects,
    StatementNode (..),
    Declaration (..),
    Redirection (..),
    DescriptorMode (..),
    ReadOptions (..),
    ReadTarget (..),
    TrapKind (..),
    ProcessDirection (..),
    Word (OneField, SplitFields, PathnameFields, QuotedArguments, QuotedArray, ExpandedWord),
    withWord,
    Value (..),
    ExpansionPart (..),
    guaranteesField,
    Scalar,
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

import Data.Type.Equality (testEquality, (:~:) (Refl))
import GHC.Show (Show (showsPrec))
import Language.Bash.Arithmetic.Plan (ArithmeticExpr)
import Language.Bash.Arithmetic.Source (ArithmeticSite)
import Language.Bash.Plan.Control (BodyRoot, EntryRoot, LoopTarget, ReturnTarget, Root, RootKind (..), SetArgumentsTarget, ShiftTarget)
import Language.Bash.Plan.Control qualified as Control
import Language.Bash.Plan.Directory (DirectoryFacts, DirectoryOperation)
import Language.Bash.Plan.Facts (ArrayShape)
import Language.Bash.Plan.Identity (DefinitionIdentity)
import Language.Bash.Plan.Operator (NumericComparison, PatternTrim, Replacement)
import Monk.Compiler.Context (Context, Entry, Phase (Normalized), contextConfig, withContext)
import Monk.Compiler.Index
import Monk.Source.Location (SourceRange)
import Monk.Translation.Types (Approximation, BindingExport, RuntimeSelection, TranslateConfig (translationRuntime))
import Prelude hiding (Word)

-- Normalized syntax carries the owner selected before parsing/normalization.
-- Existential packaging bounds inferred types independently of program size.
type role OwnedPlan nominal nominal nominal nominal nominal

data OwnedPlan owner target entry provider (phase :: Phase) where
  NormalizedPlan :: Context owner target entry provider -> EntryBody owner entry -> Set Text -> OwnedPlan owner target entry provider Normalized

data SourcePlan where
  SourcePlan :: OwnedPlan owner target entry provider Normalized -> SourcePlan

sealSourcePlan :: Context owner target entry provider -> EntryBody owner entry -> Set Text -> OwnedPlan owner target entry provider Normalized
sealSourcePlan = NormalizedPlan

ownSourcePlan :: OwnedPlan owner target entry provider Normalized -> SourcePlan
ownSourcePlan = SourcePlan

withSourcePlan :: SourcePlan -> (forall owner target entry provider. OwnedPlan owner target entry provider Normalized -> result) -> result
withSourcePlan (SourcePlan plan) consume = consume plan

normalizedContext :: OwnedPlan owner target entry provider Normalized -> Context owner target entry provider
normalizedContext (NormalizedPlan context _ _) = context

normalizedBody :: OwnedPlan owner target entry provider Normalized -> ScopedBody EntryRootKind
normalizedBody (NormalizedPlan _ (EntryBody body) _) = body

normalizedReserved :: OwnedPlan owner target entry provider Normalized -> Set Text
normalizedReserved (NormalizedPlan _ _ reserved) = reserved

sourcePlanConfig :: SourcePlan -> TranslateConfig
sourcePlanConfig plan = withSourcePlan plan (contextConfig . normalizedContext)

-- Rebinding creates a fresh owner. It produces normalized input only, so every
-- generated member must be materialized and admitted again under the provider.
rebindSourcePlan :: RuntimeSelection -> SourcePlan -> SourcePlan
rebindSourcePlan provider plan = withSourcePlan plan $ \owned ->
  let config = (contextConfig (normalizedContext owned)) {translationRuntime = provider}
   in withContext config $ \context ->
        ownSourcePlan (sealSourcePlan context (EntryBody (normalizedBody owned)) (normalizedReserved owned))

instance Show SourcePlan where
  showsPrec precedence plan = withSourcePlan plan $ \owned ->
    showsPrec precedence (contextConfig (normalizedContext owned), normalizedBody owned, normalizedReserved owned)

instance Eq SourcePlan where
  left == right = withSourcePlan left $ \a -> withSourcePlan right $ \b ->
    contextConfig (normalizedContext a) == contextConfig (normalizedContext b)
      && normalizedBody a == normalizedBody b
      && normalizedReserved a == normalizedReserved b

-- Each existential is opened once at its materialization boundary. The root
-- and every enclosed statement share one nominal scope.
type role EntryBody nominal nominal

newtype EntryBody (owner :: Type) (entry :: Entry) = EntryBody (ScopedBody EntryRootKind)

entryBody :: EntryRoot owner entry scope -> [Statement scope] -> EntryBody owner entry
entryBody root = EntryBody . scopedBody (Control.entryRootBody root)

type role ScopedBody nominal

data ScopedBody (kind :: RootKind) = forall scope. ScopedBody (BodyRoot kind scope) [Statement scope]

data ForBody = forall scope. ForBody (Root scope) (LoopTarget scope) [Statement scope]

data WhileBody = forall scope. WhileBody (Root scope) (LoopTarget scope) [Statement scope] [Statement scope]

data ArithmeticBody = forall scope. ArithmeticBody (Root scope) (LoopTarget scope) (Statement scope) (Statement scope) [Statement scope]

scopedBody :: BodyRoot kind scope -> [Statement scope] -> ScopedBody kind
scopedBody = ScopedBody

withScopedBody :: ScopedBody kind -> (forall scope. Root scope -> [Statement scope] -> result) -> result
withScopedBody (ScopedBody root statements) consume = consume (Control.bodyRootWitness root) statements

forBody :: Root scope -> LoopTarget scope -> [Statement scope] -> ForBody
forBody = ForBody

withForBody :: ForBody -> (forall scope. Root scope -> LoopTarget scope -> [Statement scope] -> result) -> result
withForBody (ForBody root target statements) consume = consume root target statements

whileBody :: Root scope -> LoopTarget scope -> [Statement scope] -> [Statement scope] -> WhileBody
whileBody = WhileBody

withWhileBody :: WhileBody -> (forall scope. Root scope -> LoopTarget scope -> [Statement scope] -> [Statement scope] -> result) -> result
withWhileBody (WhileBody root target predicate statements) consume = consume root target predicate statements

arithmeticBody :: Root scope -> LoopTarget scope -> Statement scope -> Statement scope -> [Statement scope] -> ArithmeticBody
arithmeticBody = ArithmeticBody

withArithmeticBody :: ArithmeticBody -> (forall scope. Root scope -> LoopTarget scope -> Statement scope -> Statement scope -> [Statement scope] -> result) -> result
withArithmeticBody (ArithmeticBody root target predicate increment statements) consume = consume root target predicate increment statements

bodyEffects :: (Monoid effects) => (forall scope. Statement scope -> effects) -> ScopedBody kind -> effects
bodyEffects measure body = withScopedBody body (\_ -> foldMap measure)

instance Show (ScopedBody kind) where showsPrec precedence (ScopedBody _ statements) = showsPrec precedence statements

instance Show ForBody where showsPrec precedence (ForBody _ target statements) = showsPrec precedence (Control.loopKey target, statements)

instance Show WhileBody where showsPrec precedence (WhileBody _ target predicate statements) = showsPrec precedence (Control.loopKey target, predicate, statements)

instance Show ArithmeticBody where showsPrec precedence (ArithmeticBody _ target predicate increment statements) = showsPrec precedence (Control.loopKey target, predicate, increment, statements)

instance Eq (ScopedBody kind) where ScopedBody _ a == ScopedBody _ b = sameStatements a b

instance Eq ForBody where ForBody _ at a == ForBody _ bt b = Control.loopKey at == Control.loopKey bt && sameStatements a b

instance Eq WhileBody where WhileBody _ at ap a == WhileBody _ bt bp b = Control.loopKey at == Control.loopKey bt && sameStatements ap bp && sameStatements a b

instance Eq ArithmeticBody where ArithmeticBody _ at ap ai a == ArithmeticBody _ bt bp bi b = Control.loopKey at == Control.loopKey bt && sameStatement ap bp && sameStatement ai bi && sameStatements a b

sameStatements :: [Statement left] -> [Statement right] -> Bool
sameStatements left right = length left == length right && and (zipWith sameStatement left right)

type role Statement nominal

data Statement scope = Statement (Maybe SourceRange) (StatementNode scope)
  deriving stock (Show)

instance Eq (Statement scope) where (==) = sameStatement

type role StatementNode nominal

data StatementNode scope
  = Sequence [Statement scope]
  | AssignmentCommand Bool [Statement scope]
  | DeclarationCommand [Declaration]
  | Redirected [Redirection] (Statement scope)
  | Invoke CallTarget [Word]
  | PrefixedInvoke [(Storage, Text, Scalar)] CallTarget [Word]
  | Assign Storage Text Scalar
  | AssignArray Storage Text [Word]
  | AppendArray Storage Text [Word]
  | AssignArrayElement Storage Text Int Scalar
  | Erase Text
  | SetArguments (SetArgumentsTarget scope) [Word]
  | DirectoryOperation DirectoryOperation
  | SetOption Option Bool
  | And (Statement scope) (Statement scope)
  | Or (Statement scope) (Statement scope)
  | Negate (Statement scope)
  | Conditional [Statement scope] [Statement scope] [Statement scope]
  | WhileLoop Bool WhileBody
  | ForLoop Storage Text [Word] ForBody
  | ArithmeticFor (Statement scope) ArithmeticBody
  | ShiftArguments (ShiftTarget scope) Int
  | Case Scalar [CaseArm scope]
  | PatternCondition Bool Scalar Pattern
  | NumericCondition NumericComparison Scalar Scalar
  | Approximate Approximation [Statement scope]
  | DefineFunction Text (ScopedBody FunctionRootKind)
  | SourceBody SourceRequest (ScopedBody SourceRootKind)
  | Subshell ChildRegion
  | Pipeline (NonEmpty ChildRegion)
  | SupervisedPipeline (NonEmpty ChildRegion)
  | Background ChildRegion
  | Wait [Word]
  | Read ReadOptions ReadTarget
  | PrefixedRead [(Storage, Text, Scalar)] ReadOptions ReadTarget
  | SetTrap TrapKind (Maybe (ScopedBody HandlerRootKind))
  | ArithmeticCommand ArithmeticSite ArithmeticExpr (Map Text Storage)
  | Return (ReturnTarget scope) (Maybe Scalar)
  | Exit (Maybe Scalar)
  | Break (LoopTarget scope)
  | Continue (LoopTarget scope)
  deriving stock (Show)

-- Declaration operands expand before bindings are installed. The Boolean
-- records whether this declaration introduces a fresh function-local slot.
instance Eq (StatementNode scope) where
  left == right = sameStatement (Statement Nothing left) (Statement Nothing right)

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

-- | Existential cardinality is retained with its witness until field lowering.
data Word where
  MkWord :: SCardinality cardinality -> Value Fields cardinality -> Word

deriving stock instance Show Word

instance Eq Word where
  MkWord left a == MkWord right b = case testEquality left right of
    Just Refl -> a == b
    Nothing -> False

withWord :: Word -> (forall cardinality. Value Fields cardinality -> result) -> result
withWord (MkWord _ value) consume = consume value

pattern OneField :: Scalar -> Word
pattern OneField scalar = MkWord SExactlyOne (ScalarField scalar)

pattern SplitFields :: Scalar -> Word
pattern SplitFields scalar = MkWord SFieldSequence (SplitFieldValues scalar)

pattern PathnameFields :: Pattern -> Word
pattern PathnameFields value = MkWord SFieldSequence (PathnameFieldValues value)

pattern QuotedArguments :: Scalar -> Scalar -> Bool -> Word
pattern QuotedArguments before after force = MkWord SFieldSequence (QuotedArgumentValues before after force)

pattern QuotedArray :: Text -> Scalar -> Scalar -> Bool -> Word
pattern QuotedArray name before after force = MkWord SFieldSequence (QuotedArrayValues name before after force)

pattern ExpandedWord :: [ExpansionPart] -> Word
pattern ExpandedWord parts = MkWord SFieldSequence (ExpandedFieldValues parts)

{-# COMPLETE OneField, SplitFields, PathnameFields, QuotedArguments, QuotedArray, ExpandedWord #-}

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

type Scalar = Value Bytes ExactlyOne

-- | The canonical semantic value tree. Fields cannot enter scalar-only
-- positions and an exactly-one field carries that fact to materialization.
type role Value nominal nominal

data Value (domain :: Domain) (cardinality :: Cardinality) where
  Literal :: Text -> Scalar
  Variable :: Text -> Scalar
  ArrayElement :: Text -> Int -> Scalar
  ArrayLength :: Text -> Scalar
  Positional :: Int -> Scalar
  PositionalDefault :: Int -> Bool -> Scalar -> Scalar
  PositionalAlternate :: Int -> Bool -> Scalar -> Scalar
  ArgumentCount :: Scalar
  LastStatus :: Scalar
  LastBackgroundPid :: Scalar
  Concat :: [Scalar] -> Scalar
  Substitute :: ChildRegion -> Scalar
  ProcessSubstitution :: ProcessDirection -> ChildRegion -> Scalar
  DefaultValue :: Storage -> Text -> Bool -> Bool -> Scalar -> Scalar
  AlternateValue :: Text -> Bool -> Scalar -> Scalar
  ParameterTransform :: Replacement -> Scalar -> Text -> Text -> Scalar
  ParameterPatternTransform :: PatternTrim -> Scalar -> Pattern -> Scalar
  AppendValue :: Text -> Scalar -> Scalar
  ByteLiteral :: ByteString -> Scalar
  PlatformBytes :: ByteString -> ByteString -> Scalar
  ArithmeticValue :: ArithmeticSite -> ArithmeticExpr -> Map Text Storage -> Scalar
  ScalarField :: Scalar -> Value Fields ExactlyOne
  SplitFieldValues :: Scalar -> Value Fields FieldSequence
  PathnameFieldValues :: Pattern -> Value Fields FieldSequence
  QuotedArgumentValues :: Scalar -> Scalar -> Bool -> Value Fields FieldSequence
  QuotedArrayValues :: Text -> Scalar -> Scalar -> Bool -> Value Fields FieldSequence
  ExpandedFieldValues :: [ExpansionPart] -> Value Fields FieldSequence

deriving stock instance Show (Value domain cardinality)

deriving stock instance Eq (Value domain cardinality)

newtype Pattern = MkPattern [PatternPart]
  deriving stock (Show, Eq)

data PatternPart = LiteralPattern Scalar | ActivePattern Scalar
  deriving stock (Show, Eq)

data CaseArm scope = CaseArm [Pattern] [Statement scope] CaseEnd
  deriving stock (Show, Eq)

-- | An isolated execution region owns its finite normalized function closure
-- and the scalar names whose presence, attributes and values must be copied.
data ChildRegion = MkChildRegion
  { childRange :: Maybe SourceRange,
    childBody :: ScopedBody ChildRootKind,
    childFunctions :: Map Text (ScopedBody FunctionRootKind),
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
  { sourceEntryDefinitions :: Map Text DefinitionIdentity,
    sourceEntryVariables :: Set Text,
    sourceEntryConstants :: Map Text Text,
    sourceEntryArrays :: Map Text ArrayShape,
    sourceEntryLocals :: Set Text,
    sourceEntryNumericVariables :: Set Text,
    sourceEntryDirectoryFacts :: DirectoryFacts
  }
  deriving stock (Show, Eq, Ord)

-- Structural equality ignores generative scope names, never their payloads.
-- No scope coercion or parallel erased syntax tree is constructed.
sameStatement :: Statement left -> Statement right -> Bool
sameStatement (Statement ar a) (Statement br b) =
  ar == br && case (a, b) of
    (Sequence av, Sequence bv) -> sameStatements av bv
    (AssignmentCommand af av, AssignmentCommand bf bv) -> af == bf && sameStatements av bv
    (Redirected ad av, Redirected bd bv) -> ad == bd && sameStatement av bv
    (And al ar', And bl br') -> sameStatement al bl && sameStatement ar' br'
    (Or al ar', Or bl br') -> sameStatement al bl && sameStatement ar' br'
    (Negate av, Negate bv) -> sameStatement av bv
    (Conditional ap ay an, Conditional bp by bn) -> sameStatements ap bp && sameStatements ay by && sameStatements an bn
    (ArithmeticFor ai ab, ArithmeticFor bi bb) -> sameStatement ai bi && ab == bb
    (Case av aa, Case bv ba) -> av == bv && length aa == length ba && and (zipWith sameArm aa ba)
    (Approximate aa av, Approximate ba bv) -> aa == ba && sameStatements av bv
    (Return at av, Return bt bv) -> Control.sameReturnTarget at bt && av == bv
    (ShiftArguments at av, ShiftArguments bt bv) -> Control.sameShiftTarget at bt && av == bv
    (SetArguments _ av, SetArguments _ bv) -> av == bv
    (Break at, Break bt) -> Control.loopKey at == Control.loopKey bt
    (Continue at, Continue bt) -> Control.loopKey at == Control.loopKey bt
    (DeclarationCommand a0, DeclarationCommand b0) -> a0 == b0
    (Invoke a0 a1, Invoke b0 b1) -> a0 == b0 && a1 == b1
    (PrefixedInvoke a0 a1 a2, PrefixedInvoke b0 b1 b2) -> a0 == b0 && a1 == b1 && a2 == b2
    (Assign a0 a1 a2, Assign b0 b1 b2) -> a0 == b0 && a1 == b1 && a2 == b2
    (AssignArray a0 a1 a2, AssignArray b0 b1 b2) -> a0 == b0 && a1 == b1 && a2 == b2
    (AppendArray a0 a1 a2, AppendArray b0 b1 b2) -> a0 == b0 && a1 == b1 && a2 == b2
    (AssignArrayElement a0 a1 a2 a3, AssignArrayElement b0 b1 b2 b3) -> a0 == b0 && a1 == b1 && a2 == b2 && a3 == b3
    (Erase a0, Erase b0) -> a0 == b0
    (DirectoryOperation a0, DirectoryOperation b0) -> a0 == b0
    (SetOption a0 a1, SetOption b0 b1) -> a0 == b0 && a1 == b1
    (WhileLoop a0 a1, WhileLoop b0 b1) -> a0 == b0 && a1 == b1
    (ForLoop a0 a1 a2 a3, ForLoop b0 b1 b2 b3) -> a0 == b0 && a1 == b1 && a2 == b2 && a3 == b3
    (PatternCondition a0 a1 a2, PatternCondition b0 b1 b2) -> a0 == b0 && a1 == b1 && a2 == b2
    (NumericCondition a0 a1 a2, NumericCondition b0 b1 b2) -> a0 == b0 && a1 == b1 && a2 == b2
    (DefineFunction a0 a1, DefineFunction b0 b1) -> a0 == b0 && a1 == b1
    (SourceBody a0 a1, SourceBody b0 b1) -> a0 == b0 && a1 == b1
    (Subshell a0, Subshell b0) -> a0 == b0
    (Pipeline a0, Pipeline b0) -> a0 == b0
    (SupervisedPipeline a0, SupervisedPipeline b0) -> a0 == b0
    (Background a0, Background b0) -> a0 == b0
    (Wait a0, Wait b0) -> a0 == b0
    (Read a0 a1, Read b0 b1) -> a0 == b0 && a1 == b1
    (PrefixedRead a0 a1 a2, PrefixedRead b0 b1 b2) -> a0 == b0 && a1 == b1 && a2 == b2
    (SetTrap a0 a1, SetTrap b0 b1) -> a0 == b0 && a1 == b1
    (ArithmeticCommand a0 a1 a2, ArithmeticCommand b0 b1 b2) -> a0 == b0 && a1 == b1 && a2 == b2
    (Exit a0, Exit b0) -> a0 == b0
    _ -> False
  where
    sameArm :: CaseArm left -> CaseArm right -> Bool
    sameArm (CaseArm ap av ae) (CaseArm bp bv be) = ap == bp && sameStatements av bv && ae == be

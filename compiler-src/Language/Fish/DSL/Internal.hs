{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 914
{-# OPTIONS_GHC -Wno-pattern-namespace-specifier #-}
#endif
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Fish.DSL.Internal
  ( -- * Core types
    FishType (..),
    FishStatement (..),
    FishCommand (..),
    CommandGrammar (..),
    StageGrammar,
    BackgroundGrammar,
    stageOfStatement,
    stageToStatement,
    ExprOrRedirect (..),
    FishExpr (..),
    UnquotedResult,
    FishVarRef (..),
    CommandName,
    commandNameText,
    Executable,
    literalExecutable,
    variableExecutable,
    foldExecutable,
    Identifier,
    identifierText,
    FishIndex (..),

    -- * Special variables and helpers
    SpecialVarRef (..),
    GlobPattern (..),
    GlobPart (..),
    StringOp (..),
    ReadFlag (..),
    SetFlag (..),

    -- * Job model
    VariableAssignment (..),
    FishJobPipeline (..),
    pattern MkFishJobPipeline,
    JobPipeCont (..),
    pattern PipeTo,
    pattern PipeErrorTo,
    Conjunction (..),
    FishJobConjCont (..),
    FishJobConjunction (..),
    FishJobList (..),

    -- * Type synonyms
    CmdStr,
    CmdInt,
    CmdBool,
    CmdList,
    CmdStatus,
    CmdUnit,
    ExprStr,
    ExprInt,
    ExprBool,
    ExprList,
    ExprStatus,
    ExprUnit,

    -- * Operators and flags
    Redirect (..),
    RedirectDirection (..),
    FileRedirectMode (..),
    OutputRedirectMode (..),
    RedirectForm,
    buildRedirect,
    StreamKind (..),
    ModeKind (..),
    TargetKind (..),
    RedirectTarget (..),
    Decoration (..),
    CaseItem (..),

    -- * Functions
    FishFunction (..),
    FunctionFlag (..),

    -- * Source tracking
    SourcePos (..),
    SourceRange (..),

    -- * Equality helpers
    eqGADT,
    eqFishExprSameType,

    -- * Public DSL views
    Expr,
    Arg,
    ArgumentType (..),
    Command,
    CommandRole (..),
    CommandResult,
    Stmt,
    Block (..),
    Stage (..),
    Pipeline,
    JobContinuation,
    JobConjunction,
    JobList,
    Script (..),
    Index (..),
    IndexShape (..),
    IndexResult,
    RedirectStream (..),
    RedirectMode (..),
  )
where

import Data.Type.Equality (testEquality, (:~:) (Refl))
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Language.Fish.DSL.Executable
import Language.Fish.DSL.Name (Identifier, identifierText)
import Language.Fish.DSL.Types
import Type.Reflection (typeRep)

-- | Top-level statements in a fish script.

--------------------------------------------------------------------------------

data FishStatement where
  -- | A single command or block.
  Stmt :: (Typeable a) => FishCommand grammar a -> FishStatement
  -- | A list of statements (used for scripts and block bodies).
  StmtList :: [FishStatement] -> FishStatement
  -- | A line comment (without the leading @#@ character).
  Comment :: Text -> FishStatement
  -- | An explicit empty statement.
  EmptyStmt :: FishStatement

deriving stock instance Show FishStatement

instance Eq FishStatement where
  Stmt c1 == Stmt c2 = eqCommand c1 c2
  (StmtList xs1) == (StmtList xs2) = xs1 == xs2
  (Comment t1) == (Comment t2) = t1 == t2
  EmptyStmt == EmptyStmt = True
  _ == _ = False

--------------------------------------------------------------------------------

-- | Fish commands and block forms.

--------------------------------------------------------------------------------

data CommandGrammar = Atomic | ControlGrammar | BlockGrammar | Compound | Asynchronous | Definition

type family StageGrammar (grammar :: CommandGrammar) :: Constraint where
  StageGrammar Atomic = ()
  StageGrammar BlockGrammar = ()
  StageGrammar grammar = TypeError ('Text "Illegal pipeline stage grammar")

-- | A second background marker cannot be appended to an asynchronous job.
type family BackgroundGrammar (grammar :: CommandGrammar) :: Constraint where
  BackgroundGrammar Atomic = ()
  BackgroundGrammar BlockGrammar = ()
  BackgroundGrammar Compound = ()
  BackgroundGrammar ControlGrammar = ()
  BackgroundGrammar Definition = ()
  BackgroundGrammar grammar = TypeError ('Text "An asynchronous job cannot be backgrounded again")

type role FishCommand nominal nominal

data FishCommand (grammar :: CommandGrammar) (t :: FishType) where
  -- | Basic commands and blocks.
  CommandSearch :: CommandName -> FishCommand Atomic TStatus
  Command :: CommandName -> [ExprOrRedirect] -> FishCommand Atomic TStatus
  -- | An already admitted scalar executable capability, never source text.
  CommandExpr :: Executable -> [ExprOrRedirect] -> FishCommand Atomic TStatus
  Set :: [SetFlag] -> Identifier -> FishExpr (TList TStr) -> FishCommand Atomic TUnit
  Function :: FishFunction -> FishCommand Definition TUnit
  For ::
    Identifier ->
    FishExpr (TList TStr) ->
    NonEmpty FishStatement ->
    [Redirect] ->
    FishCommand BlockGrammar TStatus
  While ::
    FishJobList ->
    NonEmpty FishStatement ->
    [Redirect] ->
    FishCommand BlockGrammar TStatus
  Begin :: NonEmpty FishStatement -> [Redirect] -> FishCommand BlockGrammar TStatus
  If ::
    FishJobList ->
    NonEmpty FishStatement ->
    [FishStatement] ->
    [Redirect] ->
    FishCommand BlockGrammar TStatus
  Switch ::
    FishExpr TStr ->
    NonEmpty CaseItem ->
    [Redirect] ->
    FishCommand BlockGrammar TStatus
  Break :: FishCommand ControlGrammar TUnit
  Continue :: FishCommand ControlGrammar TUnit
  ReturnScalar :: FishExpr TStr -> FishCommand ControlGrammar TStatus
  Return :: Maybe (FishExpr TInt) -> FishCommand ControlGrammar TStatus
  -- | Control flow and environment.
  Exit :: Maybe (FishExpr TInt) -> FishCommand Atomic TStatus
  Source :: FishExpr TStr -> FishCommand Atomic TStatus
  Eval :: FishExpr TStr -> FishCommand Atomic TStatus
  -- | IO commands.
  Read :: [ReadFlag] -> [Identifier] -> FishCommand Atomic TStatus
  Echo :: NonEmpty (FishExpr TStr) -> FishCommand Atomic TUnit
  Printf :: FishExpr TStr -> [FishExpr TStr] -> FishCommand Atomic TUnit
  -- | Combining commands and job control.
  Pipeline :: FishJobPipeline -> FishCommand Compound TStatus
  JobConj :: FishJobConjunction -> FishCommand Compound TStatus
  Semicolon ::
    (Typeable a, Typeable b) =>
    FishCommand leftGrammar a ->
    FishCommand rightGrammar b ->
    FishCommand Compound b
  Not :: (StageGrammar grammar) => FishCommand grammar TStatus -> FishCommand BlockGrammar TStatus
  Background :: (Typeable a, BackgroundGrammar grammar) => FishCommand grammar a -> FishCommand Asynchronous TStatus
  Wait :: Maybe (FishExpr TInt) -> FishCommand Atomic TStatus
  Exec :: Executable -> [ExprOrRedirect] -> FishCommand ControlGrammar TStatus
  -- | Command decoration (@builtin@, @command@, @exec@).
  Decorated ::
    (Typeable a) =>
    Decoration ->
    FishCommand Atomic a ->
    FishCommand Atomic a

deriving stock instance Show (FishCommand grammar t)

instance (Typeable t) => Eq (FishCommand grammar t) where
  (==) = eqGADT eqFishCommandSameType

eqFishCommandSameType :: FishCommand leftGrammar a -> FishCommand rightGrammar a -> Bool
eqFishCommandSameType (Command txt1 args1) (Command txt2 args2) = txt1 == txt2 && args1 == args2
eqFishCommandSameType (CommandSearch left) (CommandSearch right) = left == right
eqFishCommandSameType (CommandExpr head1 args1) (CommandExpr head2 args2) = head1 == head2 && args1 == args2
eqFishCommandSameType (Set f1 v1 e1) (Set f2 v2 e2) = f1 == f2 && v1 == v2 && eqFishExpr e1 e2
eqFishCommandSameType (Function f1) (Function f2) = f1 == f2
eqFishCommandSameType (For v1 l1 b1 r1) (For v2 l2 b2 r2) = v1 == v2 && eqFishExpr l1 l2 && b1 == b2 && r1 == r2
eqFishCommandSameType (While c1 b1 r1) (While c2 b2 r2) = c1 == c2 && b1 == b2 && r1 == r2
eqFishCommandSameType (Begin s1 r1) (Begin s2 r2) = s1 == s2 && r1 == r2
eqFishCommandSameType (If c1 t1 e1 r1) (If c2 t2 e2 r2) = c1 == c2 && t1 == t2 && e1 == e2 && r1 == r2
eqFishCommandSameType (Switch e1 cs1 r1) (Switch e2 cs2 r2) = eqFishExpr e1 e2 && cs1 == cs2 && r1 == r2
eqFishCommandSameType Break Break = True
eqFishCommandSameType Continue Continue = True
eqFishCommandSameType (ReturnScalar e1) (ReturnScalar e2) = e1 == e2
eqFishCommandSameType (Return e1) (Return e2) = case (e1, e2) of
  (Nothing, Nothing) -> True
  (Just x, Just y) -> eqFishExpr x y
  _ -> False
eqFishCommandSameType (Exit x1) (Exit x2) = case (x1, x2) of
  (Nothing, Nothing) -> True
  (Just a, Just b) -> eqFishExpr a b
  _ -> False
eqFishCommandSameType (Source e1) (Source e2) = eqFishExpr e1 e2
eqFishCommandSameType (Eval e1) (Eval e2) = eqFishExpr e1 e2
eqFishCommandSameType (Read f1 vs1) (Read f2 vs2) = f1 == f2 && vs1 == vs2
eqFishCommandSameType (Echo es1) (Echo es2) = es1 == es2
eqFishCommandSameType (Printf f1 a1) (Printf f2 a2) = eqFishExpr f1 f2 && a1 == a2
eqFishCommandSameType (Pipeline p1) (Pipeline p2) = p1 == p2
eqFishCommandSameType (JobConj j1) (JobConj j2) = j1 == j2
eqFishCommandSameType (Semicolon c1a c1b) (Semicolon c2a c2b) = eqCommand c1a c2a && eqFishCommandSameType c1b c2b
eqFishCommandSameType (Not c1) (Not c2) = eqFishCommandSameType c1 c2
eqFishCommandSameType (Background c1) (Background c2) = eqCommand c1 c2
eqFishCommandSameType (Wait a1) (Wait a2) = case (a1, a2) of
  (Nothing, Nothing) -> True
  (Just x, Just y) -> eqFishExpr x y
  _ -> False
eqFishCommandSameType (Exec c1 a1) (Exec c2 a2) = c1 == c2 && a1 == a2
eqFishCommandSameType (Decorated d1 c1) (Decorated d2 c2) = d1 == d2 && eqFishCommandSameType c1 c2
-- \| Fallback for constructors not matching or different types.
eqFishCommandSameType _ _ = False

--------------------------------------------------------------------------------

-- | Expressions and command arguments.

--------------------------------------------------------------------------------

-- | A command argument or redirection.
data ExprOrRedirect where
  ExprVal :: (Typeable a) => FishExpr a -> ExprOrRedirect
  RedirectVal :: Redirect -> ExprOrRedirect

deriving stock instance Show ExprOrRedirect

instance Eq ExprOrRedirect where
  (ExprVal e1) == (ExprVal e2) = eqFishExpr e1 e2
  (RedirectVal r1) == (RedirectVal r2) = r1 == r2
  _ == _ = False

-- | GADT for fish expressions.
-- | Unquoted expansion has list cardinality even for a scalar/index reference.
type family UnquotedResult (value :: FishType) :: FishType where
  UnquotedResult TStr = TList TStr
  UnquotedResult (TList element) = TList element
  UnquotedResult other = other

type role FishExpr nominal

data FishExpr (t :: FishType) where
  -- | Literals.
  ExprLiteral :: Text -> FishExpr TStr
  -- | Owned child source, quoted as literal transport data only by the renderer.
  ExprEmbeddedScript :: Script -> FishExpr TStr
  ExprNumLiteral :: Int -> FishExpr TInt
  -- | Variables.
  ExprVariable :: FishVarRef t -> FishExpr (UnquotedResult t)
  -- | Exactly one quoted field, including an unset or empty scalar.
  ExprQuotedVariable :: FishVarRef TStr -> FishExpr TStr
  -- | Special variables.
  ExprSpecialVar :: SpecialVarRef t -> FishExpr t
  -- | String operations.
  ExprStringConcat :: FishExpr TStr -> FishExpr TStr -> FishExpr TStr
  ExprStringOp :: StringOp -> FishExpr TStr -> FishExpr (TList TStr)
  ExprJoinList :: FishExpr (TList TStr) -> FishExpr TStr
  -- | Resolve a path relative to the Fish file currently being sourced.
  ExprFileRelative :: Text -> FishExpr TStr
  -- | Arithmetic operations.
  ExprMath :: NonEmpty (FishExpr TStr) -> FishExpr TInt
  -- | Command substitution (fish produces a list of strings).
  ExprCommandSubst :: NonEmpty FishStatement -> FishExpr (TList TStr)
  -- | Quoted capture retains one field and embedded newlines.
  ExprQuotedCommandSubst :: NonEmpty FishStatement -> FishExpr TStr
  -- | List operations.
  ExprListLiteral :: [FishExpr TStr] -> FishExpr (TList TStr)
  ExprListConcat :: (Typeable a) => FishExpr (TList a) -> FishExpr (TList a) -> FishExpr (TList a)
  -- | Glob patterns.
  ExprGlob :: GlobPattern -> FishExpr (TList TStr)
  -- | Process substitution (psub).
  ExprProcessSubst :: NonEmpty FishStatement -> FishExpr TStr

deriving stock instance Show (FishExpr t)

-- | Eq instance using eqGADT helper.
instance (Typeable t) => Eq (FishExpr t) where
  (==) = eqGADT eqFishExprSameType

-- | Helper to compare FishExpr when types are known to be equal (a ~ b).
eqFishExprSameType :: FishExpr a -> FishExpr a -> Bool
eqFishExprSameType (ExprLiteral t1) (ExprLiteral t2) = t1 == t2
eqFishExprSameType (ExprEmbeddedScript a) (ExprEmbeddedScript b) = a == b
eqFishExprSameType (ExprNumLiteral n1) (ExprNumLiteral n2) = n1 == n2
eqFishExprSameType (ExprVariable v1) (ExprVariable v2) = eqVarRef v1 v2
eqFishExprSameType (ExprQuotedVariable v1) (ExprQuotedVariable v2) = v1 == v2
eqFishExprSameType (ExprStringConcat x1 y1) (ExprStringConcat x2 y2) = x1 == x2 && y1 == y2
eqFishExprSameType (ExprStringOp o1 a1) (ExprStringOp o2 a2) = o1 == o2 && a1 == a2
eqFishExprSameType (ExprJoinList a1) (ExprJoinList a2) = eqFishExpr a1 a2
eqFishExprSameType (ExprFileRelative p1) (ExprFileRelative p2) = p1 == p2
eqFishExprSameType (ExprMath xs1) (ExprMath xs2) = xs1 == xs2
eqFishExprSameType (ExprCommandSubst s1) (ExprCommandSubst s2) = s1 == s2
eqFishExprSameType (ExprQuotedCommandSubst s1) (ExprQuotedCommandSubst s2) = s1 == s2
eqFishExprSameType (ExprListLiteral s1) (ExprListLiteral s2) = s1 == s2
eqFishExprSameType (ExprListConcat a1 b1) (ExprListConcat a2 b2) = eqFishExpr a1 a2 && eqFishExpr b1 b2
eqFishExprSameType (ExprGlob g1) (ExprGlob g2) = g1 == g2
eqFishExprSameType (ExprProcessSubst s1) (ExprProcessSubst s2) = s1 == s2
eqFishExprSameType (ExprSpecialVar v1) (ExprSpecialVar v2) = v1 == v2
eqFishExprSameType _ _ = False

-- | Heterogeneous equality check for FishExpr using Typeable.
eqFishExpr :: forall a b. (Typeable a, Typeable b) => FishExpr a -> FishExpr b -> Bool
eqFishExpr = eqGADT eqFishExprSameType

--------------------------------------------------------------------------------

-- | Variables and indexing.

--------------------------------------------------------------------------------

-- | Variable references.
data FishVarRef (t :: FishType) where
  VarAll :: Identifier -> FishVarRef (TList TStr)
  VarScalar :: Identifier -> FishVarRef TStr
  VarIndex :: Identifier -> FishIndex TStr t -> FishVarRef t

deriving stock instance Show (FishVarRef t)

instance Eq (FishVarRef t) where
  (==) = eqVarRef

eqVarRef :: FishVarRef left -> FishVarRef right -> Bool
eqVarRef (VarAll a) (VarAll b) = a == b
eqVarRef (VarScalar a) (VarScalar b) = a == b
eqVarRef (VarIndex a i) (VarIndex b j) = a == b && eqIndex i j
eqVarRef _ _ = False

eqIndex :: FishIndex a left -> FishIndex a right -> Bool
eqIndex (IndexSingle a) (IndexSingle b) = a == b
eqIndex (IndexRange a b) (IndexRange c d) = a == c && b == d
eqIndex (IndexList a) (IndexList b) = a == b
eqIndex _ _ = False

-- | Indexing into a list (supports ranges and index lists).
data FishIndex (a :: FishType) (b :: FishType) where
  IndexSingle :: FishExpr TInt -> FishIndex a a
  IndexRange :: Maybe (FishExpr TInt) -> Maybe (FishExpr TInt) -> FishIndex a (TList a)
  IndexList :: NonEmpty (FishExpr TInt) -> FishIndex a (TList a)

deriving stock instance Show (FishIndex a b)

instance Eq (FishIndex a b) where
  IndexSingle a1 == IndexSingle a2 = a1 == a2
  IndexRange s1 e1 == IndexRange s2 e2 = s1 == s2 && e1 == e2
  IndexList xs1 == IndexList xs2 = xs1 == xs2
  _ == _ = False

-- | Function definition.
data FishFunction = MkFishFunction
  { funcName :: Text,
    funcFlags :: [FunctionFlag],
    funcParams :: [Identifier],
    funcBody :: NonEmpty FishStatement
  }
  deriving stock (Show, Eq)

-- | Job model (pipelines and conjunctions).

--------------------------------------------------------------------------------

-- | Variable assignment attached to a job or continuation.
data VariableAssignment = MkVariableAssignment
  { vaName :: Identifier,
    vaValue :: Maybe (FishExpr TStr)
  }
  deriving stock (Show, Eq)

-- | Pipeline continuation: @|@ followed by variables and a statement.
data JobPipeCont
  = PipeToStage
      { jpcVariables :: [VariableAssignment],
        jpcStatement :: Stage
      }
  | PipeErrorToStage
      { jpcVariables :: [VariableAssignment],
        jpcStatement :: Stage
      }
  deriving stock (Show, Eq)

-- | Foreground pipeline with optional @time@ and leading variables.
-- Backgrounding belongs exclusively to the indexed Background command.
data FishJobPipeline = JobPipeline
  { jpTime :: Bool,
    jpVariables :: [VariableAssignment],
    jpStatement :: Stage,
    jpCont :: [JobPipeCont]
  }
  deriving stock (Show, Eq)

pattern PipeTo :: [VariableAssignment] -> FishStatement -> JobPipeCont
pattern PipeTo variables statement <- PipeToStage variables (stageToStatement -> statement)
  where
    PipeTo variables statement = PipeToStage variables (stageOfStatement statement)

pattern PipeErrorTo :: [VariableAssignment] -> FishStatement -> JobPipeCont
pattern PipeErrorTo variables statement <- PipeErrorToStage variables (stageToStatement -> statement)
  where
    PipeErrorTo variables statement = PipeErrorToStage variables (stageOfStatement statement)

{-# COMPLETE PipeTo, PipeErrorTo #-}

pattern MkFishJobPipeline :: Bool -> [VariableAssignment] -> FishStatement -> [JobPipeCont] -> FishJobPipeline
pattern MkFishJobPipeline timed variables statement rest <- JobPipeline timed variables (stageToStatement -> statement) rest
  where
    MkFishJobPipeline timed variables statement rest =
      case (timed, variables, statement, rest) of
        (False, [], Stmt (Pipeline pipeline), []) -> pipeline
        _ -> JobPipeline timed variables (stageOfStatement statement) rest

{-# COMPLETE MkFishJobPipeline #-}

-- | Conjunction continuation: @and job@ or @or job@.
data FishJobConjCont
  = JCAnd FishJobPipeline
  | JCOr FishJobPipeline
  deriving stock (Show, Eq)

-- | A job conjunction consisting of an optional leading decorator and continuations.
data FishJobConjunction = MkFishJobConjunction
  { jcDecorator :: Maybe Conjunction,
    jcJob :: FishJobPipeline,
    jcContinuations :: [FishJobConjCont]
  }
  deriving stock (Show, Eq)

-- | A list of job conjunctions (used for if/while conditions).
newtype FishJobList = MkFishJobList (NonEmpty FishJobConjunction)
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------

-- | Case items and redirections.

--------------------------------------------------------------------------------

-- | A switch case item with one or more patterns and a body.
data CaseItem = MkCaseItem
  { casePatterns :: NonEmpty (FishExpr TStr),
    caseBody :: NonEmpty FishStatement
  }
  deriving stock (Show, Eq)

-- | Canonical legal Fish redirection forms. Descriptor duplication cannot
-- accidentally inherit append or clobber syntax, and combined streams only
-- open output files. Descriptor numbers are nonnegative by construction.
data Redirect
  = FileRedirect Natural FileRedirectMode (FishExpr TStr)
  | BothFileRedirect OutputRedirectMode (FishExpr TStr)
  | DuplicateRedirect Natural RedirectDirection Natural
  | CloseRedirect Natural RedirectDirection
  deriving stock (Show, Eq)

data RedirectDirection = ReadFrom | WriteTo
  deriving stock (Show, Eq)

data FileRedirectMode = OverwriteFile | AppendFile | InputFile | ClobberFile
  deriving stock (Show, Eq)

data OutputRedirectMode = OutputOverwrite | OutputAppend | OutputClobber
  deriving stock (Show, Eq)

data TargetKind = FileTarget | DescriptorTarget

data RedirectTarget (kind :: TargetKind) where
  RedirectFile :: FishExpr TStr -> RedirectTarget FileTarget
  RedirectTargetFD :: Natural -> RedirectTarget DescriptorTarget
  RedirectClose :: RedirectTarget DescriptorTarget

deriving stock instance Show (RedirectTarget kind)

deriving stock instance Eq (RedirectTarget kind)

--------------------------------------------------------------------------------

-- | Equality helper for GADTs.

--------------------------------------------------------------------------------

eqGADT ::
  forall a b f.
  (Typeable a, Typeable b) =>
  (forall x. f x -> f x -> Bool) ->
  f a ->
  f b ->
  Bool
eqGADT eqSameType left right =
  case testEquality (typeRep @a) (typeRep @b) of
    Just Refl -> eqSameType left (coerce right)
    Nothing -> False

--------------------------------------------------------------------------------

-- | Type synonyms for convenience.

--------------------------------------------------------------------------------

-- | Command producing a string value.
type CmdStr grammar = FishCommand grammar TStr

-- | Command producing an integer value.
type CmdInt grammar = FishCommand grammar TInt

-- | Command producing a boolean value.
type CmdBool grammar = FishCommand grammar TBool

-- | Command producing a list value.
type CmdList grammar a = FishCommand grammar (TList a)

-- | Command producing an exit status.
type CmdStatus grammar = FishCommand grammar TStatus

-- | Command producing unit.
type CmdUnit grammar = FishCommand grammar TUnit

-- | Expression producing a string value.
type ExprStr = FishExpr TStr

-- | Expression producing an integer value.
type ExprInt = FishExpr TInt

-- | Expression producing a boolean value.
type ExprBool = FishExpr TBool

-- | Expression producing a list value.
type ExprList a = FishExpr (TList a)

-- | Expression producing an exit status.
type ExprStatus = FishExpr TStatus

-- | Expression producing unit.
type ExprUnit = FishExpr TUnit

--------------------------------------------------------------------------------

-- | Public DSL aliases and invariant-carrying wrappers.

--------------------------------------------------------------------------------

type Expr = FishExpr

type Arg = ExprOrRedirect

class ArgumentType (t :: FishType) where
  argumentTypeWitness :: Proxy t -> ()

instance ArgumentType TStr where
  argumentTypeWitness _ = ()

instance ArgumentType TInt where
  argumentTypeWitness _ = ()

instance ArgumentType (TList TStr) where
  argumentTypeWitness _ = ()

instance ArgumentType (TList TInt) where
  argumentTypeWitness _ = ()

data CommandRole
  = ReturnsStatus
  | ReturnsUnit
  deriving stock (Show, Eq)

type family CommandResult (r :: CommandRole) = (result :: FishType) | result -> r where
  CommandResult ReturnsStatus = TStatus
  CommandResult ReturnsUnit = TUnit

type Command grammar r = FishCommand grammar (CommandResult r)

type Stmt = FishStatement

newtype Block = MkBlock (NonEmpty Stmt)
  deriving stock (Show, Eq)

-- | Pipeline stages admit atomic commands and blocks only. A command list or
-- background job must be placed explicitly inside an owned block.
data Stage where
  MkStage :: (Typeable t, StageGrammar grammar) => FishCommand grammar t -> Stage

deriving stock instance Show Stage

instance Eq Stage where
  MkStage left == MkStage right = eqCommand left right

eqCommand :: forall leftGrammar rightGrammar a b. (Typeable a, Typeable b) => FishCommand leftGrammar a -> FishCommand rightGrammar b -> Bool
eqCommand left right = case testEquality (typeRep @a) (typeRep @b) of
  Just Refl -> eqFishCommandSameType left right
  Nothing -> False

stageToStatement :: Stage -> FishStatement
stageToStatement (MkStage command) = Stmt command

-- | Internal statement builders close command lists into a block before they
-- become a stage; public callers select the grammar in the command's type.
stageOfStatement :: FishStatement -> Stage
stageOfStatement statement@(Stmt command) = case command of
  CommandSearch {} -> MkStage command
  Command {} -> MkStage command
  CommandExpr {} -> MkStage command
  Set {} -> MkStage command
  For {} -> MkStage command
  While {} -> MkStage command
  Begin {} -> MkStage command
  If {} -> MkStage command
  Switch {} -> MkStage command
  Exit {} -> MkStage command
  Source {} -> MkStage command
  Eval {} -> MkStage command
  Read {} -> MkStage command
  Echo {} -> MkStage command
  Printf {} -> MkStage command
  Not {} -> MkStage command
  Wait {} -> MkStage command
  Decorated {} -> MkStage command
  _ -> MkStage (Begin (statement :| []) [])
stageOfStatement statement = MkStage (Begin (statement :| []) [])

type Pipeline = FishJobPipeline

type JobContinuation = FishJobConjCont

type JobConjunction = FishJobConjunction

type JobList = FishJobList

newtype Script = MkScript [Stmt]
  deriving stock (Show, Eq)

data IndexShape
  = IndexOneShape
  | IndexRangeShape
  | IndexManyShape
  deriving stock (Show, Eq)

type family IndexResult (shape :: IndexShape) (element :: FishType) :: FishType where
  IndexResult IndexOneShape element = element
  IndexResult IndexRangeShape element = TList element
  IndexResult IndexManyShape element = TList element

data Index (shape :: IndexShape) where
  MkIndexSingle :: Expr TInt -> Index IndexOneShape
  MkIndexRange :: Maybe (Expr TInt) -> Maybe (Expr TInt) -> Index IndexRangeShape
  MkIndexList :: NonEmpty (Expr TInt) -> Index IndexManyShape

deriving stock instance Show (Index shape)

instance Eq (Index shape) where
  MkIndexSingle left == MkIndexSingle right = left == right
  MkIndexRange leftStart leftEnd == MkIndexRange rightStart rightEnd =
    leftStart == rightStart && leftEnd == rightEnd
  MkIndexList left == MkIndexList right = left == right

data StreamKind = SingleStream | CombinedStreams

data RedirectStream (kind :: StreamKind) where
  DescriptorStream :: Natural -> RedirectStream SingleStream
  BothStreams :: RedirectStream CombinedStreams

deriving stock instance Show (RedirectStream kind)

deriving stock instance Eq (RedirectStream kind)

data ModeKind = OverwriteMode | AppendRedirectMode | InputMode | ClobberMode

data RedirectMode (kind :: ModeKind) where
  Overwrite :: RedirectMode OverwriteMode
  Append :: RedirectMode AppendRedirectMode
  Input :: RedirectMode InputMode
  Clobber :: RedirectMode ClobberMode

deriving stock instance Show (RedirectMode kind)

deriving stock instance Eq (RedirectMode kind)

-- | Closed admissibility cannot be extended by downstream orphan instances.
type RedirectForm stream mode target = (AllowedRedirect stream mode target, BuildRedirect stream mode target)

type family AllowedRedirect stream mode target :: Constraint where
  AllowedRedirect SingleStream OverwriteMode FileTarget = ()
  AllowedRedirect SingleStream AppendRedirectMode FileTarget = ()
  AllowedRedirect SingleStream InputMode FileTarget = ()
  AllowedRedirect SingleStream ClobberMode FileTarget = ()
  AllowedRedirect CombinedStreams OverwriteMode FileTarget = ()
  AllowedRedirect CombinedStreams AppendRedirectMode FileTarget = ()
  AllowedRedirect CombinedStreams ClobberMode FileTarget = ()
  AllowedRedirect SingleStream OverwriteMode DescriptorTarget = ()
  AllowedRedirect SingleStream InputMode DescriptorTarget = ()
  AllowedRedirect stream mode target = TypeError ('Text "Illegal redirect form")

class BuildRedirect stream mode target where
  buildRedirect :: RedirectStream stream -> RedirectMode mode -> RedirectTarget target -> Redirect

instance BuildRedirect SingleStream OverwriteMode FileTarget where
  buildRedirect (DescriptorStream fd) Overwrite (RedirectFile path) = FileRedirect fd OverwriteFile path

instance BuildRedirect SingleStream AppendRedirectMode FileTarget where
  buildRedirect (DescriptorStream fd) Append (RedirectFile path) = FileRedirect fd AppendFile path

instance BuildRedirect SingleStream InputMode FileTarget where
  buildRedirect (DescriptorStream fd) Input (RedirectFile path) = FileRedirect fd InputFile path

instance BuildRedirect SingleStream ClobberMode FileTarget where
  buildRedirect (DescriptorStream fd) Clobber (RedirectFile path) = FileRedirect fd ClobberFile path

instance BuildRedirect CombinedStreams OverwriteMode FileTarget where
  buildRedirect BothStreams Overwrite (RedirectFile path) = BothFileRedirect OutputOverwrite path

instance BuildRedirect CombinedStreams AppendRedirectMode FileTarget where
  buildRedirect BothStreams Append (RedirectFile path) = BothFileRedirect OutputAppend path

instance BuildRedirect CombinedStreams ClobberMode FileTarget where
  buildRedirect BothStreams Clobber (RedirectFile path) = BothFileRedirect OutputClobber path

instance BuildRedirect SingleStream OverwriteMode DescriptorTarget where
  buildRedirect (DescriptorStream fd) Overwrite (RedirectTargetFD target) = DuplicateRedirect fd WriteTo target
  buildRedirect (DescriptorStream fd) Overwrite RedirectClose = CloseRedirect fd WriteTo

instance BuildRedirect SingleStream InputMode DescriptorTarget where
  buildRedirect (DescriptorStream fd) Input (RedirectTargetFD target) = DuplicateRedirect fd ReadFrom target
  buildRedirect (DescriptorStream fd) Input RedirectClose = CloseRedirect fd ReadFrom

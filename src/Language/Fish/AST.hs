{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Language.Fish.AST
  ( -- * Core types
    FishType (..),
    FishStatement (..),
    FishCommand (..),
    ExprOrRedirect (..),
    FishExpr (..),
    FishVarRef (..),
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
    JobPipeCont (..),
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
    RedirectOp (..),
    RedirectSource (..),
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

    -- * Example
    exampleAST,
  )
where

import Data.Type.Equality (testEquality, (:~:) (Refl))
import Type.Reflection (typeRep)

--------------------------------------------------------------------------------

-- | Phantom-typed tags for expressions and commands.

--------------------------------------------------------------------------------

data FishType
  = TStr
  | TInt
  | TBool
  | TList FishType
  | TStatus
  | TUnit
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------

-- | Top-level statements in a fish script.

--------------------------------------------------------------------------------

data FishStatement where
  -- | A single command or block.
  Stmt :: (Typeable a) => FishCommand a -> FishStatement
  -- | A list of statements (used for scripts and block bodies).
  StmtList :: [FishStatement] -> FishStatement
  -- | A line comment (without the leading '#').
  Comment :: Text -> FishStatement
  -- | An explicit empty statement.
  EmptyStmt :: FishStatement

deriving stock instance Show FishStatement

instance Eq FishStatement where
  (Stmt (c1 :: FishCommand a)) == (Stmt (c2 :: FishCommand b)) =
    case testEquality (typeRep @a) (typeRep @b) of
      Just Refl -> c1 == c2
      Nothing -> False
  (StmtList xs1) == (StmtList xs2) = xs1 == xs2
  (Comment t1) == (Comment t2) = t1 == t2
  EmptyStmt == EmptyStmt = True
  _ == _ = False

--------------------------------------------------------------------------------

-- | Fish commands and block forms.

--------------------------------------------------------------------------------

data FishCommand (t :: FishType) where
  -- | Basic commands and blocks.
  Command :: Text -> [ExprOrRedirect] -> FishCommand TStatus
  Set :: [SetFlag] -> Text -> FishExpr (TList TStr) -> FishCommand TUnit
  Function :: FishFunction -> FishCommand TUnit
  For ::
    Text ->
    FishExpr (TList TStr) ->
    NonEmpty FishStatement ->
    [ExprOrRedirect] ->
    FishCommand TStatus
  While ::
    FishJobList ->
    NonEmpty FishStatement ->
    [ExprOrRedirect] ->
    FishCommand TStatus
  Begin :: NonEmpty FishStatement -> [ExprOrRedirect] -> FishCommand TStatus
  If ::
    FishJobList ->
    NonEmpty FishStatement ->
    [FishStatement] ->
    [ExprOrRedirect] ->
    FishCommand TStatus
  Switch ::
    FishExpr TStr ->
    NonEmpty CaseItem ->
    [ExprOrRedirect] ->
    FishCommand TStatus
  Break :: FishCommand TUnit
  Continue :: FishCommand TUnit
  Return :: Maybe (FishExpr TInt) -> FishCommand TStatus
  -- | Control flow and environment.
  Exit :: Maybe (FishExpr TInt) -> FishCommand TStatus
  Source :: FishExpr TStr -> FishCommand TStatus
  Eval :: FishExpr TStr -> FishCommand TStatus
  -- | IO commands.
  Read :: [ReadFlag] -> [Text] -> FishCommand TStatus
  Echo :: NonEmpty (FishExpr TStr) -> FishCommand TUnit
  Printf :: FishExpr TStr -> [FishExpr TStr] -> FishCommand TUnit
  -- | Combining commands and job control.
  Pipeline :: FishJobPipeline -> FishCommand TStatus
  JobConj :: FishJobConjunction -> FishCommand TStatus
  Semicolon ::
    (Typeable a, Typeable b) =>
    FishCommand a ->
    FishCommand b ->
    FishCommand b
  Not :: FishCommand TStatus -> FishCommand TStatus
  Background :: (Typeable a) => FishCommand a -> FishCommand TStatus
  Wait :: Maybe (FishExpr TInt) -> FishCommand TStatus
  Exec :: FishExpr TStr -> [ExprOrRedirect] -> FishCommand TStatus
  -- | Command decoration (`builtin`, `command`, `exec`).
  Decorated ::
    (Typeable a) =>
    Decoration ->
    FishCommand a ->
    FishCommand a

deriving stock instance Show (FishCommand t)

instance (Typeable t) => Eq (FishCommand t) where
  (==) = eqGADT eqFishCommandSameType

eqFishCommandSameType :: FishCommand a -> FishCommand a -> Bool
eqFishCommandSameType (Command txt1 args1) (Command txt2 args2) = txt1 == txt2 && args1 == args2
eqFishCommandSameType (Set f1 v1 e1) (Set f2 v2 e2) = f1 == f2 && v1 == v2 && eqFishExpr e1 e2
eqFishCommandSameType (Function f1) (Function f2) = f1 == f2
eqFishCommandSameType (For v1 l1 b1 r1) (For v2 l2 b2 r2) = v1 == v2 && eqFishExpr l1 l2 && b1 == b2 && r1 == r2
eqFishCommandSameType (While c1 b1 r1) (While c2 b2 r2) = c1 == c2 && b1 == b2 && r1 == r2
eqFishCommandSameType (Begin s1 r1) (Begin s2 r2) = s1 == s2 && r1 == r2
eqFishCommandSameType (If c1 t1 e1 r1) (If c2 t2 e2 r2) = c1 == c2 && t1 == t2 && e1 == e2 && r1 == r2
eqFishCommandSameType (Switch e1 cs1 r1) (Switch e2 cs2 r2) = eqFishExpr e1 e2 && cs1 == cs2 && r1 == r2
eqFishCommandSameType Break Break = True
eqFishCommandSameType Continue Continue = True
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
eqFishCommandSameType (Semicolon c1a c1b) (Semicolon c2a c2b) = eqGADT eqFishCommandSameType c1a c2a && eqFishCommandSameType c1b c2b
eqFishCommandSameType (Not c1) (Not c2) = c1 == c2
eqFishCommandSameType (Background (c1 :: FishCommand a)) (Background (c2 :: FishCommand b)) =
  eqGADT eqFishCommandSameType c1 c2
eqFishCommandSameType (Wait a1) (Wait a2) = case (a1, a2) of
  (Nothing, Nothing) -> True
  (Just x, Just y) -> eqFishExpr x y
  _ -> False
eqFishCommandSameType (Exec c1 a1) (Exec c2 a2) = eqFishExpr c1 c2 && a1 == a2
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
data FishExpr (t :: FishType) where
  -- | Literals.
  ExprLiteral :: Text -> FishExpr TStr
  ExprNumLiteral :: Int -> FishExpr TInt
  -- | Variables.
  ExprVariable :: FishVarRef t -> FishExpr t
  -- | Special variables.
  ExprSpecialVar :: SpecialVarRef t -> FishExpr t
  -- | String operations.
  ExprStringConcat :: FishExpr TStr -> FishExpr TStr -> FishExpr TStr
  ExprStringOp :: StringOp -> FishExpr TStr -> FishExpr TStr
  ExprJoinList :: FishExpr (TList TStr) -> FishExpr TStr
  -- | Arithmetic operations.
  ExprMath :: NonEmpty (FishExpr TStr) -> FishExpr TInt
  -- | Command substitution (fish produces a list of strings).
  ExprCommandSubst :: NonEmpty FishStatement -> FishExpr (TList TStr)
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
eqFishExprSameType (ExprNumLiteral n1) (ExprNumLiteral n2) = n1 == n2
eqFishExprSameType (ExprVariable v1) (ExprVariable v2) = v1 == v2
eqFishExprSameType (ExprStringConcat x1 y1) (ExprStringConcat x2 y2) = x1 == x2 && y1 == y2
eqFishExprSameType (ExprStringOp o1 a1) (ExprStringOp o2 a2) = o1 == o2 && a1 == a2
eqFishExprSameType (ExprJoinList a1) (ExprJoinList a2) = eqFishExpr a1 a2
eqFishExprSameType (ExprMath xs1) (ExprMath xs2) = xs1 == xs2
eqFishExprSameType (ExprCommandSubst s1) (ExprCommandSubst s2) = s1 == s2
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
  VarAll :: Text -> FishVarRef (TList TStr)
  VarScalar :: Text -> FishVarRef TStr
  VarIndex :: Text -> FishIndex TStr t -> FishVarRef t

deriving stock instance Show (FishVarRef t)

instance Eq (FishVarRef t) where
  VarAll a == VarAll b = a == b
  VarScalar a == VarScalar b = a == b
  VarIndex a i == VarIndex b j = a == b && i == j
  _ == _ = False

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

--------------------------------------------------------------------------------

-- | Function definitions and flags.

--------------------------------------------------------------------------------

-- | Function flags accepted by `function`.
data FunctionFlag
  = FuncDescription Text
  | FuncOnEvent Text
  | FuncOnVariable Text
  | FuncOnJobExit Text
  | FuncOnProcessExit Text
  | FuncWraps Text
  | FuncHelp
  | FuncInheritVariable
  | FuncUnknownFlag Text
  deriving stock (Show, Eq)

-- | Function definition.
data FishFunction = FishFunction
  { funcName :: Text,
    funcFlags :: [FunctionFlag],
    funcParams :: [Text],
    funcBody :: NonEmpty FishStatement
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------

-- | Flags accepted by `set`.

--------------------------------------------------------------------------------

data SetFlag
  = SetLocal
  | SetGlobal
  | SetUniversal
  | SetExport
  | SetUnexport
  | SetAppend
  | SetPrepend
  | SetErase
  | SetPath
  | SetQuery
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------

-- | Job model (pipelines and conjunctions).

--------------------------------------------------------------------------------

-- | Variable assignment attached to a job or continuation.
data VariableAssignment = VariableAssignment
  { vaName :: Text,
    vaValue :: Maybe (FishExpr TStr)
  }
  deriving stock (Show, Eq)

-- | Pipeline continuation: `|` followed by variables and a statement.
data JobPipeCont = PipeTo
  { jpcVariables :: [VariableAssignment],
    jpcStatement :: FishStatement
  }
  deriving stock (Show, Eq)

-- | Pipeline with optional `time`, leading variables, and backgrounding.
data FishJobPipeline = FishJobPipeline
  { jpTime :: Bool,
    jpVariables :: [VariableAssignment],
    jpStatement :: FishStatement,
    jpCont :: [JobPipeCont],
    jpBackgrounded :: Bool
  }
  deriving stock (Show, Eq)

-- | Conjunction keywords used by fish: `and` / `or`.
data Conjunction
  = ConjAnd
  | ConjOr
  deriving stock (Show, Eq)

-- | Conjunction continuation: `and job` or `or job`.
data FishJobConjCont
  = JCAnd FishJobPipeline
  | JCOr FishJobPipeline
  deriving stock (Show, Eq)

-- | A job conjunction consisting of an optional leading decorator and continuations.
data FishJobConjunction = FishJobConjunction
  { jcDecorator :: Maybe Conjunction,
    jcJob :: FishJobPipeline,
    jcContinuations :: [FishJobConjCont]
  }
  deriving stock (Show, Eq)

-- | A list of job conjunctions (used for if/while conditions).
newtype FishJobList = FishJobList (NonEmpty FishJobConjunction)
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------

-- | Case items and redirections.

--------------------------------------------------------------------------------

-- | A switch case item with one or more patterns and a body.
data CaseItem = CaseItem
  { casePatterns :: NonEmpty (FishExpr TStr),
    caseBody :: NonEmpty FishStatement
  }
  deriving stock (Show, Eq)

-- | Detailed redirection model.
data Redirect = Redirect
  { redirSource :: RedirectSource,
    redirOp :: RedirectOp,
    redirTarget :: RedirectTarget
  }
  deriving stock (Show, Eq)

-- | Redirection source.
data RedirectSource
  = RedirectStdout
  | RedirectStderr
  | RedirectStdin
  | RedirectBoth
  | RedirectFD Int
  deriving stock (Show, Eq)

-- | Redirection operator.
data RedirectOp
  = RedirectOut
  | RedirectOutAppend
  | RedirectIn
  | RedirectClobber
  | RedirectReadWrite
  deriving stock (Show, Eq)

-- | Redirection target.
data RedirectTarget
  = RedirectFile (FishExpr TStr)
  | RedirectTargetFD Int
  | RedirectClose
  deriving stock (Show, Eq)

-- | Command decoration (`builtin`, `command`, `exec`).
data Decoration
  = DecBuiltin
  | DecCommand
  | DecExec
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------

-- | Source tracking.

--------------------------------------------------------------------------------

-- | A source position in an input file (1-based line/column).
data SourcePos = SourcePos
  { srcFile :: Text,
    srcLine :: Int,
    srcColumn :: Int
  }
  deriving stock (Show, Eq, Ord)

-- | A source range with start and end positions.
data SourceRange = SourceRange
  { rangeStart :: SourcePos,
    rangeEnd :: SourcePos
  }
  deriving stock (Show, Eq, Ord)

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

type CmdStr = FishCommand TStr

type CmdInt = FishCommand TInt

type CmdBool = FishCommand TBool

type CmdList a = FishCommand (TList a)

type CmdStatus = FishCommand TStatus

type CmdUnit = FishCommand TUnit

type ExprStr = FishExpr TStr

type ExprInt = FishExpr TInt

type ExprBool = FishExpr TBool

type ExprList a = FishExpr (TList a)

type ExprStatus = FishExpr TStatus

type ExprUnit = FishExpr TUnit

--------------------------------------------------------------------------------

-- | Special variables, glob patterns, string ops, and read flags.

--------------------------------------------------------------------------------

-- | Typed special variables available in fish.
data SpecialVarRef (t :: FishType) where
  SVStatus :: SpecialVarRef TInt
  SVPipestatus :: SpecialVarRef (TList TInt)
  SVArgv :: SpecialVarRef (TList TStr)
  SVPID :: SpecialVarRef TInt
  SVLastPID :: SpecialVarRef TInt
  SVHostname :: SpecialVarRef TStr
  SVUser :: SpecialVarRef TStr
  SVHome :: SpecialVarRef TStr
  SVPWD :: SpecialVarRef TStr

deriving stock instance Show (SpecialVarRef t)

instance Eq (SpecialVarRef t) where
  SVStatus == SVStatus = True
  SVPipestatus == SVPipestatus = True
  SVArgv == SVArgv = True
  SVPID == SVPID = True
  SVLastPID == SVLastPID = True
  SVHostname == SVHostname = True
  SVUser == SVUser = True
  SVHome == SVHome = True
  SVPWD == SVPWD = True
  _ == _ = False

-- | A glob pattern composed of parts.
newtype GlobPattern = GlobPattern [GlobPart]
  deriving stock (Show, Eq)

-- | A component of a glob pattern.
data GlobPart
  = GlobLiteral Text
  | GlobStar
  | GlobStarStar
  | GlobQuestion
  | GlobCharClass Text
  | GlobBraces (NonEmpty Text)
  deriving stock (Show, Eq)

-- | String operations used by `string`.
data StringOp
  = StrLength
  | StrLower
  | StrUpper
  | StrEscape
  | StrUnescape
  | StrSplit Text
  | StrJoin Text
  | StrReplace Text Text
  | StrMatch Text
  deriving stock (Show, Eq)

-- | Flags for the `read` builtin.
data ReadFlag
  = ReadPrompt Text
  | ReadLocal
  | ReadGlobal
  | ReadUniversal
  | ReadExport
  | ReadArray
  | ReadNChars Text
  | ReadTimeout Text
  | ReadFD Text
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------

-- | Example AST.

--------------------------------------------------------------------------------

exampleAST :: [FishStatement]
exampleAST =
  [ Comment "Example test script",
    Stmt (Command "echo" [ExprVal (ExprLiteral "Hello Fish!")]),
    Stmt
      ( If
          (FishJobList (FishJobConjunction Nothing (FishJobPipeline False [] (Stmt (Command "true" [])) [] False) [] :| []))
          (Stmt (Command "echo" [ExprVal (ExprLiteral "In the if!")]) :| [])
          [Stmt (Command "echo" [ExprVal (ExprLiteral "In the else!")])]
          []
      ),
    Stmt
      ( Function
          FishFunction
            { funcName = "greet",
              funcFlags = [],
              funcParams = ["name"],
              funcBody =
                Stmt
                  ( Command
                      "echo"
                      [ ExprVal (ExprLiteral "Hello,"),
                        ExprVal (ExprVariable (VarIndex "name" (IndexSingle (ExprNumLiteral 1))))
                      ]
                  )
                  :| []
            }
      ),
    Stmt
      ( For
          "x"
          (ExprListLiteral [ExprLiteral "1", ExprLiteral "2", ExprLiteral "3"])
          ( Stmt
              ( Command
                  "echo"
                  [ ExprVal (ExprLiteral "Number:"),
                    ExprVal (ExprVariable (VarAll "x"))
                  ]
              )
              :| []
          )
          []
      ),
    Stmt
      ( For
          "x"
          ( ExprListConcat
              (ExprVariable (VarAll "var1"))
              (ExprListConcat (ExprVariable (VarAll "var2")) (ExprVariable (VarAll "var3")))
          )
          ( Stmt
              ( Command
                  "echo"
                  [ ExprVal (ExprLiteral "Variable:"),
                    ExprVal (ExprVariable (VarAll "x"))
                  ]
              )
              :| []
          )
          []
      ),
    Stmt
      ( Pipeline
          FishJobPipeline
            { jpTime = False,
              jpVariables = [],
              jpStatement = Stmt (Command "grep" [ExprVal (ExprLiteral "something")]),
              jpCont = [PipeTo {jpcVariables = [], jpcStatement = Stmt (Command "wc" [ExprVal (ExprLiteral "-l")])}],
              jpBackgrounded = False
            }
      ),
    Stmt
      ( Switch
          (ExprJoinList (ExprVariable (VarAll "myvar")))
          ( CaseItem
              { casePatterns = ExprLiteral "foo" :| [],
                caseBody = Stmt (Command "echo" [ExprVal (ExprLiteral "It was foo")]) :| []
              }
              :| [ CaseItem
                     { casePatterns = ExprLiteral "bar" :| [ExprLiteral "baz"],
                       caseBody = Stmt (Command "echo" [ExprVal (ExprLiteral "It was bar or baz")]) :| []
                     }
                 ]
          )
          []
      ),
    Stmt
      ( Begin
          (Stmt (Command "echo" [ExprVal (ExprLiteral "brace body")]) :| [])
          [RedirectVal (Redirect RedirectStdout RedirectOut (RedirectFile (ExprLiteral "/dev/null")))]
      )
  ]

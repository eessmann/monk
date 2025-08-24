{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Language.Fish.AST
  ( -- * Types
    FishType (..),
    FishStatement (..),
    FishCommand (..),
    ExprOrRedirect (..),
    FishExpr (..),
    FishIndex (..),
    -- Job pipeline & conjunction (Fish semantics)
    VariableAssignment (..),
    FishJobPipeline (..),
    JobPipeCont (..),
    Conjunction (..),
    FishJobConjCont (..),
    FishJobConjunction (..),

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

    -- * Arithmetic & Boolean
    ArithOp (..),
    NumExpr (..), -- Kept for structure, but might be simplified further
    BoolExpr (..),

    -- * Operators, Flags, etc.
    TestOperator (..),
    RedirectOp (..),
    Decoration (..),
    CaseItem (..),
    FreestandingArgumentList (..),

    -- * Function & Variable Details
    FishFunction (..),
    FunctionFlag (..),
    VariableScope (..),

    -- * Source
    SourceRange (..),

    -- * Helper for Eq instances
    eqGADT,
    eqFishExprSameType,

    -- * Example
    exampleAST
  )
where

import Relude
import Data.Coerce (coerce)
import Data.Type.Equality (testEquality, (:~:) (Refl))
import Type.Reflection (typeRep)
import Data.Typeable (Typeable)
import Data.List.NonEmpty (NonEmpty((:|)))

--------------------------------------------------------------------------------
---- 1. FishType
--------------------------------------------------------------------------------

data FishType
  = TStr -- String/Text
  | TInt -- Integer
  | TBool -- Boolean
  | TList FishType
  | TStatus -- Status of a command - 0 for success, non-zero for failure
  | TUnit -- No meaningful return value (void-like)
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
---- 2. Statements
--------------------------------------------------------------------------------
data FishStatement where
  Stmt ::
    (Typeable a) =>
    FishCommand a ->
    FishStatement
  StmtList :: [FishStatement] -> FishStatement -- Keep as list for empty blocks/scripts
  Comment :: Text -> FishStatement
  Freestanding :: FreestandingArgumentList -> FishStatement
  SemiNl :: FishStatement -- Represents a semicolon followed by newline potentially

deriving stock instance Show FishStatement

instance Eq FishStatement where
  (Stmt (c1 :: FishCommand a)) == (Stmt (c2 :: FishCommand b)) =
    case testEquality (typeRep @a) (typeRep @b) of
      Just Refl -> c1 == c2
      Nothing -> False
  (StmtList xs1) == (StmtList xs2) = xs1 == xs2
  (Comment t1) == (Comment t2) = t1 == t2
  (Freestanding f1) == (Freestanding f2) = f1 == f2
  SemiNl == SemiNl = True
  _ == _ = False

--------------------------------------------------------------------------------
---- 3. Commands
--------------------------------------------------------------------------------

data FishCommand (t :: FishType) where
  ---------------------------------------------------
  -- Basic Commands
  ---------------------------------------------------
  Command :: Text -> [ExprOrRedirect] -> FishCommand TStatus
  Set ::
    (Typeable a) =>
    VariableScope ->
    Text ->
    FishExpr a -> -- Changed from FishArg
    FishCommand TUnit
  Function :: FishFunction -> FishCommand TUnit
  For ::
    Text ->
    NonEmpty (FishExpr TStr) ->
    NonEmpty FishStatement -> -- Changed to NonEmpty
    FishCommand TUnit
  While ::
    FishExpr TBool ->
    NonEmpty FishStatement -> -- Changed to NonEmpty
    FishCommand TUnit
  Begin :: NonEmpty FishStatement -> FishCommand TUnit -- Changed to NonEmpty
  If ::
    FishExpr TBool ->
    NonEmpty FishStatement -> -- then branch - Changed to NonEmpty
    [FishStatement] -> -- else branch (can be empty)
    FishCommand TUnit
  Switch ::
    FishExpr TStr -> -- Changed from FishArg
    NonEmpty CaseItem -> -- Changed to NonEmpty
    FishCommand TUnit
  Break :: FishCommand TUnit
  Continue :: FishCommand TUnit
  Return ::
    Maybe (FishExpr TInt) ->
    FishCommand TStatus
  Source :: FishExpr TStr -> FishCommand TUnit -- Changed from FishArg
  Brace :: NonEmpty FishStatement -> FishCommand TUnit -- Changed to NonEmpty

  ---------------------------------------------------
  -- IO / Redirection
  ---------------------------------------------------
  Read :: Text -> FishCommand TStatus -- Reads a line into variable(s), returns status
  Echo :: NonEmpty (FishExpr TStr) -> FishCommand TUnit -- Changed from FishArg, made NonEmpty
  Printf :: FishExpr TStr -> [FishExpr TStr] -> FishCommand TUnit -- Changed from FishArg

  ---------------------------------------------------
  -- Combining Commands / Job Control
  ---------------------------------------------------
  Pipeline :: FishJobPipeline -> FishCommand TStatus
  JobConj :: FishJobConjunction -> FishCommand TStatus
  Semicolon ::
    (Typeable a, Typeable b) =>
    FishCommand a ->
    FishCommand b ->
    FishCommand b
  Not ::
    FishCommand TStatus -> -- 'not' primarily operates on status
    FishCommand TStatus
  Background :: FishCommand TStatus -> FishCommand TStatus -- Backgrounding returns status

  ---------------------------------------------------
  -- Decorated
  ---------------------------------------------------
  Decorated ::
    (Typeable a) =>
    Decoration ->
    FishCommand a ->
    FishCommand a
  ---------------------------------------------------
  -- Try/Catch
  ---------------------------------------------------
  TryCatch ::
    NonEmpty FishStatement -> -- try block
    NonEmpty FishStatement -> -- catch block
    FishCommand TUnit

deriving stock instance Show (FishCommand t)

instance (Typeable t) => Eq (FishCommand t) where
  (==) = eqGADT eqFishCommandSameType

eqFishCommandSameType :: FishCommand a -> FishCommand a -> Bool
eqFishCommandSameType (Command txt1 args1) (Command txt2 args2) = txt1 == txt2 && args1 == args2
eqFishCommandSameType (Set s1 v1 e1) (Set s2 v2 e2) = s1 == s2 && v1 == v2 && eqFishExpr e1 e2
eqFishCommandSameType (Function f1) (Function f2) = f1 == f2
eqFishCommandSameType (For v1 l1 b1) (For v2 l2 b2) = v1 == v2 && l1 == l2 && b1 == b2
eqFishCommandSameType (While c1 b1) (While c2 b2) = c1 == c2 && b1 == b2
eqFishCommandSameType (Begin s1) (Begin s2) = s1 == s2
eqFishCommandSameType (If c1 t1 e1) (If c2 t2 e2) = c1 == c2 && t1 == t2 && e1 == e2
eqFishCommandSameType (Switch e1 cs1) (Switch e2 cs2) = eqFishExpr e1 e2 && cs1 == cs2
eqFishCommandSameType Break Break = True
eqFishCommandSameType Continue Continue = True
eqFishCommandSameType (Return e1) (Return e2) = case (e1, e2) of
  (Nothing, Nothing) -> True
  (Just x, Just y) -> eqFishExpr x y
  _ -> False
eqFishCommandSameType (Source e1) (Source e2) = eqFishExpr e1 e2
eqFishCommandSameType (Brace s1) (Brace s2) = s1 == s2
eqFishCommandSameType (Read v1) (Read v2) = v1 == v2
eqFishCommandSameType (Echo es1) (Echo es2) = es1 == es2
eqFishCommandSameType (Printf f1 a1) (Printf f2 a2) = eqFishExpr f1 f2 && a1 == a2
eqFishCommandSameType (Pipeline p1) (Pipeline p2) = p1 == p2
eqFishCommandSameType (JobConj j1) (JobConj j2) = j1 == j2
eqFishCommandSameType (Semicolon c1a c1b) (Semicolon c2a c2b) = eqGADT eqFishCommandSameType c1a c2a && eqFishCommandSameType c1b c2b
eqFishCommandSameType (Not c1) (Not c2) = c1 == c2
eqFishCommandSameType (Background c1) (Background c2) = c1 == c2
eqFishCommandSameType (Decorated d1 c1) (Decorated d2 c2) = d1 == d2 && eqFishCommandSameType c1 c2
eqFishCommandSameType (TryCatch t1 c1) (TryCatch t2 c2) = t1 == t2 && c1 == c2
-- Fallback for constructors not matching or different types
eqFishCommandSameType _ _ = False


--------------------------------------------------------------------------------
---- 4. Expressions & Redirects (Unified Arg/Expr)
--------------------------------------------------------------------------------

-- | Represents either an expression (value) or a redirection.
-- Used in command argument lists.
data ExprOrRedirect where
  ExprVal :: (Typeable a) => FishExpr a -> ExprOrRedirect
  RedirectVal :: RedirectOp -> FishExpr TStr -> ExprOrRedirect

deriving stock instance Show ExprOrRedirect

instance Eq ExprOrRedirect where
  (ExprVal e1) == (ExprVal e2) = eqFishExpr e1 e2
  (RedirectVal op1 e1) == (RedirectVal op2 e2) = op1 == op2 && eqFishExpr e1 e2
  _ == _ = False

-- | GADT for Fish expressions (replaces FishArg).
-- Typeable t constraint needed for Eq instance via eqGADT.
data FishExpr (t :: FishType) where
  -- Literals
  ExprLiteral :: Text -> FishExpr TStr
  ExprNumLiteral :: Int -> FishExpr TInt
  ExprBoolLiteral :: Bool -> FishExpr TBool

  -- Variables (generalized)
  ExprVariable :: (Typeable a) => Text -> Maybe FishIndex -> FishExpr a

  -- String operations
  ExprStringConcat :: FishExpr TStr -> FishExpr TStr -> FishExpr TStr

  -- Arithmetic operations
  ExprNumArith :: ArithOp -> FishExpr TInt -> FishExpr TInt -> FishExpr TInt

  -- Boolean operations
  ExprBoolExpr :: BoolExpr -> FishExpr TBool

  -- Command Substitution
  -- In fish, command substitutions yield strings (often multiple; split on newlines)
  ExprCommandSubstStr :: NonEmpty FishStatement -> FishExpr TStr
  ExprCommandSubst :: NonEmpty FishStatement -> FishExpr (TList TStr)

deriving stock instance Show (FishExpr t)

-- | Eq instance using eqGADT helper
instance (Typeable t) => Eq (FishExpr t) where
  (==) = eqGADT eqFishExprSameType

-- | Helper to compare FishExpr when types are known to be equal (a ~ b)
eqFishExprSameType :: FishExpr a -> FishExpr a -> Bool
eqFishExprSameType (ExprLiteral t1) (ExprLiteral t2) = t1 == t2
eqFishExprSameType (ExprNumLiteral n1) (ExprNumLiteral n2) = n1 == n2
eqFishExprSameType (ExprBoolLiteral b1) (ExprBoolLiteral b2) = b1 == b2
eqFishExprSameType (ExprVariable v1 i1) (ExprVariable v2 i2) = v1 == v2 && i1 == i2
eqFishExprSameType (ExprStringConcat x1 y1) (ExprStringConcat x2 y2) = x1 == x2 && y1 == y2
eqFishExprSameType (ExprNumArith op1 l1 r1) (ExprNumArith op2 l2 r2) = op1 == op2 && l1 == l2 && r1 == r2
eqFishExprSameType (ExprBoolExpr b1) (ExprBoolExpr b2) = b1 == b2
eqFishExprSameType (ExprCommandSubstStr s1) (ExprCommandSubstStr s2) = s1 == s2
eqFishExprSameType (ExprCommandSubst s1) (ExprCommandSubst s2) = s1 == s2
eqFishExprSameType _ _ = False

-- | Heterogeneous equality check for FishExpr using Typeable
eqFishExpr :: forall a b. (Typeable a, Typeable b) => FishExpr a -> FishExpr b -> Bool
eqFishExpr = eqGADT eqFishExprSameType

--------------------------------------------------------------------------------
---- 5. Indexing
--------------------------------------------------------------------------------

data FishIndex
  = SingleIndex (FishExpr TInt)
  | RangeIndex (FishExpr TInt) (FishExpr TInt)
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
---- 6. Numeric & Boolean Sub-AST (Simplified)
--------------------------------------------------------------------------------

data ArithOp
  = OpPlus
  | OpMinus
  | OpMultiply
  | OpDivide
  | OpModulo
  deriving stock (Show, Eq)

-- Potentially NumExpr could be removed if ExprNumArith handles all cases,
-- but kept for now if direct representation of e.g. `math` expressions is needed.
data NumExpr
  = NumLit Int
  | NumVar Text -- Now redundant with ExprVariable?
  | NumArth ArithOp NumExpr NumExpr
  deriving stock (Show, Eq)

data BoolExpr where
  -- Note: BoolLiteral is now handled by ExprBoolLiteral
  BoolVar :: Text -> BoolExpr -- Redundant with ExprVariable?
  BoolAnd :: FishExpr TBool -> FishExpr TBool -> BoolExpr
  BoolOr :: FishExpr TBool -> FishExpr TBool -> BoolExpr
  BoolNot :: FishExpr TBool -> BoolExpr
  BoolTestOp ::
    (Typeable a, Typeable b) =>
    TestOperator ->
    FishExpr a -> -- Changed from FishArg
    FishExpr b -> -- Changed from FishArg
    BoolExpr
  BoolCommand :: FishCommand TStatus -> BoolExpr

deriving stock instance Show BoolExpr

instance Eq BoolExpr where
  BoolVar v1 == BoolVar v2 = v1 == v2
  BoolAnd a1 b1 == BoolAnd a2 b2 = eqFishExpr a1 a2 && eqFishExpr b1 b2
  BoolOr a1 b1 == BoolOr a2 b2 = eqFishExpr a1 a2 && eqFishExpr b1 b2
  BoolNot x1 == BoolNot x2 = eqFishExpr x1 x2
  BoolTestOp op1 x1 y1 == BoolTestOp op2 x2 y2 =
      op1 == op2 && eqFishExpr x1 x2 && eqFishExpr y1 y2
  BoolCommand c1 == BoolCommand c2 = c1 == c2
  _ == _ = False


--------------------------------------------------------------------------------
---- 7. Function-Related Types
--------------------------------------------------------------------------------

data FunctionFlag
  = FuncDescription Text
  | FuncOnEvent Text
  | FuncHelp
  | FuncInheritVariable
  | FuncUnknownFlag Text
  deriving stock (Show, Eq)

data FishFunction = FishFunction
  { funcName :: Text,
    funcFlags :: [FunctionFlag],
    funcParams :: [FishExpr TStr], -- Changed from FishArg
    funcBody :: NonEmpty FishStatement -- Changed to NonEmpty
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
---- 8. Variable Scopes for 'set'
--------------------------------------------------------------------------------

data VariableScope
  = ScopeLocal
  | ScopeGlobal
  | ScopeExported
  | ScopeUniversal
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
---- 9. Job Control
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
---- 9b. Job Model (Fish semantics)
--------------------------------------------------------------------------------

-- Variable assignment like FOO=bar, attached to a job or continuation
data VariableAssignment = VariableAssignment
  { vaName :: Text,
    vaValue :: Maybe (FishExpr TStr)
  }
  deriving stock (Show, Eq)

-- Pipeline continuation: `|` followed by variables and a statement
data JobPipeCont = PipeTo
  { jpcVariables :: [VariableAssignment]
  , jpcStatement :: FishStatement
  }
  deriving stock (Show, Eq)

-- Pipeline: optional `time`, variables, a statement, continuations, and optional background `&`
data FishJobPipeline = FishJobPipeline
  { jpTime :: Bool
  , jpVariables :: [VariableAssignment]
  , jpStatement :: FishStatement
  , jpCont :: [JobPipeCont]
  , jpBackgrounded :: Bool
  }
  deriving stock (Show, Eq)

-- Conjunction keywords used by fish: `and` / `or`
data Conjunction
  = ConjAnd
  | ConjOr
  deriving stock (Show, Eq)

-- Conjunction continuation: `and job` or `or job`
data FishJobConjCont
  = JCAnd FishJobPipeline
  | JCOr FishJobPipeline
  deriving stock (Show, Eq)

-- A job conjunction consisting of an optional leading decorator, a job, and continuations
data FishJobConjunction = FishJobConjunction
  { jcDecorator :: Maybe Conjunction,
    jcJob :: FishJobPipeline,
    jcContinuations :: [FishJobConjCont],
    jcSemiNl :: Bool
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
---- 10. Case Items, Redirection, Etc.
--------------------------------------------------------------------------------

data CaseItem = CaseItem
  { casePatterns :: NonEmpty (FishExpr TStr), -- Changed from FishArg, NonEmpty
    caseBody :: NonEmpty FishStatement -- Changed to NonEmpty
  }
  deriving stock (Show, Eq)

-- Represents arguments passed directly on the command line perhaps?
newtype FreestandingArgumentList = FreestandingArgumentList [FishExpr TStr] -- Changed from FishArg
  deriving stock (Show, Eq)

data RedirectOp
  = RedirectOut -- >
  | RedirectOutAppend -- >>
  | RedirectIn -- <
  | RedirectErr -- 2>
  | RedirectErrAppend -- 2>>
  | RedirectBoth -- &>
  | RedirectBothAppend -- &>>
  deriving stock (Show, Eq)

data Decoration
  = DecBuiltin
  | DecCommand
  | DecExec
  deriving stock (Show, Eq)

data TestOperator
  = Eq -- = (numeric/string)
  | Neq -- != (numeric/string)
  | Gt -- > (numeric)
  | Lt -- < (numeric)
  | Ge -- >= (numeric)
  | Le -- <= (numeric)
  | IsFile -- -f
  | IsDir -- -d
  | IsSymlink -- -h / -L
  | Exists -- -e
  | IsReadable -- -r
  | IsWritable -- -w
  | IsExecutable -- -x
  | IsEmpty -- -z (string)
  | NotEmpty -- -n (string)
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
---- 11. Source Tracking
--------------------------------------------------------------------------------

data SourceRange = SourceRange
  { startPos :: Int,
    endPos :: Int
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
---- Equality Helper for GADTs
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
---- Type synonyms (Updated)
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
type ExprStatus = FishExpr TStatus -- Usually via ExprSubstituted
type ExprUnit = FishExpr TUnit -- Less common, maybe via ExprSubstituted

--------------------------------------------------------------------------------
---- Example AST (Updated)
--------------------------------------------------------------------------------

exampleAST :: [FishStatement]
exampleAST =
  [ -- 1) A comment
    Comment "Example test script",

    -- 2) A simple command: echo "Hello Fish!"
    Stmt $
      Command "echo" [ExprVal (ExprLiteral "Hello Fish!")],

    -- 3) An if-statement
    Stmt $
      If
        (ExprBoolLiteral True)
        (Stmt (Command "echo" [ExprVal (ExprLiteral "In the if!")]) :| [])
        [Stmt (Command "echo" [ExprVal (ExprLiteral "In the else!")])], -- Else can be empty list

    -- 4) A function definition
    Stmt $
      Function $
        FishFunction
          { funcName = "greet",
            funcFlags = [],
            funcParams = [ExprLiteral "name"], -- function greet name
            funcBody =
              Stmt (
                Command "echo"
                  [ ExprVal (ExprLiteral "Hello,"),
                    ExprVal (ExprVariable @TStr "name" Nothing) -- Explicit type application
                  ]
              ) :| []
          },

    -- 5a) A for-loop over literals
    Stmt $
      For
        "x"
        (ExprLiteral "1" :| [ExprLiteral "2", ExprLiteral "3"])
        ( Stmt (
            Command "echo"
              [ ExprVal (ExprLiteral "Number:"),
                ExprVal (ExprVariable @TStr "x" Nothing)
              ]
          ) :| []
        ),

    SemiNl,

    -- 5b) A for-loop over variables
    Stmt $
      For
        "x"
        ( ExprVariable @TStr "var1" Nothing
          :| [ ExprVariable @TStr "var2" Nothing
             , ExprVariable @TStr "var3" Nothing
             ]
        )
        ( Stmt (
            Command "echo"
              [ ExprVal (ExprLiteral "Variable:"),
                ExprVal (ExprVariable @TStr "x" Nothing)
              ]
          ) :| []
        ),

    -- 6) A pipeline of commands
    Stmt $
      Pipeline $
        FishJobPipeline
          { jpTime = False
          , jpVariables = []
          , jpStatement = Stmt (Command "grep" [ExprVal (ExprLiteral "something")])
          , jpCont = [PipeTo { jpcVariables = [], jpcStatement = Stmt (Command "wc" [ExprVal (ExprLiteral "-l")]) }]
          , jpBackgrounded = False
          },

    -- 7) A switch statement
    Stmt $
      Switch
        (ExprVariable @TStr "myvar" Nothing)
        ( CaseItem
            { casePatterns = ExprLiteral "foo" :| [],
              caseBody = Stmt (Command "echo" [ExprVal (ExprLiteral "It was foo")]) :| []
            }
        :| [ CaseItem
            { casePatterns = ExprLiteral "bar" :| [ExprLiteral "baz"], -- Multiple patterns
              caseBody = Stmt (Command "echo" [ExprVal (ExprLiteral "It was bar or baz")]) :| []
            }
           ]
        )
  ]

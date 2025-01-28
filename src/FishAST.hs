module FishAST (
  -- * Types
  FishType(..),
  FishStatement(..),
  FishCommand(..),
  FishArgOrRedirect(..),
  FishArg(..),
  FishExpr(..),
  FishIndex(..),
  -- * Type synonyms
  CmdStr, CmdInt, CmdBool, CmdList, CmdStatus, CmdUnit,
  ArgStr, ArgInt, ArgBool, ArgList, ArgStatus, ArgUnit,
  ExprStr, ExprInt, ExprBool, ExprList, ExprStatus, ExprUnit,
  -- * Arithmetic & Boolean
  ArithOp(..),
  NumExpr(..),
  BoolExpr(..),
  -- * Operators, Flags, etc.
  TestOperator(..),
  RedirectOp(..),
  Decoration(..),
  CaseItem(..),
  FreestandingArgumentList(..),
  -- * Function & Variable Details
  FishFunction(..),
  FunctionFlag(..),
  VariableScope(..),
  -- * Job Control
  JobConjunction(..),
  -- * Source
  SourceRange(..)) where

import qualified ShellCheck.Interface as Bash (Position(..))
import Data.Type.Equality (testEquality, (:~:)(Refl))
import Type.Reflection (typeRep)

--------------------------------------------------------------------------------
---- 1. FishType
--------------------------------------------------------------------------------

data FishType
  = TStr       -- String
  | TInt       -- Integer
  | TBool      -- Boolean
  | TList FishType
  | TStatus    -- Status of a command - 0 for success, non-zero for failure
  | TUnit      -- No meaningful return value (void-like)
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
---- 2. Statements
--------------------------------------------------------------------------------

-- | We add a (Typeable a) constraint so we can unify the type index in 'Stmt'.
data FishStatement where
  Stmt         :: Typeable a
               => FishCommand a
               -> FishStatement
  StmtList     :: [FishStatement] -> FishStatement
  Comment      :: Text -> FishStatement
  Freestanding :: FreestandingArgumentList -> FishStatement
  SemiNl       :: FishStatement

deriving stock instance Show FishStatement

instance Eq FishStatement where
  (Stmt (c1 :: FishCommand a)) == (Stmt (c2 :: FishCommand b)) =
    -- We must unify the type variables for c1 :: FishCommand a
    -- and c2 :: FishCommand b.
    case testEquality (typeRep @a) (typeRep @b) of
      Just Refl -> c1 == c2  -- now we know a ~ b
      Nothing   -> False

  (StmtList xs1) == (StmtList xs2)          = xs1 == xs2
  (Comment t1)   == (Comment t2)            = t1 == t2
  (Freestanding f1) == (Freestanding f2)    = f1 == f2
  SemiNl         == SemiNl                  = True
  _              == _                       = False

--------------------------------------------------------------------------------
---- 3. Commands
--------------------------------------------------------------------------------

data FishCommand (t :: FishType) where
  ---------------------------------------------------
  -- Basic Commands, with Typeable constraints as needed
  ---------------------------------------------------
  Command   :: Text -> [FishArgOrRedirect] -> FishCommand TUnit

  Set       :: Typeable a
            => VariableScope
            -> Text
            -> FishArg a
            -> FishCommand TUnit

  Function  :: FishFunction -> FishCommand TUnit

  For       :: Typeable a
            => Text
            -> FishArg (TList a)
            -> [FishStatement]
            -> FishCommand TUnit

  While     :: FishExpr TBool
            -> [FishStatement]
            -> FishCommand TUnit

  Begin     :: [FishStatement] -> FishCommand TUnit

  If        :: FishExpr TBool
            -> [FishStatement]  -- then
            -> [FishStatement]  -- else
            -> FishCommand TUnit

  Switch    :: FishArg TStr
            -> [CaseItem]
            -> FishCommand TUnit

  Break     :: FishCommand TUnit
  Continue  :: FishCommand TUnit

  Return    :: Typeable a
            => FishArg a
            -> FishCommand a

  Source    :: FishArg TStr -> FishCommand TUnit
  Brace     :: [FishStatement] -> FishCommand TUnit
  HereDoc   :: FishArg TStr -> FishArg TStr -> FishCommand TUnit

  ---------------------------------------------------
  -- IO / Redirection
  ---------------------------------------------------
  Read      :: Text -> FishCommand TStr
  Echo      :: FishArg TStr -> FishCommand TUnit
  Printf    :: FishArg TStr -> [FishArg TStr] -> FishCommand TUnit

  Redirect  :: Typeable a
            => FishCommand a
            -> RedirectOp
            -> FishArg TStr
            -> FishCommand TUnit

  ---------------------------------------------------
  -- Combining Commands / Job Control
  ---------------------------------------------------
  Pipeline  :: NonEmpty (FishCommand TStatus) -> FishCommand TStatus

  JobControl:: JobConjunction
            -> FishCommand TStatus
            -> FishCommand TStatus
            -> FishCommand TStatus

  Semicolon :: (Typeable a, Typeable b)
            => FishCommand a
            -> FishCommand b
            -> FishCommand b

  Not       :: Typeable a
            => FishCommand a
            -> FishCommand TStatus

  Background:: FishCommand TStatus -> FishCommand TStatus
  Disown    :: FishCommand TStatus -> FishCommand TStatus

  ---------------------------------------------------
  -- Decorated
  ---------------------------------------------------
  Decorated :: Typeable a
            => Decoration
            -> FishCommand a
            -> FishCommand a

  ---------------------------------------------------
  -- Try/Catch
  ---------------------------------------------------
  TryCatch  :: [FishStatement]
            -> [FishStatement]
            -> FishCommand TUnit

deriving stock instance Show (FishCommand t)

instance Typeable t => Eq (FishCommand t) where
  (==) = eqGADT eqFishCommandSameType

eqFishCommandSameType :: FishCommand a -> FishCommand a -> Bool
  ----------------------------------------
  -- Command
  ----------------------------------------
eqFishCommandSameType (Command txt1 args1) (Command txt2 args2) =
    txt1 == txt2 && args1 == args2

  ----------------------------------------
  -- Set
  ----------------------------------------
eqFishCommandSameType (Set scope1 var1 arg1) (Set scope2 var2 arg2) =
    (scope1 == scope2) && (var1 == var2)
    && eqFishArg arg1 arg2
eqFishCommandSameType (Set _ _ _) _ = False

  ----------------------------------------
  -- Function
  ----------------------------------------
eqFishCommandSameType (Function f1) (Function f2) = f1 == f2
eqFishCommandSameType (Function _)  _            = False

  ----------------------------------------
  -- For
  ----------------------------------------
eqFishCommandSameType (For v1 list1 stmts1) (For v2 list2 stmts2) =
    (v1 == v2)
    && eqFishArg list1 list2
    && (stmts1 == stmts2)
eqFishCommandSameType (For _ _ _) _ = False

  ----------------------------------------
  -- While, Begin, If, Switch, Break, Continue
  ----------------------------------------
eqFishCommandSameType (While cond1 body1) (While cond2 body2) =
    cond1 == cond2 && body1 == body2
eqFishCommandSameType (While _ _) _ = False

eqFishCommandSameType (Begin s1) (Begin s2) = s1 == s2
eqFishCommandSameType (Begin _) _         = False

eqFishCommandSameType (If c1 th1 el1) (If c2 th2 el2) =
    (c1 == c2) && (th1 == th2) && (el1 == el2)
eqFishCommandSameType (If _ _ _) _ = False

eqFishCommandSameType (Switch a1 cs1) (Switch a2 cs2) =
    a1 == a2 && cs1 == cs2
eqFishCommandSameType (Switch _ _) _ = False

eqFishCommandSameType Break Break = True
eqFishCommandSameType Break _     = False

eqFishCommandSameType Continue Continue = True
eqFishCommandSameType Continue _        = False

  ----------------------------------------
  -- Return
  ----------------------------------------
eqFishCommandSameType (Return a1) (Return a2) = a1 == a2
eqFishCommandSameType (Return _) _            = False

  ----------------------------------------
  -- Source, Brace, HereDoc
  ----------------------------------------
eqFishCommandSameType (Source f1) (Source f2) = f1 == f2
eqFishCommandSameType (Source _) _           = False

eqFishCommandSameType (Brace b1) (Brace b2) = b1 == b2
eqFishCommandSameType (Brace _) _           = False

eqFishCommandSameType (HereDoc x1 y1) (HereDoc x2 y2) =
    (x1 == x2) && (y1 == y2)
eqFishCommandSameType (HereDoc _ _) _ = False

  ----------------------------------------
  -- IO and Redirection
  ----------------------------------------
eqFishCommandSameType (Read v1) (Read v2) = v1 == v2
eqFishCommandSameType (Read _) _      = False

eqFishCommandSameType (Echo x1) (Echo x2) = x1 == x2
eqFishCommandSameType (Echo _) _          = False

eqFishCommandSameType (Printf fmt1 xs1) (Printf fmt2 xs2) =
    fmt1 == fmt2 && xs1 == xs2
eqFishCommandSameType (Printf _ _) _ = False

eqFishCommandSameType (Redirect c1 op1 a1) (Redirect c2 op2 a2) =
    (eqGADT eqFishCommandSameType c1 c2) &&
    (op1 == op2) &&
    (a1 == a2)
eqFishCommandSameType (Redirect _ _ _) _ = False

  ----------------------------------------
  -- Pipeline, JobControl
  ----------------------------------------
eqFishCommandSameType (Pipeline xs1) (Pipeline xs2) = xs1 == xs2
eqFishCommandSameType (Pipeline _) _                = False

eqFishCommandSameType (JobControl conj1 l1 r1) (JobControl conj2 l2 r2) =
    conj1 == conj2 &&
    l1 == l2 &&
    r1 == r2
eqFishCommandSameType (JobControl _ _ _) _ = False

  ----------------------------------------
  -- Semicolon
  ----------------------------------------
eqFishCommandSameType (Semicolon a1 b1) (Semicolon a2 b2) =
    -- unify both a1 and a2, and b1 and b2
    (eqGADT eqFishCommandSameType a1 a2) &&
    (eqGADT eqFishCommandSameType b1 b2)
eqFishCommandSameType (Semicolon _ _) _ = False

  ----------------------------------------
  -- Not
  ----------------------------------------
eqFishCommandSameType (Not c1) (Not c2) = eqGADT eqFishCommandSameType c1 c2
eqFishCommandSameType (Not _) _       = False

  ----------------------------------------
  -- Background, Disown
  ----------------------------------------
eqFishCommandSameType (Background c1) (Background c2) = c1 == c2
eqFishCommandSameType (Background _) _             = False

eqFishCommandSameType (Disown c1) (Disown c2) = c1 == c2
eqFishCommandSameType (Disown _) _         = False

  ----------------------------------------
  -- Decorated
  ----------------------------------------
eqFishCommandSameType (Decorated d1 c1) (Decorated d2 c2) =
    d1 == d2 && c1 == c2
eqFishCommandSameType (Decorated _ _) _ = False

  ----------------------------------------
  -- TryCatch
  ----------------------------------------
eqFishCommandSameType (TryCatch t1 c1) (TryCatch t2 c2) =
    (t1 == t2) && (c1 == c2)
eqFishCommandSameType (TryCatch _ _) _ = False

  ----------------------------------------
  -- Default 
  ----------------------------------------
eqFishCommandSameType _ _ = False

--------------------------------------------------------------------------------
---- 4. Arguments & Redirects
--------------------------------------------------------------------------------

data FishArgOrRedirect where
  Arg   :: Typeable a
        => FishArg a
        -> FishArgOrRedirect
  Redir :: RedirectOp
        -> FishArg TStr
        -> FishArgOrRedirect
deriving stock instance Show FishArgOrRedirect

instance Eq FishArgOrRedirect where
  Arg a1 == Arg a2   = eqFishArg a1 a2
  Redir op1 s1 == Redir op2 s2 = (op1 == op2) && (s1 == s2)
  _ == _ = False

-- | GADT for arguments, requiring Typeable t so we can unify t in eqFishArg
data FishArg (t :: FishType) where
  ArgLiteral     :: Text -> FishArg TStr
  ArgNumber      :: Int  -> FishArg TInt

  -- We add Typeable a here for ArgVariable
  ArgVariable    :: Typeable a
                 => Text
                 -> FishArg a

  ArgConcat      :: FishArg TStr -> FishArg TStr -> FishArg TStr

  ArgList        :: Typeable a
                 => [FishArg a]
                 -> FishArg (TList a)

  ArgSubstituted :: Typeable a
                 => FishCommand a
                 -> FishArg a

deriving stock instance Show (FishArg t)

instance (Typeable t) => Eq (FishArg t) where
  -- Instead of pattern matching a1 with a2 directly, we delegate to eqFishArg
  (==) = eqGADT eqFishArgSameType

-- | eqFishArg compares two FishArg values of possibly different index types.
eqFishArg :: forall a b. (Typeable a, Typeable b) => FishArg a -> FishArg b -> Bool
eqFishArg = eqGADT eqFishArgSameType


-- We define eqFishArgSameType for the actual pattern matching where a ~ b
eqFishArgSameType
  :: FishArg a
  -> FishArg a
  -> Bool
eqFishArgSameType (ArgLiteral t1) (ArgLiteral t2) = t1 == t2
eqFishArgSameType (ArgNumber n1)  (ArgNumber n2)  = n1 == n2

eqFishArgSameType (ArgVariable v1) (ArgVariable v2) = v1 == v2
eqFishArgSameType (ArgConcat x1 y1) (ArgConcat x2 y2) =
  x1 == x2 && y1 == y2

eqFishArgSameType (ArgList xs1) (ArgList xs2) = xs1 == xs2

eqFishArgSameType (ArgSubstituted c1) (ArgSubstituted c2) =
  c1 == c2

-- If different constructors:
eqFishArgSameType _ _ = False

--------------------------------------------------------------------------------
---- 5. Expressions
--------------------------------------------------------------------------------

data FishExpr (t :: FishType) where
  ExprStringLit    :: String -> FishExpr TStr
  ExprStringVar    :: Text -> Maybe FishIndex -> FishExpr TStr
  ExprStringConcat :: FishExpr TStr -> FishExpr TStr -> FishExpr TStr

  ExprListLit      :: Typeable a
                   => [FishExpr a]
                   -> FishExpr (TList a)

  ExprNum          :: NumExpr -> FishExpr TInt
  ExprBool         :: BoolExpr -> FishExpr TBool

  ExprSubstituted  :: Typeable a
                   => FishCommand a
                   -> FishExpr a

deriving stock instance Show (FishExpr t)

instance (Typeable t) => Eq (FishExpr t) where
  (==) = eqGADT eqFishExprSameType

eqFishExprSameType :: FishExpr a -> FishExpr a -> Bool
eqFishExprSameType (ExprStringLit s1)  (ExprStringLit s2)  = s1 == s2
eqFishExprSameType (ExprStringVar v1 i1) (ExprStringVar v2 i2) =
  v1 == v2 && i1 == i2
eqFishExprSameType (ExprStringConcat x1 y1) (ExprStringConcat x2 y2) =
  x1 == x2 && y1 == y2
eqFishExprSameType (ExprListLit xs1) (ExprListLit xs2) =
  xs1 == xs2
eqFishExprSameType (ExprNum n1) (ExprNum n2) = n1 == n2
eqFishExprSameType (ExprBool b1) (ExprBool b2) = b1 == b2
eqFishExprSameType (ExprSubstituted c1) (ExprSubstituted c2) = c1 == c2

eqFishExprSameType _ _ = False

--------------------------------------------------------------------------------
---- 6. Numeric & Boolean Sub-AST
--------------------------------------------------------------------------------

data ArithOp
  = OpPlus
  | OpMinus
  | OpMultiply
  | OpDivide
  | OpModulo
  deriving stock (Show, Eq)

data NumExpr
  = NumLiteral  Int
  | NumVariable Text
  | NumArith    ArithOp NumExpr NumExpr
  deriving stock (Show, Eq)

data BoolExpr where
  BoolLiteral  :: Bool -> BoolExpr
  BoolVariable :: Text -> BoolExpr
  BoolAnd      :: BoolExpr -> BoolExpr -> BoolExpr
  BoolOr       :: BoolExpr -> BoolExpr -> BoolExpr
  BoolNot      :: BoolExpr -> BoolExpr
  BoolTestOp   :: forall a b. (Typeable a, Typeable b)
               => TestOperator
               -> FishArg a
               -> FishArg b
               -> BoolExpr
  BoolCommand  :: FishCommand TStatus -> BoolExpr

deriving stock instance Show BoolExpr

instance Eq BoolExpr where
  BoolLiteral b1 == BoolLiteral b2 = b1 == b2
  BoolVariable v1 == BoolVariable v2 = v1 == v2
  BoolAnd a1 b1 == BoolAnd a2 b2 = a1 == a2 && b1 == b2
  BoolOr a1 b1 == BoolOr a2 b2 = a1 == a2 && b1 == b2
  BoolNot x1 == BoolNot x2 = x1 == x2

  -- When we match two BoolTestOp, unify the type of x1 with x2, y1 with y2:
  BoolTestOp op1 x1 y1 == BoolTestOp op2 x2 y2 =
    op1 == op2
    && eqFishArg x1 x2
    && eqFishArg y1 y2

  BoolCommand c1 == BoolCommand c2 = c1 == c2
  _ == _ = False

--------------------------------------------------------------------------------
---- 7. Indexing
--------------------------------------------------------------------------------

data FishIndex
  = SingleIndex (FishExpr TInt)
  | RangeIndex  (FishExpr TInt) (FishExpr TInt)
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
---- 8. Function-Related Types
--------------------------------------------------------------------------------

data FunctionFlag
  = FuncDescription Text
  | FuncOnEvent Text
  | FuncHelp
  | FuncInheritVariable
  | FuncUnknownFlag Text
  deriving stock (Show, Eq)

data FishFunction = FishFunction
  { funcName   :: Text
  , funcFlags  :: [FunctionFlag]
  , funcParams :: [FishArg TStr]
  , funcBody   :: [FishStatement]
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
---- 9. Variable Scopes for 'set'
--------------------------------------------------------------------------------

data VariableScope
  = ScopeLocal
  | ScopeGlobal
  | ScopeExported
  | ScopeUniversal
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
---- 10. Job Control
--------------------------------------------------------------------------------

data JobConjunction
  = ConjAnd
  | ConjOr
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
---- 11. Case Items, Redirection, Etc.
--------------------------------------------------------------------------------

data CaseItem = CaseItem
  { casePatterns :: [FishArg TStr]
  , caseBody     :: [FishStatement]
  }
  deriving stock (Show, Eq)

newtype FreestandingArgumentList = FreestandingArgumentList [FishArg TStr]
  deriving stock (Show, Eq)

data RedirectOp
  = RedirectOut
  | RedirectOutAppend
  | RedirectIn
  | RedirectErr
  | RedirectErrAppend
  | RedirectBoth
  | RedirectBothAppend
  deriving stock (Show, Eq)

data Decoration
  = DecBuiltin
  | DecCommand
  | DecExec
  deriving stock (Show, Eq)

data TestOperator
  = Eq
  | Neq
  | Gt
  | Lt
  | Ge
  | Le
  | IsFile
  | IsDir
  | IsSymlink
  | StringContains
  | StringMatch
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
---- 12. Source Tracking
--------------------------------------------------------------------------------

data SourceRange = SourceRange
  { startPos :: Bash.Position
  , endPos   :: Bash.Position
  } deriving stock (Show, Eq)


--------------------------------------------------------------------------------
---- Equatuality Helpers
--------------------------------------------------------------------------------

eqGADT :: forall a b f. (Typeable a, Typeable b) 
  => (forall x. f x -> f x -> Bool) -> f a -> f b -> Bool
eqGADT eqSameType left right =
  case testEquality (typeRep @a) (typeRep @b) of
    Just Refl -> eqSameType left (coerce right)
    Nothing   -> False

-- | Type synonyms for convenience:
type CmdStr    = FishCommand 'TStr
type CmdInt    = FishCommand 'TInt
type CmdBool   = FishCommand 'TBool
type CmdList a = FishCommand ('TList a)
type CmdStatus = FishCommand 'TStatus
type CmdUnit   = FishCommand 'TUnit

type ArgStr    = FishArg 'TStr
type ArgInt    = FishArg 'TInt
type ArgBool   = FishArg 'TBool
type ArgList a = FishArg ('TList a)
type ArgStatus = FishArg 'TStatus
type ArgUnit   = FishArg 'TUnit

type ExprStr    = FishExpr 'TStr
type ExprInt    = FishExpr 'TInt
type ExprBool   = FishExpr 'TBool
type ExprList a = FishExpr ('TList a)
type ExprStatus = FishExpr 'TStatus
type ExprUnit   = FishExpr 'TUnit


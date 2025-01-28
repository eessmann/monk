module FishAST (
  -- * Types
  FishType(..),
  FishStatement(..),
  FishCommand(..),
  FishArgOrRedirect(..),
  FishArg(..),
  FishExpr(..),
  FishIndex(..),
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
  SourceRange(..)
) where

import qualified ShellCheck.Interface as Bash (Position(..))

--------------------------------------------------------------------------------
---- 1. FishType
--------------------------------------------------------------------------------

data FishType
  = TStr       -- String
  | TInt       -- Integer
  | TBool      -- Boolean
  | TList FishType
  | TStatus    -- Status of a command (0 or 1)
  | TUnit      -- No meaningful return value (void-like)
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
---- 2. Statements
--------------------------------------------------------------------------------

data FishStatement where
  Stmt         :: FishCommand a -> FishStatement
  StmtList     :: [FishStatement] -> FishStatement
  Comment      :: Text -> FishStatement
  Freestanding :: FreestandingArgumentList -> FishStatement
  SemiNl       :: FishStatement
  
deriving stock instance Show FishStatement

instance Eq FishStatement where
  (==) :: FishStatement -> FishStatement -> Bool
  Stmt a == Stmt b = a == b
  StmtList a == StmtList b = a == b
  Comment a == Comment b = a == b
  Freestanding a == Freestanding b = a == b
  SemiNl == SemiNl = True
  _ == _ = False

--------------------------------------------------------------------------------
---- 3. Commands
--------------------------------------------------------------------------------

data FishCommand (t :: FishType) where
  ----------------------------------------
  -- Fundamental Commands
  ----------------------------------------
  -- Generic command execution
  Command       :: Text -> [FishArgOrRedirect] -> FishCommand TUnit

  -- 'set' with variable scopes (local/global/exported/universal)
  Set           :: VariableScope
                -> Text        -- variable name
                -> FishArg a   -- value
                -> FishCommand TUnit

  -- Legacy: If you still want a separate SetEnv you can keep it,
  -- but in this example we've removed it in favor of using ScopeExported.

  -- A function definition referencing a separate FishFunction record
  Function      :: FishFunction -> FishCommand TUnit

  -- For loop: for var in (some list) ...
  For           :: Text
                -> FishArg (TList a)
                -> [FishStatement]
                -> FishCommand TUnit

  While         :: FishExpr TBool
                -> [FishStatement]
                -> FishCommand TUnit

  Begin         :: [FishStatement] -> FishCommand TUnit

  If            :: FishExpr TBool
                -> [FishStatement]    -- then block
                -> [FishStatement]    -- else block
                -> FishCommand TUnit

  Switch        :: FishArg TStr
                -> [CaseItem]         -- case items
                -> FishCommand TUnit

  Break         :: FishCommand TUnit
  Continue      :: FishCommand TUnit

  -- Return from function, carrying any typed value
  Return        :: FishArg a -> FishCommand a

  Source        :: FishArg TStr -> FishCommand TUnit
  Brace         :: [FishStatement] -> FishCommand TUnit
  HereDoc       :: FishArg TStr -> FishArg TStr -> FishCommand TUnit

  ----------------------------------------
  -- I/O and Redirection
  ----------------------------------------
  Read          :: Text -> FishCommand TStr   -- read var
  Echo          :: FishArg TStr -> FishCommand TUnit
  Printf        :: FishArg TStr
                -> [FishArg TStr]
                -> FishCommand TUnit

  Redirect      :: FishCommand a
                -> RedirectOp
                -> FishArg TStr
                -> FishCommand TUnit

  ----------------------------------------
  -- Combining Commands / Job Control
  ----------------------------------------
  -- Pipeline returns the exit status of the last command
  Pipeline      :: NonEmpty (FishCommand TStatus) -> FishCommand TStatus

  -- Unify "and"/"or" with job control into one constructor
  JobControl    :: JobConjunction
                -> FishCommand TStatus
                -> FishCommand TStatus
                -> FishCommand TStatus

  -- Execute commands sequentially (like semicolon)
  Semicolon     :: FishCommand a
                -> FishCommand b
                -> FishCommand b

  -- Not operator for commands
  Not           :: FishCommand a -> FishCommand TStatus

  -- Background a job/pipeline: cmd &
  Background    :: FishCommand TStatus -> FishCommand TStatus

  -- Disown a job (run without job control)
  Disown        :: FishCommand TStatus -> FishCommand TStatus

  ----------------------------------------
  -- Decorated Command
  ----------------------------------------
  Decorated     :: Decoration -> FishCommand a -> FishCommand a

  ----------------------------------------
  -- Try/Catch Block
  ----------------------------------------
  TryCatch      :: [FishStatement]  -- try block
                -> [FishStatement]  -- catch block
                -> FishCommand TUnit

deriving stock instance Show (FishCommand t)
instance Eq (FishCommand t) where
  -- Generic command execution
  (Command txt1 args1) == (Command txt2 args2) =
    (txt1 == txt2) && (args1 == args2)

  -- set
  (Set scope1 var1 arg1) == (Set scope2 var2 arg2) =
    (scope1 == scope2) && (var1 == var2) && (arg1 == arg2)

  -- function
  (Function f1) == (Function f2) = f1 == f2

  -- For
  (For v1 list1 stmts1) == (For v2 list2 stmts2) =
    (v1 == v2) && (list1 == list2) && (stmts1 == stmts2)

  -- While
  (While cond1 body1) == (While cond2 body2) =
    (cond1 == cond2) && (body1 == body2)

  -- Begin
  (Begin s1) == (Begin s2) = s1 == s2

  -- If
  (If c1 th1 el1) == (If c2 th2 el2) =
    (c1 == c2) && (th1 == th2) && (el1 == el2)

  (Switch a1 cases1) == (Switch a2 cases2) =
    (a1 == a2) && (cases1 == cases2)

  Break == Break = True
  Continue == Continue = True

  -- Return
  (Return a1) == (Return a2) = a1 == a2

  (Source f1) == (Source f2) = f1 == f2
  (Brace b1) == (Brace b2)   = b1 == b2
  (HereDoc x1 y1) == (HereDoc x2 y2) =
    (x1 == x2) && (y1 == y2)

  -----------------------
  -- I/O and Redirection
  -----------------------
  (Read v1) == (Read v2)     = v1 == v2
  (Echo x1) == (Echo x2)     = x1 == x2
  (Printf fmt1 xs1) == (Printf fmt2 xs2) =
    (fmt1 == fmt2) && (xs1 == xs2)

  (Redirect c1 op1 a1) == (Redirect c2 op2 a2) =
    (c1 == c2) && (op1 == op2) && (a1 == a2)

  -----------------------
  -- Combining Commands
  -----------------------
  (Pipeline xs1) == (Pipeline xs2) = xs1 == xs2

  (JobControl conj1 l1 r1) == (JobControl conj2 l2 r2) =
    (conj1 == conj2) && (l1 == l2) && (r1 == r2)

  (Semicolon a1 b1) == (Semicolon a2 b2) =
    (a1 == a2) && (b1 == b2)

  (Not c1) == (Not c2) = c1 == c2

  (Background c1) == (Background c2) = c1 == c2
  (Disown c1) == (Disown c2)         = c1 == c2

  (Decorated d1 c1) == (Decorated d2 c2) =
    (d1 == d2) && (c1 == c2)

  (TryCatch t1 c1) == (TryCatch t2 c2) =
    (t1 == t2) && (c2 == c2)

  -- Everything else is not equal
  _ == _ = False

--------------------------------------------------------------------------------
---- 4. Arguments & Redirects
--------------------------------------------------------------------------------

data FishArgOrRedirect where
  Arg   :: FishArg a -> FishArgOrRedirect
  Redir :: RedirectOp -> FishArg TStr -> FishArgOrRedirect
deriving stock instance Show FishArgOrRedirect
instance Eq FishArgOrRedirect where
  (==) :: FishArgOrRedirect -> FishArgOrRedirect -> Bool
  Arg a == Arg b = a == b
  Redir op1 a1 == Redir op2 a2 = op1 == op2 && a1 == a2
  _ == _ = False

data FishArg (t :: FishType) where
  ArgLiteral     :: Text -> FishArg TStr
  ArgNumber      :: Int -> FishArg TInt
  ArgVariable    :: Text -> FishArg a
  ArgConcat      :: FishArg TStr -> FishArg TStr -> FishArg TStr
  ArgList        :: [FishArg a] -> FishArg (TList a)
  ArgSubstituted :: FishCommand a -> FishArg a
deriving stock instance Show (FishArg t)
instance Eq (FishArg t) where
  (==) :: FishArg t -> FishArg t -> Bool
  ArgLiteral a1 == ArgLiteral a2 = a1 == a2
  ArgNumber n1 == ArgNumber n2 = n1 == n2
  ArgVariable v1 == ArgVariable v2 = v1 == v2
  ArgConcat a1 b1 == ArgConcat a2 b2 = a1 == a2 && b1 == b2
  ArgList a1 == ArgList a2 = a1 == a2
  ArgSubstituted a1 == ArgSubstituted a2 = a1 == a2
  _ == _ = False


--------------------------------------------------------------------------------
---- 5. Expressions
--------------------------------------------------------------------------------

data FishExpr (t :: FishType) where
  -- String expressions
  ExprStringLit   :: String -> FishExpr TStr
  ExprStringVar   :: Text -> Maybe FishIndex -> FishExpr TStr
  ExprStringConcat :: FishExpr TStr -> FishExpr TStr -> FishExpr TStr

  -- Lists
  ExprListLit     :: [FishExpr a] -> FishExpr (TList a)

  -- Numeric expressions
  ExprNum         :: NumExpr -> FishExpr TInt

  -- Boolean expressions
  ExprBool        :: BoolExpr -> FishExpr TBool

  -- Command Substitution => type depends on the command
  ExprSubstituted :: FishCommand a -> FishExpr a

deriving stock instance Show (FishExpr t)
instance Eq (FishExpr t) where
  (==) :: FishExpr t -> FishExpr t -> Bool
  ExprStringLit s1 == ExprStringLit s2 = s1 == s2
  ExprStringVar v1 i1 == ExprStringVar v2 i2 = v1 == v2 && i1 == i2
  ExprStringConcat a1 b1 == ExprStringConcat a2 b2 = a1 == a2 && b1 == b2
  ExprListLit a1 == ExprListLit a2 = a1 == a2
  ExprNum n1 == ExprNum n2 = n1 == n2
  ExprBool b1 == ExprBool b2 = b1 == b2
  ExprSubstituted a1 == ExprSubstituted a2 = a1 == a2
  _ == _ = False

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
  = NumLiteral   Int
  | NumVariable  Text
  | NumArith     ArithOp NumExpr NumExpr
  deriving stock (Show, Eq)

-- | Boolean expression GADT allowing "test" on arbitrary FishArg types
data BoolExpr where
  BoolLiteral  :: Bool -> BoolExpr
  BoolVariable :: Text -> BoolExpr
  BoolAnd      :: BoolExpr -> BoolExpr -> BoolExpr
  BoolOr       :: BoolExpr -> BoolExpr -> BoolExpr
  BoolNot      :: BoolExpr -> BoolExpr
  BoolTestOp   :: forall a b.
                  TestOperator
               -> FishArg a
               -> FishArg b
               -> BoolExpr
  BoolCommand  :: FishCommand TStatus -> BoolExpr   -- (0 => True, otherwise => False)

deriving stock instance Show BoolExpr
instance Eq BoolExpr where
  (==) :: BoolExpr -> BoolExpr -> Bool
  BoolLiteral b1 == BoolLiteral b2 = b1 == b2
  BoolVariable v1 == BoolVariable v2 = v1 == v2
  BoolAnd a1 b1 == BoolAnd a2 b2 = a1 == a2 && b1 == b2
  BoolOr a1 b1 == BoolOr a2 b2 = a1 == a2 && b1 == b2
  BoolNot a1 == BoolNot a2 = a1 == a2
  BoolTestOp op1 x1 y1 == BoolTestOp op2 x2 y2 = (op1 == op2) && (x1 == x2) && (y1 == y2)
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

-- | Flags that can appear in a 'function' definition in fish, e.g.
--   function --description 'some desc' --on-event 'foo' ...
data FunctionFlag
  = FuncDescription Text
  | FuncOnEvent Text
  | FuncHelp
  | FuncInheritVariable
  | FuncUnknownFlag Text  -- catch-all for unmodeled flags
  deriving stock (Show, Eq)

-- | A separate record for function definitions
data FishFunction = FishFunction
  { funcName   :: Text
  , funcFlags  :: [FunctionFlag]
  , funcParams :: [FishArg TStr]
  , funcBody   :: [FishStatement]
  }
deriving stock instance Show FishFunction
deriving stock instance Eq FishFunction

-- (Show, Eq)

--------------------------------------------------------------------------------
---- 9. Variable Scopes for 'set'
--------------------------------------------------------------------------------

data VariableScope
  = ScopeLocal
  | ScopeGlobal
  | ScopeExported   -- similar to 'set -x' for environment variables
  | ScopeUniversal
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
---- 10. Try/Catch is now in 'FishCommand' (TryCatch constructor)
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
---- 11. Job Control: unify "and"/"or" with job control
--------------------------------------------------------------------------------

data JobConjunction
  = ConjAnd   -- 'and', or '&&'
  | ConjOr    -- 'or',  or '||'
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
---- 12. Misc (Case Items, RedirectOp, etc.)
--------------------------------------------------------------------------------

data CaseItem = CaseItem
  { casePatterns :: [FishArg TStr]
  , caseBody     :: [FishStatement]
  }
deriving stock instance Show CaseItem
deriving stock instance Eq CaseItem

newtype FreestandingArgumentList = FreestandingArgumentList [FishArg TStr]
deriving stock instance Show FreestandingArgumentList
deriving stock instance Eq FreestandingArgumentList

data RedirectOp
  = RedirectOut       -- >
  | RedirectOutAppend -- >>
  | RedirectIn        -- <
  | RedirectErr       -- ^>
  | RedirectErrAppend -- ^>>
  | RedirectBoth      -- &>
  | RedirectBothAppend -- &>>
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
---- 13. Optional Source Tracking (omitted for now)
--------------------------------------------------------------------------------

data SourceRange = SourceRange
  { startPos :: Bash.Position
  , endPos   :: Bash.Position 
  } deriving stock (Show, Eq)
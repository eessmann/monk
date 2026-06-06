{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Fish.DSL.Internal
  ( Expr (..),
    Arg (..),
    Command (..),
    CommandRole (..),
    CommandResult,
    Stmt (..),
    Block (..),
    Stage (..),
    Pipeline (..),
    JobContinuation (..),
    JobConjunction (..),
    JobList (..),
    CaseItem (..),
    Script (..),
    Index (..),
    IndexShape (..),
    IndexResult,
    RedirectStream (..),
    RedirectMode (..),
    RedirectTarget (..),
    ArgumentType,
    lowerExpr,
    lowerArg,
    lowerCommand,
    lowerStmt,
    lowerBlock,
    lowerStage,
    lowerPipelineValue,
    lowerJobContinuation,
    lowerJobConjunction,
    lowerJobList,
    lowerCaseItem,
    lowerPipeline,
    lowerPipelineWithTime,
    lowerScript,
    lowerIndex,
    lowerRedirectStream,
    lowerRedirectMode,
    lowerRedirectTarget,
  )
where

import Data.Type.Equality (testEquality, (:~:) (Refl))
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Language.Fish.AST qualified as Raw
import Type.Reflection (typeRep)

data Expr (t :: Raw.FishType) where
  UnsafeExpr :: (Typeable t) => Raw.FishExpr t -> Expr t

deriving stock instance Show (Expr t)

instance Eq (Expr t) where
  UnsafeExpr left == UnsafeExpr right = left == right

type family ArgumentType (t :: Raw.FishType) :: Constraint where
  ArgumentType 'Raw.TStr = ()
  ArgumentType 'Raw.TInt = ()
  ArgumentType ('Raw.TList 'Raw.TStr) = ()
  ArgumentType ('Raw.TList 'Raw.TInt) = ()
  ArgumentType other =
    TypeError ('Text "Fish command arguments must be string, integer, or renderable list expressions.")

data Arg where
  UnsafeArgExpr :: (ArgumentType t, Typeable t) => Expr t -> Arg
  UnsafeArgRedirect :: Raw.Redirect -> Arg

deriving stock instance Show Arg

instance Eq Arg where
  UnsafeArgExpr (left :: Expr a) == UnsafeArgExpr (right :: Expr b) =
    case testEquality (typeRep @a) (typeRep @b) of
      Just Refl -> left == right
      Nothing -> False
  UnsafeArgRedirect left == UnsafeArgRedirect right = left == right
  _ == _ = False

data CommandRole
  = ReturnsStatus
  | ReturnsUnit
  deriving stock (Show, Eq)

type family CommandResult (r :: CommandRole) :: Raw.FishType where
  CommandResult 'ReturnsStatus = 'Raw.TStatus
  CommandResult 'ReturnsUnit = 'Raw.TUnit

data Command (r :: CommandRole) where
  UnsafeCommand ::
    (Typeable (CommandResult r)) =>
    Raw.FishCommand (CommandResult r) ->
    Command r

deriving stock instance Show (Command r)

instance Eq (Command r) where
  UnsafeCommand left == UnsafeCommand right = left == right

newtype Stmt = UnsafeStmt Raw.FishStatement
  deriving stock (Show, Eq)

newtype Block = UnsafeBlock (NonEmpty Stmt)
  deriving stock (Show, Eq)

newtype Stage = UnsafeStage (Command 'ReturnsStatus)
  deriving stock (Show, Eq)

newtype Pipeline = UnsafePipeline Raw.FishJobPipeline
  deriving stock (Show, Eq)

data JobContinuation
  = UnsafeAndThen Pipeline
  | UnsafeOrElse Pipeline
  deriving stock (Show, Eq)

newtype JobConjunction = UnsafeJobConjunction Raw.FishJobConjunction
  deriving stock (Show, Eq)

newtype JobList = UnsafeJobList Raw.FishJobList
  deriving stock (Show, Eq)

newtype CaseItem = UnsafeCaseItem Raw.CaseItem
  deriving stock (Show, Eq)

newtype Script = UnsafeScript [Stmt]
  deriving stock (Show, Eq)

data IndexShape
  = IndexOne
  | IndexRange
  | IndexMany
  deriving stock (Show, Eq)

type family IndexResult (shape :: IndexShape) (element :: Raw.FishType) :: Raw.FishType where
  IndexResult 'IndexOne element = element
  IndexResult 'IndexRange element = 'Raw.TList element
  IndexResult 'IndexMany element = 'Raw.TList element

data Index (shape :: IndexShape) where
  UnsafeIndexSingle :: Expr 'Raw.TInt -> Index 'IndexOne
  UnsafeIndexRange :: Maybe (Expr 'Raw.TInt) -> Maybe (Expr 'Raw.TInt) -> Index 'IndexRange
  UnsafeIndexList :: NonEmpty (Expr 'Raw.TInt) -> Index 'IndexMany

deriving stock instance Show (Index shape)

instance Eq (Index shape) where
  UnsafeIndexSingle left == UnsafeIndexSingle right = left == right
  UnsafeIndexRange leftStart leftEnd == UnsafeIndexRange rightStart rightEnd =
    leftStart == rightStart && leftEnd == rightEnd
  UnsafeIndexList left == UnsafeIndexList right = left == right

data RedirectStream
  = Stdout
  | Stderr
  | Stdin
  | Both
  | Fd Int
  deriving stock (Show, Eq)

data RedirectMode
  = Overwrite
  | Append
  | Input
  | Clobber
  | ReadWrite
  deriving stock (Show, Eq)

newtype RedirectTarget = UnsafeRedirectTarget Raw.RedirectTarget
  deriving stock (Show, Eq)

lowerExpr :: Expr t -> Raw.FishExpr t
lowerExpr (UnsafeExpr expr) = expr

lowerArg :: Arg -> Raw.ExprOrRedirect
lowerArg = \case
  UnsafeArgExpr expr -> Raw.ExprVal (lowerExpr expr)
  UnsafeArgRedirect redir -> Raw.RedirectVal redir

lowerCommand :: Command r -> Raw.FishCommand (CommandResult r)
lowerCommand (UnsafeCommand cmd) = cmd

lowerStmt :: Stmt -> Raw.FishStatement
lowerStmt (UnsafeStmt stmt) = stmt

lowerBlock :: Block -> NonEmpty Raw.FishStatement
lowerBlock (UnsafeBlock stmts) = lowerStmt <$> stmts

lowerStage :: Stage -> Raw.FishStatement
lowerStage (UnsafeStage cmd) = Raw.Stmt (lowerCommand cmd)

lowerPipelineValue :: Pipeline -> Raw.FishJobPipeline
lowerPipelineValue (UnsafePipeline pipe) = pipe

lowerJobContinuation :: JobContinuation -> Raw.FishJobConjCont
lowerJobContinuation = \case
  UnsafeAndThen pipe -> Raw.JCAnd (lowerPipelineValue pipe)
  UnsafeOrElse pipe -> Raw.JCOr (lowerPipelineValue pipe)

lowerJobConjunction :: JobConjunction -> Raw.FishJobConjunction
lowerJobConjunction (UnsafeJobConjunction conj) = conj

lowerJobList :: JobList -> Raw.FishJobList
lowerJobList (UnsafeJobList jobs) = jobs

lowerCaseItem :: CaseItem -> Raw.CaseItem
lowerCaseItem (UnsafeCaseItem item) = item

lowerPipeline :: NonEmpty Stage -> Raw.FishJobPipeline
lowerPipeline = lowerPipelineWithTime False

lowerPipelineWithTime :: Bool -> NonEmpty Stage -> Raw.FishJobPipeline
lowerPipelineWithTime timed (headStage :| rest) =
  Raw.MkFishJobPipeline
    { Raw.jpTime = timed,
      Raw.jpVariables = [],
      Raw.jpStatement = lowerStage headStage,
      Raw.jpCont = pipeContinuation <$> rest,
      Raw.jpBackgrounded = False
    }
  where
    pipeContinuation next =
      Raw.PipeTo
        { Raw.jpcVariables = [],
          Raw.jpcStatement = lowerStage next
        }

lowerScript :: Script -> [Raw.FishStatement]
lowerScript (UnsafeScript stmts) = lowerStmt <$> stmts

lowerIndex :: Index shape -> Raw.FishIndex element (IndexResult shape element)
lowerIndex = \case
  UnsafeIndexSingle expr -> Raw.IndexSingle (lowerExpr expr)
  UnsafeIndexRange start end -> Raw.IndexRange (lowerExpr <$> start) (lowerExpr <$> end)
  UnsafeIndexList indexes -> Raw.IndexList (lowerExpr <$> indexes)

lowerRedirectStream :: RedirectStream -> Raw.RedirectSource
lowerRedirectStream = \case
  Stdout -> Raw.RedirectStdout
  Stderr -> Raw.RedirectStderr
  Stdin -> Raw.RedirectStdin
  Both -> Raw.RedirectBoth
  Fd handle -> Raw.RedirectFD handle

lowerRedirectMode :: RedirectMode -> Raw.RedirectOp
lowerRedirectMode = \case
  Overwrite -> Raw.RedirectOut
  Append -> Raw.RedirectOutAppend
  Input -> Raw.RedirectIn
  Clobber -> Raw.RedirectClobber
  ReadWrite -> Raw.RedirectReadWrite

lowerRedirectTarget :: RedirectTarget -> Raw.RedirectTarget
lowerRedirectTarget (UnsafeRedirectTarget target) = target

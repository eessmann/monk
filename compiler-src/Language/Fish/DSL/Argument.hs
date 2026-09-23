{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Argument fields exclude redirections and retain expansion cardinality.
module Language.Fish.DSL.Argument
  ( Argument (..),
    SomeArgument (..),
    argumentExpression,
  )
where

import GHC.Show (Show (showsPrec))
import Language.Fish.DSL.Internal (ExprOrRedirect (ExprVal), FishExpr, FishType (..))
import Monk.Compiler.Index (Cardinality (..))

type role Argument nominal

data Argument (cardinality :: Cardinality) where
  ScalarArgument :: FishExpr TStr -> Argument ExactlyOne
  ListArgument :: FishExpr (TList TStr) -> Argument FieldSequence

data SomeArgument where
  SomeArgument :: Argument cardinality -> SomeArgument

argumentExpression :: SomeArgument -> ExprOrRedirect
argumentExpression (SomeArgument (ScalarArgument value)) = ExprVal value
argumentExpression (SomeArgument (ListArgument values)) = ExprVal values

deriving stock instance Show (Argument cardinality)

deriving stock instance Eq (Argument cardinality)

instance Show SomeArgument where
  showsPrec precedence = showsPrec precedence . argumentExpression

instance Eq SomeArgument where
  left == right = argumentExpression left == argumentExpression right

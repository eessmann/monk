{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

-- | Bounded indices shared by the semantic plan and materializer. Finite flow
-- environments remain ordinary maps; none of these indices encodes a map.
module Monk.Compiler.Index
  ( Domain (..),
    Cardinality (..),
    SCardinality (..),
    World,
    withWorld,
  )
where

import Data.Singletons (Sing, SingI (..))
import Data.Type.Equality (TestEquality (..), (:~:) (Refl))

data Domain = Bytes | Fields

data Cardinality = ExactlyOne | FieldSequence

data SCardinality (cardinality :: Cardinality) where
  SExactlyOne :: SCardinality ExactlyOne
  SFieldSequence :: SCardinality FieldSequence

deriving stock instance Show (SCardinality cardinality)

instance TestEquality SCardinality where
  testEquality SExactlyOne SExactlyOne = Just Refl
  testEquality SFieldSequence SFieldSequence = Just Refl
  testEquality _ _ = Nothing

-- | Constructors stay private: distinct continuations cannot share evidence.
type role World nominal

data World (world :: Type) = World

withWorld :: (forall world. World world -> result) -> result
withWorld consume = consume World

type instance Sing @Cardinality = SCardinality

instance SingI ExactlyOne where
  sing = SExactlyOne

instance SingI FieldSequence where
  sing = SFieldSequence

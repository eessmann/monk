{-# LANGUAGE RoleAnnotations #-}

-- | A value and its ordered defining statements. There is no projection of
-- either component: values can only be transformed inside an emission, and
-- the only eliminator closes a complete statement consumer.
module Language.Fish.Translator.Emission
  ( Emission,
    emit,
    renderEmission,
  )
where

import Data.Sequence qualified as Seq
import Language.Fish.DSL.Internal (FishStatement)

type role Emission nominal

data Emission value = Emission (Seq FishStatement) value

instance Functor Emission where
  fmap f (Emission statements value) = Emission statements (f value)

instance Applicative Emission where
  pure = Emission Seq.empty
  Emission before f <*> Emission after value = Emission (before <> after) (f value)

instance Monad Emission where
  Emission before value >>= continue = case continue value of
    Emission after result -> Emission (before <> after) result

emit :: [FishStatement] -> Emission ()
emit statements = Emission (Seq.fromList statements) ()

renderEmission :: Emission [FishStatement] -> [FishStatement]
renderEmission (Emission statements final) = toList statements <> final

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Ordered operand regions. Scoped values become usable only after their
-- defining emissions are closed together with their consumer.
module Language.Fish.Translator.Region
  ( Region,
    Scalar,
    Fields,
    ClosedFields,
    captureScalar,
    literalScalar,
    concatScalars,
    runScalar,
    runTaggedScalars,
    scalarField,
    runFields,
    closedSingle,
    closedSequence,
    fieldsEmission,
    sequenceFields,
    concatFields,
  )
where

import Control.Monad.State.Strict qualified as State
import Language.Fish.DSL.Argument
import Language.Fish.DSL.Internal
import Language.Fish.Translator.Emission (Emission)
import Language.Fish.Translator.Emission qualified as Emission
import Language.Fish.Translator.Statement (assign, scalarVar)
import Monk.Compiler.Index (Cardinality (..))

type role Region nominal nominal nominal

newtype Region (region :: Type) m value = Region (State.StateT (Emission ()) m value)
  deriving newtype (Functor, Applicative, Monad)

type role Scalar nominal

newtype Scalar (region :: Type) = Scalar (FishExpr TStr)

type role Fields nominal nominal

data Fields (region :: Type) (cardinality :: Cardinality) where
  OneField :: FishExpr TStr -> Fields region ExactlyOne

type role ClosedFields nominal

newtype ClosedFields (cardinality :: Cardinality) = ClosedFields (Emission [SomeArgument])

emit :: (Monad m) => Emission () -> Region region m ()
emit statements = Region (State.modify' (>> statements))

liftRegion :: (Monad m) => m value -> Region region m value
liftRegion = Region . lift

captureScalar :: (Monad m) => m Identifier -> m (Emission (FishExpr TStr)) -> Region region m (Scalar region)
captureScalar allocate materialize = do
  value <- liftRegion materialize
  temporary <- liftRegion allocate
  emit $ value >>= \expression -> Emission.emit [assign [SetLocal] temporary expression]
  pure (Scalar (scalarVar temporary))

literalScalar :: Text -> Scalar region
literalScalar = Scalar . ExprLiteral

concatScalars :: [Scalar region] -> Scalar region
concatScalars = Scalar . foldl' ExprStringConcat (ExprLiteral "") . map scalarValue

runScalar :: (Monad m) => (forall region. Region region m (Scalar region)) -> m (Emission (FishExpr TStr))
runScalar (Region action) = do
  (value, statements) <- State.runStateT action (pure ())
  pure (statements >> pure (scalarValue value))

runTaggedScalars :: (Monad m) => (forall region. Region region m [(tag, Scalar region)]) -> m (Emission [(tag, FishExpr TStr)])
runTaggedScalars (Region action) = do
  (values, statements) <- State.runStateT action (pure ())
  pure (statements >> pure (map (second scalarValue) values))

scalarValue :: Scalar region -> FishExpr TStr
scalarValue (Scalar expression) = expression

scalarField :: Scalar region -> Fields region ExactlyOne
scalarField (Scalar expression) = OneField expression

runFields :: (Monad m) => (forall region. Region region m (Fields region cardinality)) -> m (ClosedFields cardinality)
runFields (Region action) = do
  (OneField value, statements) <- State.runStateT action (pure ())
  pure (ClosedFields (statements >> pure [SomeArgument (ScalarArgument value)]))

closedSingle :: FishExpr TStr -> ClosedFields ExactlyOne
closedSingle value = ClosedFields (pure [SomeArgument (ScalarArgument value)])

closedSequence :: Emission [SomeArgument] -> ClosedFields FieldSequence
closedSequence = ClosedFields

-- Exposing an emission retains ownership; only a complete statement consumer
-- can render it. No caller can extract or reorder its prelude separately.
fieldsEmission :: ClosedFields cardinality -> Emission [SomeArgument]
fieldsEmission (ClosedFields value) = value

sequenceFields :: ClosedFields cardinality -> ClosedFields FieldSequence
sequenceFields (ClosedFields value) = ClosedFields value

concatFields :: [ClosedFields FieldSequence] -> ClosedFields FieldSequence
concatFields = ClosedFields . fmap concat . traverse fieldsEmission

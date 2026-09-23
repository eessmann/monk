-- | Finite array facts: an unknown identity is not a dense length proof.
module Language.Bash.Plan.Facts
  ( ArrayShape (..),
    DenseLength (..),
    appendDense,
    writeDense,
    joinArrayShapes,
  )
where

import Data.Map.Strict qualified as M

data ArrayShape = UnknownArray | DenseArray DenseLength
  deriving stock (Show, Eq, Ord)

data DenseLength = KnownLength Natural | DynamicLength
  deriving stock (Show, Eq, Ord)

appendDense :: DenseLength -> Natural -> DenseLength
appendDense DynamicLength _ = DynamicLength
appendDense (KnownLength size) count = KnownLength (size + count)

-- | A dynamic read proves only that replacing element zero preserves density.
writeDense :: DenseLength -> Int -> Maybe DenseLength
writeDense DynamicLength 0 = Just DynamicLength
writeDense DynamicLength _ = Nothing
writeDense (KnownLength size) offset
  | offset >= 0, fromIntegral offset <= size = Just (KnownLength (max size (fromIntegral offset + 1)))
  | otherwise = Nothing

-- | A join that loses a density proof retains the identity, preventing a later
-- scalar operation from accidentally deleting an array tail.
joinArrayShapes :: Map Text ArrayShape -> Map Text ArrayShape -> Map Text ArrayShape
joinArrayShapes = M.mergeWithKey (\_ a b -> Just (if a == b then a else UnknownArray)) (M.map (const UnknownArray)) (M.map (const UnknownArray))

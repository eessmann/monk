{-# LANGUAGE OverloadedStrings #-}

module CrossWorldDenseProof where

import qualified Data.Map.Strict as M
import Language.Bash.Plan.Facts (ArrayShape (UnknownArray))
import Language.Bash.Plan.Normalize.State
  ( FactData (factArrays),
    Normalization,
    lookupDense,
    transitionDense,
    updateNormalization,
    withNormalizationFacts,
    writeDenseAt,
  )

-- A real invalidation opens a new fact world. The old proof cannot establish
-- density for that world's write consumer, even when the variable name agrees.
invalid :: Normalization scope -> Either () (Normalization scope)
invalid before = withNormalizationFacts before $ \oldFacts -> do
  proof <- maybe (Left ()) Right (lookupDense oldFacts "values")
  let after = updateNormalization id (\facts -> facts {factArrays = M.singleton "values" UnknownArray}) id before
  transitionDense (\currentFacts -> maybe (Left ()) Right (writeDenseAt currentFacts proof 0)) after

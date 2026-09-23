{-# LANGUAGE OverloadedStrings #-}

module StaleDenseUpdate where

import qualified Data.Map.Strict as M
import Language.Bash.Plan.Facts (ArrayShape (UnknownArray))
import Language.Bash.Plan.Normalize.State

-- Applying a formerly valid update after invalidation must fail, even if the
-- old proof was consumed before the transition.
invalid :: Normalization scope -> Either () (Normalization scope)
invalid before = withNormalizationFacts before $ \facts -> do
  proof <- maybe (Left ()) Right (lookupDense facts "values")
  update <- maybe (Left ()) Right (writeDenseAt facts proof 0)
  let after = updateNormalization id (\current -> current {factArrays = M.singleton "values" UnknownArray}) id before
  transitionDense (\_ -> Right update) after

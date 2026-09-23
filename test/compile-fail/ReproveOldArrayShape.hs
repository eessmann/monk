module ReproveOldArrayShape where

import Language.Bash.Plan.Facts (ArrayShape)
import Language.Bash.Plan.Normalize.State (DenseProof, FlowFacts, lookupDense)

-- An arbitrary old unindexed shape cannot mint a proof in the current world.
invalid :: FlowFacts world -> ArrayShape -> Maybe (DenseProof world)
invalid = lookupDense

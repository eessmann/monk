module CoerceDenseProof where

import Data.Coerce (coerce)
import Language.Bash.Plan.Normalize.State (DenseProof)

invalid :: DenseProof left -> DenseProof right
invalid = coerce

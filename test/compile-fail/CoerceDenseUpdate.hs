module CoerceDenseUpdate where

import Data.Coerce (coerce)
import Language.Bash.Plan.Normalize.State (DenseUpdate)

invalid :: DenseUpdate oldWorld -> DenseUpdate currentWorld
invalid = coerce

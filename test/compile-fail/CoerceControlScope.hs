module CoerceControlScope where

import Data.Coerce (coerce)
import Language.Bash.Plan.Control (LoopTarget)

invalid :: LoopTarget outer -> LoopTarget inner
invalid = coerce

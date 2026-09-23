module CoerceRegionScalar where

import Data.Coerce (coerce)
import Language.Fish.Translator.Region (Scalar)

coerceRegionScalar :: Scalar old -> Scalar new
coerceRegionScalar = coerce

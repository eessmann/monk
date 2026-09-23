module CoerceEmission where

import Data.Coerce (coerce)
import Language.Fish.Translator.Emission (Emission)

newtype Checked = Checked Int

-- Nominal payload roles prevent manufacturing a new proof from its storage.
invalid :: Emission Int -> Emission Checked
invalid = coerce

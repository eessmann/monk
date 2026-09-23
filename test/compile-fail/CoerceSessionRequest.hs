module CoerceSessionRequest where

import Data.Coerce (coerce)
import Language.Fish.Translator.Session.Request (Request)

invalid :: Request original -> Request swapped
invalid = coerce

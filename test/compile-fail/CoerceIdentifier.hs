module CoerceIdentifier where

import Data.Coerce (coerce)
import qualified Data.Text as T
import Language.Fish.DSL (Identifier)

invalid :: T.Text -> Identifier
invalid = coerce

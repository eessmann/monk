module CoerceCommandName where

import Data.Coerce (coerce)
import Data.Text (Text)
import Language.Fish.DSL

invalid :: Text -> CommandName
invalid = coerce

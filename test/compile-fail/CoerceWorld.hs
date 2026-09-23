module CoerceWorld where

import Data.Coerce (coerce)
import Monk.Compiler.Index (World)

invalid :: World left -> World right
invalid = coerce

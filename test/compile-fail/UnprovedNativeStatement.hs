module UnprovedNativeStatement where

import Language.Bash.Plan (Statement)
import Language.Bash.Plan.Effects (nativeRegionStatement)

-- Native conversion consumes the certified statement, not arbitrary syntax.
invalid :: Statement scope -> Statement scope
invalid = nativeRegionStatement

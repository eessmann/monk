{-# LANGUAGE DataKinds #-}

module CoerceOwnedPlan where

import Data.Coerce (coerce)
import Language.Bash.Plan (OwnedPlan)
import Monk.Compiler.Context (Phase (Normalized))

invalid :: OwnedPlan owner target entry provider Normalized -> OwnedPlan otherOwner target entry provider Normalized
invalid = coerce

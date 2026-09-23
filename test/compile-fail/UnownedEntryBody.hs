{-# LANGUAGE DataKinds #-}

module UnownedEntryBody where

import Language.Bash.Plan
import Language.Bash.Plan.Control
import Monk.Compiler.Context

invalid :: Context owner target entry provider -> ScopedBody EntryRootKind -> OwnedPlan owner target entry provider Normalized
invalid context body = sealSourcePlan context body mempty

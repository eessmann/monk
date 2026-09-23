{-# LANGUAGE DataKinds #-}

module CrossOwnerEntryBody where

import Language.Bash.Plan
import Monk.Compiler.Context

invalid :: Context owner target entry provider -> EntryBody otherOwner entry -> OwnedPlan owner target entry provider Normalized
invalid context body = sealSourcePlan context body mempty

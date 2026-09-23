{-# LANGUAGE DataKinds #-}

module LoopAsFunctionRoot where

import Language.Bash.Plan
import Language.Bash.Plan.Control
import Monk.Translation.Types

invalid :: ScopedBody FunctionRootKind
invalid = withEntryControl Standalone $ \entry ->
  withLoopControl entry $ \_ loop -> scopedBody (rootWitness loop) []

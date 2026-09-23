{-# LANGUAGE DataKinds #-}

module CoerceBodyKind where

import Data.Coerce (coerce)
import Language.Bash.Plan
import Language.Bash.Plan.Control

invalid :: ScopedBody ChildRootKind -> ScopedBody FunctionRootKind
invalid = coerce

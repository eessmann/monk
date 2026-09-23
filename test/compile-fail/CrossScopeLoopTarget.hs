{-# LANGUAGE DataKinds #-}

module CrossScopeLoopTarget where

import Language.Bash.Plan (ScopedBody, Statement (Statement), StatementNode (Break), scopedBody)
import Language.Bash.Plan.Control
import Monk.Translation.Types (EntryMode (Standalone))

-- A target from the enclosing loop cannot be placed in a fresh child body.
invalid :: ScopedBody ChildRootKind
invalid = withEntryControl Standalone $ \entry ->
  withLoopControl entry $ \target loop ->
    withChildControl loop $ \root _ ->
      scopedBody root [Statement Nothing (Break target)]

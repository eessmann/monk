module CrossLoopBodyTarget where

import Language.Bash.Plan (ForBody, Statement (Statement), StatementNode (Break), forBody)
import Language.Bash.Plan.Control
import Monk.Translation.Types (EntryMode (Standalone))

-- Both loops belong to one function, but the inner target cannot escape its body.
invalid :: ForBody
invalid = withEntryControl Standalone $ \entry ->
  withLoopControl entry $ \outer outerScope ->
    withLoopControl outerScope $ \inner _ ->
      forBody (rootWitness outerScope) outer [Statement Nothing (Break inner)]

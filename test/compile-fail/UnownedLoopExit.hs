module UnownedLoopExit where

import Language.Bash.Plan (StatementNode)
import Language.Bash.Plan.Normalize.Control (Control, breakStatement)

-- A control root itself is not evidence that it owns a break target.
invalid :: Control scope -> StatementNode scope
invalid = breakStatement

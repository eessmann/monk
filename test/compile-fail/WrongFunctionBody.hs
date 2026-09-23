{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module WrongFunctionBody where

import Language.Bash.Plan
import Language.Bash.Plan.Control

invalid :: ScopedBody ChildRootKind -> StatementNode scope
invalid = DefineFunction "f"

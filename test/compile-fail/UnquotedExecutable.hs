{-# LANGUAGE OverloadedStrings #-}

module UnquotedExecutable where

import Language.Fish.DSL.Internal

invalid = CommandExpr (ExprVariable (VarScalar "command")) []

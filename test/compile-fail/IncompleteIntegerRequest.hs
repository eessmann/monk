{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module IncompleteIntegerRequest where

import Language.Bash.Arithmetic.Plan (BinaryOperator (Add))
import Language.Fish.DSL.Internal (FishExpr (ExprLiteral))
import Language.Fish.Translator.Primitive
import Monk.Runtime.Abi2 (CliOpcode (CliInteger))

incomplete :: Primitive CliInteger
incomplete = BinaryInteger Add (ExprLiteral "1")

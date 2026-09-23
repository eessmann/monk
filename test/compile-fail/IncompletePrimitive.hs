{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module IncompletePrimitive where

import Language.Fish.DSL.Internal (FishExpr (ExprLiteral))
import Language.Fish.Translator.Primitive
import Monk.Runtime.Abi2 (CliOpcode (CliSplit))

incomplete :: Primitive CliSplit
incomplete = SplitFields (ExprLiteral " ")

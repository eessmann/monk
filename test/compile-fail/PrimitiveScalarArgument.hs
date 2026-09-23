{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module PrimitiveScalarArgument where

import Language.Fish.DSL.Internal (FishExpr (..), FishVarRef (VarAll))
import Language.Fish.Translator.Primitive
import Monk.Runtime.Abi2 (CliOpcode (CliSplit))

invalid :: Primitive CliSplit
invalid = SplitFields (ExprVariable (VarAll "ifs")) (ExprLiteral "value")

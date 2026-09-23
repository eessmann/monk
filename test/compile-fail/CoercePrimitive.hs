{-# LANGUAGE DataKinds #-}

module CoercePrimitive where

import Data.Coerce (coerce)
import Language.Fish.Translator.Primitive (Primitive)
import Monk.Runtime.Abi2 (CliOpcode (CliArgv, CliSplit))

invalid :: Primitive CliSplit -> Primitive CliArgv
invalid = coerce

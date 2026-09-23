{-# LANGUAGE DataKinds #-}

module CrossRegionScalar where

import Data.Functor.Identity (Identity)
import Language.Fish.DSL.Internal (FishExpr, FishType (TStr))
import Language.Fish.Translator.Emission (Emission)
import qualified Language.Fish.Translator.Region as Region

crossRegionScalar :: Region.Scalar outer -> Identity (Emission (FishExpr TStr))
crossRegionScalar value = Region.runScalar (pure value)

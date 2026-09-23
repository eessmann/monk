{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module SessionNativeCapability where

import Data.Type.Equality ((:~:) (Refl))
import Language.Bash.Plan.Effects (Effect (Session), NativeEligible)

invalid :: NativeEligible Session :~: True
invalid = Refl

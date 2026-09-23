{-# LANGUAGE DataKinds #-}

module CoerceProvider where

import Data.Coerce (coerce)
import Monk.Compiler.Artifact (Artifact, Phase (Admitted))

invalid :: Artifact owner target entry provider Admitted -> Artifact owner target entry otherProvider Admitted
invalid = coerce

{-# LANGUAGE DataKinds #-}

module CoerceArtifactPhase where

import Data.Coerce (coerce)
import Monk.Compiler.Artifact (Artifact, Phase (Admitted, Draft))

invalid :: Artifact owner target entry provider Draft -> Artifact owner target entry provider Admitted
invalid = coerce

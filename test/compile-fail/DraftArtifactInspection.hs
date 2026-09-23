{-# LANGUAGE DataKinds #-}

module DraftArtifactInspection where

import Language.Fish.DSL.Internal (Script)
import Monk.Compiler.Artifact (Artifact, Phase (Draft), artifactEntry)

invalid :: Artifact owner target entry provider Draft -> Script
invalid = artifactEntry

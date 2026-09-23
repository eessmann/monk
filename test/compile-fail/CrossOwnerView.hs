module CrossOwnerView where

import Data.Coerce (coerce)
import Monk.Compiler.Artifact (ArtifactView)

invalid :: ArtifactView left target entry provider -> ArtifactView right target entry provider
invalid = coerce

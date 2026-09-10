module ForgeRuntimeArtifact (forge) where

import Monk.Output

forge :: NativeRuntimeImage -> NativeRuntimeArtifact
forge = MkNativeRuntimeArtifact OutputStdout

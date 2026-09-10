module UpdateBundle where

import Monk.Output

forge :: OutputBundle -> OutputBundle
forge value = value {bundleRuntimeArtifacts = []}

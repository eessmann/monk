module UpdateGraph where

import Monk.Source

forge :: SourceGraph -> SourceGraph
forge value = value {sourceRoot = "untrusted.bash"}

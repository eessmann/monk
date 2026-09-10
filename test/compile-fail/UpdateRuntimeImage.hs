module UpdateRuntimeImage (forge) where

import Monk.Output (NativeRuntimeImage, nativeImageBytes)

forge :: NativeRuntimeImage -> NativeRuntimeImage
forge image = image {nativeImageBytes = mempty}

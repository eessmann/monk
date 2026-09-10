module Positive (inspectResult, inspectGraph, inspectBundle, inspectRuntime, ordinaryScript) where

import Monk.AST (Script, script)
import Monk.Output (NativeRuntimeImage, OutputBundle, bundleUserFiles, nativeImageABI)
import Monk.Source (SourceGraph, sourceRoot)
import Monk.Translation (TranslationResult, translationScript)

inspectResult :: TranslationResult -> Script
inspectResult = translationScript

inspectGraph :: SourceGraph -> FilePath
inspectGraph = sourceRoot

inspectBundle :: OutputBundle -> Int
inspectBundle = length . bundleUserFiles

inspectRuntime :: NativeRuntimeImage -> Int
inspectRuntime = nativeImageABI

ordinaryScript :: Script
ordinaryScript = script []

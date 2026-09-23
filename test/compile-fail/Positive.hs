{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Positive (inspectResult, inspectGraph, inspectBundle, inspectRuntime, ordinaryScript, redirectedBlock, backgroundCommand) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Language.Fish.DSL as Fish
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

redirectedBlock :: Fish.Command Fish.BlockGrammar Fish.ReturnsStatus
redirectedBlock = Fish.beginWithRedirects (Fish.stmt (Fish.command "true" []) :| []) [Fish.redirect Fish.stdout Fish.overwrite (Fish.fileTarget (Fish.str "output"))]

backgroundCommand :: Fish.Command Fish.Asynchronous Fish.ReturnsStatus
backgroundCommand = Fish.background (Fish.command "true" [])

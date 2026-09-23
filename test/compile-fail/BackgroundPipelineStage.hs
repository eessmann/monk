{-# LANGUAGE OverloadedStrings #-}

module BackgroundPipelineStage where

import Language.Fish.DSL

invalid = stage (background (command "true" []))

{-# LANGUAGE OverloadedStrings #-}

module SequencePipelineStage where

import Language.Fish.DSL

invalid = stage (semicolon (command "true" []) (command "false" []))

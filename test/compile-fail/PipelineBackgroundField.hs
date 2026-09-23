{-# LANGUAGE OverloadedStrings #-}

module PipelineBackgroundField where

import Language.Fish.DSL.Internal

invalid :: FishJobPipeline
invalid = JobPipeline False [] (MkStage (Command "true" [])) [] True

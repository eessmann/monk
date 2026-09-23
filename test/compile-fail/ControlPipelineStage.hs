module ControlPipelineStage where

import Language.Fish.DSL

invalid = stage (return_ Nothing)

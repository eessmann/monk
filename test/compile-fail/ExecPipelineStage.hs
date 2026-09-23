{-# LANGUAGE OverloadedStrings #-}

module ExecPipelineStage where

import Language.Fish.DSL

invalid = stage (exec (literalExecutable "true") [])

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module NestedBackground where

import Language.Fish.DSL

invalid :: Command Asynchronous ReturnsStatus
invalid = background (background (command "true" []))

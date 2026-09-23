{-# LANGUAGE OverloadedStrings #-}

module RedirectCombinedInput where

import Language.Fish.DSL

invalid = redirect both input (fileTarget (str "input"))

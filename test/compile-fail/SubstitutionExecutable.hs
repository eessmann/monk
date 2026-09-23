{-# LANGUAGE OverloadedStrings #-}

module SubstitutionExecutable where

import Data.List.NonEmpty (NonEmpty (..))
import Language.Fish.DSL

invalid = execute (commandSubst (stmt (command "printf" [arg (str "true")]) :| [])) []

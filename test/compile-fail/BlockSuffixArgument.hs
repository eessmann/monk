{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module BlockSuffixArgument where

import Data.List.NonEmpty (NonEmpty (..))
import Language.Fish.DSL

invalid :: Command BlockGrammar ReturnsStatus
invalid = beginWithRedirects (stmt (command "true" []) :| []) [arg (str "not-a-redirection")]

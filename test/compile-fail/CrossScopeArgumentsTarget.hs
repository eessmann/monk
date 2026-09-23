{-# LANGUAGE OverloadedStrings #-}

module CrossScopeArgumentsTarget where

import Language.Bash.Plan.Control
import Monk.Translation.Types (EntryMode (Standalone))

invalid :: Maybe ()
invalid = withEntryControl Standalone $ \entry -> do
  target <- setArgumentsTarget entry
  withFunctionControl "f" entry $ \_ function ->
    Just (consumeSetArguments (rootWitness function) target)

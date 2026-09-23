{-# LANGUAGE DataKinds #-}

module RawCommandName where

import Data.Text (Text)
import Language.Fish.DSL

invalid :: Text -> Command Atomic ReturnsStatus
invalid name = command name []

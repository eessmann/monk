{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module RawFunctionParameter where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Language.Fish.DSL.Internal

invalid :: T.Text -> FishFunction
invalid name = MkFishFunction "f" [] [name] (EmptyStmt NE.:| [])

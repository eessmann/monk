{-# LANGUAGE DataKinds #-}

module RawVariableIdentifier where

import qualified Data.Text as T
import Language.Fish.DSL.Internal

invalid :: T.Text -> FishVarRef TStr
invalid = VarScalar

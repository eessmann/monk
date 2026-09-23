{-# LANGUAGE DataKinds #-}

module RawSetIdentifier where

import qualified Data.Text as T
import Language.Fish.DSL.Internal

invalid :: T.Text -> FishCommand Atomic TUnit
invalid name = Set [] name (ExprListLiteral [])

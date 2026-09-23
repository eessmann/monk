{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module SessionReplyCardinality where

import Language.Fish.DSL.Internal (FishExpr, FishType (TStr), Script (MkScript))
import qualified Language.Fish.Translator.Session.Request as Request

-- An endpoint response contains two fields, never a scalar executable value.
invalid :: FishExpr TStr
invalid = Request.replyExpression (Request.Substitution Request.Input (Request.singleBody (Request.BodyStage (MkScript []) []))) "test_"

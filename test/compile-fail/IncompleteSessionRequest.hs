{-# LANGUAGE OverloadedStrings #-}

module IncompleteSessionRequest where

import Language.Fish.DSL.Internal (FishExpr (ExprLiteral), FishStatement)
import qualified Language.Fish.Translator.Session as Session
import qualified Language.Fish.Translator.Session.Request as Request

-- An open request cannot omit its path, even inside the private compiler.
invalid :: FishStatement
invalid = Session.request "test_" (Request.FdOpen (Request.Site (ExprLiteral "source") (ExprLiteral "1")) 3 Request.WriteFile)

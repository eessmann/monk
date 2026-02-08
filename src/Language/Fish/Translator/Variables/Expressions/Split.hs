{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Variables.Expressions.Split
  ( splitOnIfsExpr,
    splitOnIfsListExpr,
  )
where

import Data.List.NonEmpty qualified as NE
import Language.Fish.AST

splitOnIfsExpr :: FishExpr TStr -> FishExpr (TList TStr)
splitOnIfsExpr expr =
  ExprCommandSubst
    ( Stmt
        ( Command
            "string"
            [ ExprVal (ExprLiteral "split"),
              ExprVal (ExprLiteral "--"),
              ExprVal (ExprVariable (VarScalar "IFS")),
              ExprVal expr
            ]
        )
        NE.:| []
    )

splitOnIfsListExpr :: FishExpr (TList TStr) -> FishExpr (TList TStr)
splitOnIfsListExpr listExpr =
  splitOnIfsExpr (ExprJoinList listExpr)

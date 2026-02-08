{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Pretty.Pattern
  ( prettyCaseItemWith,
    prettyCasePatternWith,
  )
where

import Data.List.NonEmpty qualified as NE
import Language.Fish.AST
import Language.Fish.Pretty.Expr (escapeFishString)
import Prettyprinter

prettyCaseItemWith :: (FishStatement -> Doc ann) -> (forall t. FishExpr t -> Doc ann) -> CaseItem -> Doc ann
prettyCaseItemWith prettyStmt prettyExpr (CaseItem pats body) =
  "case"
    <+> hsep (map (prettyCasePatternWith prettyExpr) (NE.toList pats))
    <> hardline
    <> indent 2 (vsep (map prettyStmt (NE.toList body)))

prettyCasePatternWith :: (FishExpr TStr -> Doc ann) -> FishExpr TStr -> Doc ann
prettyCasePatternWith prettyExpr other =
  case flattenPatternConcat other of
    Just parts -> mconcat (map (prettyCasePatternPart prettyExpr) parts)
    Nothing -> prettyCasePatternPart prettyExpr other

prettyCasePatternPart :: (FishExpr TStr -> Doc ann) -> FishExpr TStr -> Doc ann
prettyCasePatternPart prettyExpr = \case
  ExprLiteral txt -> escapeFishString txt
  other -> prettyExpr other

flattenPatternConcat :: FishExpr TStr -> Maybe [FishExpr TStr]
flattenPatternConcat expr =
  let parts = go expr
   in if length parts > 1 then Just parts else Nothing
  where
    go (ExprStringConcat a b) = go a <> go b
    go e = [e]

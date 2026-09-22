{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Pretty.Pattern
  ( prettyCaseItemWith,
    prettyCasePatternWith,
  )
where

import Data.List.NonEmpty qualified as NE
import Language.Fish.AST
import Prettyprinter

prettyCaseItemWith :: (FishStatement -> Doc ann) -> (forall t. FishExpr t -> Doc ann) -> CaseItem -> Doc ann
prettyCaseItemWith prettyStmt prettyExpr (MkCaseItem pats body) =
  "case"
    <+> hsep (map (prettyCasePatternWith prettyExpr) (NE.toList pats))
    <> hardline
    <> indent 2 (vsep (map prettyStmt (NE.toList body)))

prettyCasePatternWith :: (FishExpr TStr -> Doc ann) -> FishExpr TStr -> Doc ann
prettyCasePatternWith prettyExpr = prettyExpr

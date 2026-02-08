{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Pretty.Job
  ( prettyJobPipelineWith,
    prettyJobConjunctionWith,
    prettyJobListWith,
    prettyConjunction,
  )
where

import Data.List.NonEmpty qualified as NE
import Language.Fish.AST
import Prettyprinter
import Prelude hiding (group)

prettyJobPipelineWith :: (FishStatement -> Doc ann) -> (forall t. FishExpr t -> Doc ann) -> FishJobPipeline -> Doc ann
prettyJobPipelineWith prettyStmt prettyExpr (FishJobPipeline time vars stmt conts bg) =
  let timeDoc = if time then "command time" <> space else mempty
      varsDoc = if null vars then mempty else hsep (map (prettyVarAssign prettyExpr) vars) <> space
      headDoc = group (timeDoc <> varsDoc <> prettyStmt stmt)
      restDocs =
        map
          ( \(PipeTo v s) ->
              space
                <> "|"
                <+> (if null v then mempty else hsep (map (prettyVarAssign prettyExpr) v) <> space)
                <> prettyStmt s
          )
          conts
      pipesDoc = mconcat restDocs
   in group (headDoc <> pipesDoc <> if bg then space <> "&" else mempty)

prettyVarAssign :: (forall t. FishExpr t -> Doc ann) -> VariableAssignment -> Doc ann
prettyVarAssign prettyExpr (VariableAssignment name mval) =
  case mval of
    Nothing -> pretty name <> "="
    Just v -> pretty name <> "=" <> prettyExpr v

prettyJobConjunctionWith ::
  (FishStatement -> Doc ann) ->
  (forall t. FishExpr t -> Doc ann) ->
  FishJobConjunction ->
  Doc ann
prettyJobConjunctionWith prettyStmt prettyExpr (FishJobConjunction mdec job conts) =
  let headDoc = case mdec of
        Nothing -> prettyJobPipelineWith prettyStmt prettyExpr job
        Just d -> prettyConjunction d <+> prettyJobPipelineWith prettyStmt prettyExpr job
      rest = map prettyJCont conts
   in headDoc <> mconcat rest
  where
    prettyJCont = \case
      JCAnd jp -> space <> hardline <> prettyConjunction ConjAnd <+> prettyJobPipelineWith prettyStmt prettyExpr jp
      JCOr jp -> space <> hardline <> prettyConjunction ConjOr <+> prettyJobPipelineWith prettyStmt prettyExpr jp

prettyJobListWith :: (FishStatement -> Doc ann) -> (forall t. FishExpr t -> Doc ann) -> FishJobList -> Doc ann
prettyJobListWith prettyStmt prettyExpr (FishJobList jobs) =
  vsep (map (prettyJobConjunctionWith prettyStmt prettyExpr) (NE.toList jobs))

prettyConjunction :: Conjunction -> Doc ann
prettyConjunction = \case
  ConjAnd -> "and"
  ConjOr -> "or"

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Pretty.Statement
  ( prettyFish,
    renderFish,
    prettyFishStatement,
    isEmptyStatement,
  )
where

import Language.Fish.AST
import Language.Fish.Pretty.Command (prettyFishCommandWith)
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

--------------------------------------------------------------------------------
-- 1. Top-level pretty-printing
--------------------------------------------------------------------------------

-- | Pretty-print an entire Fish script (list of statements).
prettyFish :: [FishStatement] -> Doc ann
prettyFish = vsep . map prettyFishStatement . filter (not . isEmptyStatement)

-- | Render the Doc to final Text.
renderFish :: [FishStatement] -> Text
renderFish = renderStrict . layoutPretty defaultLayoutOptions . prettyFish

--------------------------------------------------------------------------------
-- 2. FishStatement
--------------------------------------------------------------------------------

prettyFishStatement :: FishStatement -> Doc ann
prettyFishStatement = \case
  Stmt cmd -> prettyFishCommandWith prettyFishStatement cmd
  StmtList stmts -> vsep (map prettyFishStatement (filter (not . isEmptyStatement) stmts))
  Comment txt -> "#" <> pretty txt
  EmptyStmt -> mempty

isEmptyStatement :: FishStatement -> Bool
isEmptyStatement = \case
  EmptyStmt -> True
  StmtList [] -> True
  _ -> False

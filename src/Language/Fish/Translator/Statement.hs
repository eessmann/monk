module Language.Fish.Translator.Statement
  ( toNonEmptyStmtList,
    isEmptyStatement,
    wrapPrelude,
  )
where

import Data.List.NonEmpty qualified as NE
import Language.Fish.AST
import Language.Fish.Translator.Hoist (beginIfNeeded)

-- | Drop empty statements and convert to NonEmpty if possible.
toNonEmptyStmtList :: [FishStatement] -> Maybe (NonEmpty FishStatement)
toNonEmptyStmtList stmts = NE.nonEmpty (filter (not . isEmptyStatement) stmts)

isEmptyStatement :: FishStatement -> Bool
isEmptyStatement = \case
  EmptyStmt -> True
  StmtList [] -> True
  _ -> False

wrapPrelude :: [FishStatement] -> FishCommand TStatus -> FishCommand TStatus
wrapPrelude = beginIfNeeded

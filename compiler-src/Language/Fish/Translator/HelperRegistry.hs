{-# LANGUAGE GADTs #-}

-- | Deterministic interning of structural helper definitions. A repeated name
-- may reuse only the same definition; conflicts are admission failures.
module Language.Fish.Translator.HelperRegistry
  ( Registry,
    empty,
    intern,
    names,
    definitions,
  )
where

import Control.Monad (foldM)
import Data.Map.Strict qualified as M
import Data.Sequence qualified as Seq
import Language.Fish.DSL.Internal
import Prelude hiding (empty)

newtype HelperId = HelperId Text
  deriving stock (Show, Eq, Ord)

data Registry = Registry (Map HelperId FishFunction) (Seq HelperId)

empty :: Registry
empty = Registry M.empty Seq.empty

intern :: [FishStatement] -> Registry -> Either Text Registry
intern statements initial = foldM insert initial statements
  where
    insert :: Registry -> FishStatement -> Either Text Registry
    insert (Registry entries order) (Stmt (Function function)) =
      let helperId = HelperId (funcName function)
       in case M.lookup helperId entries of
            Nothing -> pure (Registry (M.insert helperId function entries) (order Seq.|> helperId))
            Just existing
              | existing == function -> pure (Registry entries order)
              | otherwise -> Left ("Conflicting helper definition: " <> funcName function)
    insert _ _ = Left "Only structural function definitions may enter the helper registry"

names :: Registry -> [Text]
names (Registry _ order) = [name | HelperId name <- toList order]

definitions :: Registry -> [FishStatement]
definitions (Registry entries order) = [Stmt (Function function) | helperId <- toList order, Just function <- [M.lookup helperId entries]]

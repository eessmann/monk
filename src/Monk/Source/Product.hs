-- | Private ownership of normalized source graphs and their immutable inputs.
module Monk.Source.Product
  ( SourceGraph (..),
    SourceDependency (..),
    SourceOccurrence (..),
    OccurrenceId (..),
    ParserTokenId (..),
  )
where

import Language.Bash.Plan (SourcePlan)
import Language.Fish.Translator.Plan (PlannedTranslation)
import Monk.Source.Environment (SourceEnvironment, SourceSnapshot)
import Monk.Source.Location (SourceRange)
import Monk.Translation.Types (Diagnostic)

data SourceGraph = MkSourceGraph
  { graphRoot :: FilePath,
    graphEnvironment :: SourceEnvironment,
    graphSnapshots :: [SourceSnapshot],
    graphOccurrences :: [SourceOccurrence],
    graphPlan :: SourcePlan,
    graphTranslation :: PlannedTranslation,
    graphParseDiagnostics :: [Diagnostic]
  }
  deriving stock (Show, Eq)

data SourceDependency = MkSourceDependency FilePath Text
  deriving stock (Show, Eq)

newtype OccurrenceId = OccurrenceId {unOccurrenceId :: Int}
  deriving stock (Show, Eq)

newtype ParserTokenId = ParserTokenId Int
  deriving stock (Show, Eq)

data SourceOccurrence = MkSourceOccurrence
  { occurrenceSequence :: OccurrenceId,
    occurrenceTokenId :: ParserTokenId,
    occurrenceParent :: FilePath,
    occurrenceDependency :: FilePath,
    occurrenceRange :: Maybe SourceRange
  }
  deriving stock (Show, Eq)

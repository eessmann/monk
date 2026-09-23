-- | Definition identities keep source occurrences, caller imports and absence
-- disjoint. No file spelling or negative parser number doubles as a tag.
module Language.Bash.Plan.Identity
  ( SourceId,
    sourceId,
    OccurrenceId,
    occurrenceId,
    importedOccurrence,
    DefinitionIdentity (..),
  )
where

newtype SourceId = SourceId Text
  deriving stock (Show, Eq, Ord)

-- | Source identities preserve the parser's exact document spelling, including
-- synthetic input names; they are never interpreted as filesystem paths here.
sourceId :: Text -> SourceId
sourceId = SourceId

newtype OccurrenceId = OccurrenceId Natural
  deriving stock (Show, Eq, Ord)

occurrenceId :: Int -> Maybe OccurrenceId
occurrenceId number
  | number >= 0 = Just (OccurrenceId (fromIntegral number))
  | otherwise = Nothing

importedOccurrence :: Natural -> OccurrenceId
importedOccurrence = OccurrenceId

data DefinitionIdentity
  = SourceDefinition SourceId OccurrenceId
  | ImportedDefinition OccurrenceId
  | AbsentDefinition
  deriving stock (Show, Eq, Ord)

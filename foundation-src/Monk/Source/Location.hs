module Monk.Source.Location (SourcePos (..), SourceRange (..)) where

import Data.Text (Text)

-- | A source position in an input file (1-based line/column).
data SourcePos = MkSourcePos
  { srcFile :: Text,
    srcLine :: Int,
    srcColumn :: Int
  }
  deriving stock (Show, Eq, Ord)

-- | A source range with start and end positions.
data SourceRange = MkSourceRange
  { rangeStart :: SourcePos,
    rangeEnd :: SourcePos
  }
  deriving stock (Show, Eq, Ord)

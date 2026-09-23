-- | Closed semantic operations. ABI spellings are chosen only at materialization.
module Language.Bash.Plan.Operator
  ( NumericComparison (..),
    parseNumericComparison,
    comparisonName,
    comparisonArithmetic,
    Replacement (..),
    PatternTrim (..),
  )
where

import Data.List (lookup)
import Language.Bash.Arithmetic.Plan qualified as A

data NumericComparison = Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual
  deriving stock (Show, Eq)

parseNumericComparison :: Text -> Maybe NumericComparison
parseNumericComparison name = lookup name [("-eq", Equal), ("-ne", NotEqual), ("-lt", Less), ("-le", LessEqual), ("-gt", Greater), ("-ge", GreaterEqual)]

comparisonName :: NumericComparison -> Text
comparisonName = A.binaryName . comparisonArithmetic

comparisonArithmetic :: NumericComparison -> A.BinaryOperator
comparisonArithmetic = \case
  Equal -> A.Equal
  NotEqual -> A.NotEqual
  Less -> A.LessThan
  LessEqual -> A.LessEqual
  Greater -> A.GreaterThan
  GreaterEqual -> A.GreaterEqual

data Replacement = ReplaceFirst | ReplaceAll
  deriving stock (Show, Eq)

data PatternTrim = PrefixShort | PrefixLong | SuffixShort | SuffixLong
  deriving stock (Show, Eq)

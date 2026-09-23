-- | Canonical variable identities. Dynamic input is validated before it can
-- enter the structural DSL; literal instances are checked programmer claims.
module Language.Fish.DSL.Name
  ( Identifier,
    identifier,
    identifierText,
  )
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Text qualified as T

newtype Identifier = Identifier Text
  deriving stock (Show, Eq, Ord)

-- | A portable Fish/Bash binding identifier, never shell source text.
identifier :: Text -> Either Text Identifier
identifier value = case T.uncons value of
  Just (leading, rest)
    | letter leading && T.all (\character -> letter character || isDigit character) rest -> Right (Identifier value)
  _ -> Left "A variable identifier must start with an ASCII letter or underscore and contain only ASCII letters, digits and underscores"
  where
    letter character = isAsciiLower character || isAsciiUpper character || character == '_'

identifierText :: Identifier -> Text
identifierText (Identifier value) = value

-- Invalid literals cannot produce an identifier or inject shell source.
-- Applications accepting dynamic names should use 'identifier' explicitly.
instance IsString Identifier where
  fromString value = either error id (identifier (toText value))

instance Semigroup Identifier where
  Identifier left <> Identifier right = Identifier (left <> right)

module Language.Fish.Translator.Names
  ( isValidVarName
  ) where

import Data.Char (isAlpha, isAlphaNum)
import qualified Data.Text as T

isValidVarName :: Text -> Bool
isValidVarName name =
  case T.uncons name of
    Just (c, rest) | isAlpha c || c == '_' -> T.all isIdentChar rest
    _ -> False
  where
    isIdentChar ch = isAlphaNum ch || ch == '_'

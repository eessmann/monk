{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Builtins.Common
  ( parseAssignmentLiteral,
    wrapStmtList,
  )
where

import Data.Text qualified as T
import Language.Fish.AST
import Language.Fish.Translator.Names (isValidVarName)

parseAssignmentLiteral :: Text -> Maybe (Text, Text)
parseAssignmentLiteral txt = do
  let trimmed = T.strip txt
  guard (not (T.null trimmed))
  let (lhs, rest) = T.breakOn "=" trimmed
  guard (T.isPrefixOf "=" rest)
  guard (isValidVarName lhs)
  pure (lhs, T.drop 1 rest)

wrapStmtList :: [FishStatement] -> FishStatement
wrapStmtList [stmt] = stmt
wrapStmtList stmts = StmtList stmts

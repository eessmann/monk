{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Builtins.Unset
  ( translateUnsetCommand,
  )
where

import Data.Text qualified as T
import Language.Fish.AST
import Language.Fish.Translator.Builtins.Common (wrapStmtList)
import Language.Fish.Translator.Monad (TranslateM, addWarning)
import Language.Fish.Translator.Variables (tokenToLiteralText)
import ShellCheck.AST

data UnsetMode
  = UnsetVar
  | UnsetFunc
  deriving stock (Show, Eq)

translateUnsetCommand :: [Token] -> TranslateM FishStatement
translateUnsetCommand args = do
  stmts <- go UnsetVar [] args
  pure $
    case stmts of
      [] -> Comment "Skipped unset with no arguments"
      _ -> wrapStmtList stmts
  where
    go _ acc [] = pure (reverse acc)
    go mode acc (tok : rest) =
      let txt = tokenToLiteralText tok
       in if T.isPrefixOf "-" txt
            then case txt of
              "-f" -> go UnsetFunc acc rest
              "-v" -> go UnsetVar acc rest
              "--" -> go mode acc rest
              _ -> do
                addWarning ("Unsupported unset flag: " <> txt)
                go mode acc rest
            else
              if T.null txt
                then do
                  addWarning "Unsupported unset argument: empty name"
                  go mode acc rest
                else do
                  let stmt = case mode of
                        UnsetFunc -> Stmt (Command "functions" [ExprVal (ExprLiteral "-e"), ExprVal (ExprLiteral txt)])
                        UnsetVar -> Stmt (Command "set" [ExprVal (ExprLiteral "-e"), ExprVal (ExprLiteral txt)])
                  go mode (stmt : acc) rest
